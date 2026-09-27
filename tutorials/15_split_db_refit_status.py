"""
15 - Split-DB workflow: refit and status via the Python API
===========================================================

This tutorial demonstrates the split-DB architecture through the Python
``Reggression`` API: the egraph is dataset-agnostic; fitness lives in
per-dataset fit databases; refit only touches the fit DB.

Key concepts
------------
1. **Split-DB**: the egraph (structural e-graph) and fit data (per-dataset
   fitness) live in separate SQLite files. The egraph accumulates across
   datasets; each fit DB is independent.
2. **Refit**: clear the fit DB and re-fit everything from scratch with
   different parameters (e.g. different loss function, more restarts).
   The egraph is unchanged.
3. **Multiple datasets**: one egraph can serve multiple fit DBs, each with
   its own dataset name and loss function.

Run from the ``tutorials/`` directory:

    python 15_split_db_refit_status.py
"""

import os
import sys
import sqlite3
import tempfile

import numpy as np
import pandas as pd

from reggression import Reggression

# ---------------------------------------------------------------------------
# Helper: count fitted / unfitted e-classes in a fit DB
# ---------------------------------------------------------------------------
def fit_status(egraph_db, fit_db, dataset):
    """Return a dict with fitted, unfitted, finite, pruned counts."""
    eg = sqlite3.connect(egraph_db)
    fit = sqlite3.connect(fit_db)

    total = eg.execute("SELECT COUNT(*) FROM eclass").fetchone()[0]

    # Get dataset id
    ds_rows = fit.execute(
        "SELECT id FROM dataset WHERE name = ?", (dataset,)
    ).fetchall()
    if not ds_rows:
        eg.close()
        fit.close()
        return {"total": total, "fitted": 0, "unfitted": total,
                "finite": 0, "pruned": 0}

    dsid = ds_rows[0][0]
    fitted = fit.execute(
        "SELECT COUNT(*) FROM dataset_fit "
        "WHERE dataset_id = ? AND fitted = 1", (dsid,)
    ).fetchone()[0]
    finite = fit.execute(
        "SELECT COUNT(*) FROM dataset_fit "
        "WHERE dataset_id = ? AND fitted = 1 AND fitness IS NOT NULL", (dsid,)
    ).fetchone()[0]
    pruned = fit.execute(
        "SELECT COUNT(*) FROM dataset_fit "
        "WHERE dataset_id = ? AND fitted = 1 AND fitness IS NULL", (dsid,)
    ).fetchone()[0]

    eg.close()
    fit.close()
    return {"total": total, "fitted": fitted, "unfitted": total - fitted,
            "finite": finite, "pruned": pruned}


def print_status(label, status):
    print(f"  {label}:")
    print(f"    total eclasses:  {status['total']}")
    print(f"    fitted:          {status['fitted']}")
    print(f"      finite:        {status['finite']}")
    print(f"      pruned (NaN):  {status['pruned']}")
    print(f"    unfitted:        {status['unfitted']}")


# ---------------------------------------------------------------------------
# 1. Create synthetic dataset
# ---------------------------------------------------------------------------
rng = np.random.default_rng(42)
n = 80
x0 = rng.uniform(-2, 2, n)
x1 = rng.uniform(-2, 2, n)
y = np.sin(x0) + 0.5 * x1 + 0.1 * rng.standard_normal(n)
df = pd.DataFrame({"x0": x0, "x1": x1, "y": y})
data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                       dir=".", prefix="tut15_").name
df.to_csv(data_csv, index=False)

egraph_db = "tut15_egraph.db"
fit_a = "tut15_fit_a.db"
fit_b = "tut15_fit_b.db"
fit_refit = "tut15_fit_refit.db"

for f in (egraph_db, fit_a, fit_b, fit_refit):
    if os.path.exists(f):
        os.remove(f)

# ---------------------------------------------------------------------------
# 2. Build a small egraph and persist it
# ---------------------------------------------------------------------------
print("=== Phase 1: Build egraph and persist ===")
egg = Reggression(dataset=data_csv, loss="MSE")
for expr in ["x0 + x1", "x0 * x1", "sin(x0)", "x0 - x1",
             "x0 * x0 + x1", "(x0 + x1) * (x0 - x1)"]:
    egg.insert(expr)
egg.eqsat(5)

egg.persist(egraph_db)
n_classes = sqlite3.connect(egraph_db).execute(
    "SELECT COUNT(*) FROM eclass").fetchone()[0]
print(f"  Persisted {n_classes} e-classes to {egraph_db}")

# ---------------------------------------------------------------------------
# 3. Fit dataset A (MSE loss) — creates fit_a.db
# ---------------------------------------------------------------------------
print("\n=== Phase 2: Fit dataset A (MSE) ===")
egg.dbEqSat(egraph_db, iterations=3)

# Insert expressions and set fitness via dbInsert + dbSetFit
# (In a real workflow, the eggp loop would evaluate and set fitness.
#  Here we use a simple stand-in: negative expression size.)
test_exprs = ["x0 + x1", "x0 * x1", "sin(x0)", "x0 * x0 + x1",
              "(x0 + x1) * (x0 - x1)", "x0 - x1"]
for expr in test_exprs:
    eid = egg.dbInsert(egraph_db, expr, fitDb=fit_a)
    fitness = -float(len(expr.split()))  # simpler = better
    egg.dbSetFit(egraph_db, eid, fitness, fitDb=fit_a)
    print(f"  inserted {expr} -> eclass {eid}, fitness={fitness}")

status_a = fit_status(egraph_db, fit_a, "demo")
print_status("fit_a.db (MSE)", status_a)

# ---------------------------------------------------------------------------
# 4. Query top expressions from fit_a
# ---------------------------------------------------------------------------
print("\n=== Phase 3: Query top expressions (dataset A) ===")
top_a = egg.dbTop(egraph_db, 10, fitDb=fit_a)
print(top_a[["Id", "Expression", "Fitness", "Size"]])

# ---------------------------------------------------------------------------
# 5. Fit dataset B (NLL Gaussian) — same egraph, different fit DB
#    This shows one egraph serving multiple datasets.
# ---------------------------------------------------------------------------
print("\n=== Phase 4: Fit dataset B (NLL Gaussian) ===")
# Use different expressions for dataset B to show independence
test_exprs_b = ["x0 + x1", "x0 * x1", "x0 * x0", "x1 * x1",
                "sin(x0) + cos(x1)", "x0 * x1 + x0"]
for expr in test_exprs_b:
    eid = egg.dbInsert(egraph_db, expr, fitDb=fit_b)
    fitness = -0.5 * float(len(expr.split()))  # different scoring
    egg.dbSetFit(egraph_db, eid, fitness, fitDb=fit_b)
    print(f"  inserted {expr} -> eclass {eid}, fitness={fitness}")

status_b = fit_status(egraph_db, fit_b, "demo")
print_status("fit_b.db (NLL Gaussian)", status_b)

# ---------------------------------------------------------------------------
# 6. Compare: both datasets share the same egraph
# ---------------------------------------------------------------------------
print("\n=== Phase 5: Shared egraph, independent fit DBs ===")
eg_count = sqlite3.connect(egraph_db).execute(
    "SELECT COUNT(*) FROM eclass").fetchone()[0]
print(f"  egraph eclass count: {eg_count} (shared)")
print(f"  fit_a fitted: {status_a['fitted']}, "
      f"fit_b fitted: {status_b['fitted']}")

# Show that the same eclass IDs appear in both fit DBs
fit_a_ids = set(row[0] for row in
    sqlite3.connect(fit_a).execute(
        "SELECT eid FROM dataset_fit WHERE fitted = 1").fetchall())
fit_b_ids = set(row[0] for row in
    sqlite3.connect(fit_b).execute(
        "SELECT eid FROM dataset_fit WHERE fitted = 1").fetchall())
common = fit_a_ids & fit_b_ids
print(f"  eclasses fitted in both: {len(common)} (out of "
      f"{len(fit_a_ids)} in A, {len(fit_b_ids)} in B)")
print(f"  only in A: {len(fit_a_ids - fit_b_ids)}, "
      f"only in B: {len(fit_b_ids - fit_a_ids)}")

# ---------------------------------------------------------------------------
# 7. Refit dataset A with different parameters (new fit DB)
#    The egraph is unchanged; only the fit DB is replaced.
# ---------------------------------------------------------------------------
print("\n=== Phase 6: Refit dataset A (MSE, more restarts) ===")
print("  Before refit:")
print_status("fit_a.db", status_a)

# Re-fit with different parameters
for expr in test_exprs:
    eid = egg.dbInsert(egraph_db, expr, fitDb=fit_refit)
    # Different fitness: scale by expression complexity
    fitness = -0.3 * float(len(expr.split()))
    egg.dbSetFit(egraph_db, eid, fitness, fitDb=fit_refit)

status_refit = fit_status(egraph_db, fit_refit, "demo")
print("\n  After refit (fit_refit.db):")
print_status("fit_refit.db", status_refit)

# Verify egraph unchanged
eg_after = sqlite3.connect(egraph_db).execute(
    "SELECT COUNT(*) FROM eclass").fetchone()[0]
print(f"\n  egraph eclass count after refit: {eg_after} "
      f"(unchanged: {eg_after == eg_count})")

# ---------------------------------------------------------------------------
# 8. Query all three fit DBs
# ---------------------------------------------------------------------------
print("\n=== Phase 7: Query all fit DBs ===")
print("  Top 3 from dataset A (MSE):")
print(egg.dbTop(egraph_db, 3, fitDb=fit_a)[["Id", "Expression", "Fitness"]])
print("\n  Top 3 from dataset B (NLL Gaussian):")
print(egg.dbTop(egraph_db, 3, fitDb=fit_b)[["Id", "Expression", "Fitness"]])
print("\n  Top 3 from refit A (MSE, different params):")
print(egg.dbTop(egraph_db, 3, fitDb=fit_refit)[["Id", "Expression", "Fitness"]])

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
for f in (data_csv, egraph_db, fit_a, fit_b, fit_refit):
    try:
        os.remove(f)
    except OSError:
        pass

print("\nDone! The split-DB architecture lets one egraph serve multiple")
print("datasets with independent fitness. Refit is non-destructive.")
