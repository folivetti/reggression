"""
14 - Loading and querying a pre-existing SQLite database
========================================================

This tutorial shows how to load an e-graph that was previously persisted to a
SQLite database (via ``persist``, ``importDB``, or the ``srtree-db`` CLI) and
query it without re-running the entire pipeline.

Key points
----------
1. A ``Reggression`` object always needs a dataset CSV to initialise (it
   derives column names and computes description length). But once initialised,
   ``loadDB`` replaces the in-memory e-graph with the one stored in the SQLite
   file.
2. After ``loadDB``, the full query API works: ``top``, ``pareto``,
   ``countPattern``, ``distributionOfTokens``, etc.
3. You can also **resume** work: insert new expressions, run eqsat, and
   persist back to the same (or a different) database file.

Run from the ``tutorials/`` directory:

    python 14_load_existing_db.py
"""

import os
import sys
import sqlite3
import tempfile

import numpy as np
import pandas as pd

from reggression import Reggression

# ---------------------------------------------------------------------------
# 1. Create a small e-graph and persist it to a database.
#    (This step is just so the tutorial is self-contained; in practice the DB
#    file would already exist from a previous session or from ``srtree-db``.)
# ---------------------------------------------------------------------------
rng = np.random.default_rng(7)
n = 80
df = pd.DataFrame({
    "x0": rng.uniform(-2, 2, n),
    "x1": rng.uniform(-2, 2, n),
    "y":  np.sin(rng.uniform(-2, 2, n)) + 0.5 * rng.uniform(-2, 2, n),
})
data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                       dir=".", prefix="tut14_").name
df.to_csv(data_csv, index=False)

db_file = "tut14_preexisting.db"
for f in (db_file,):
    if os.path.exists(f):
        os.remove(f)

print("=== Phase 1: build and persist an e-graph ===")
egg = Reggression(dataset=data_csv, loss="MSE")

for expr in ["x0 + x1", "x0 * x1", "sin(x0)", "x0 - x1",
             "(x0 + x1) * (x0 - x1)", "1.0 + x0"]:
    egg.insert(expr)
egg.eqsat(5)

egg.persist(db_file)
n_classes = sqlite3.connect(db_file).execute(
    "SELECT COUNT(*) FROM eclass").fetchone()[0]
print(f"Persisted {n_classes} e-classes to {db_file}\n")

# ---------------------------------------------------------------------------
# 2. Load the pre-existing database with a fresh Reggression object.
#    The constructor still needs the dataset CSV (for column names / DL), but
#    loadDB replaces the in-memory graph with the persisted one.
# ---------------------------------------------------------------------------
print("=== Phase 2: load the pre-existing database ===")
egg2 = Reggression(dataset=data_csv, loss="MSE")
egg2.loadDB(db_file)

print("Top 5 expressions (loaded from DB):")
print(egg2.top(5)[["Id", "Expression", "Fitness", "Size"]])

print("\nPareto front:")
print(egg2.pareto()[["Id", "Expression", "Fitness", "Size"]])

# ---------------------------------------------------------------------------
# 3. Query patterns and distributions — all read from the loaded graph.
# ---------------------------------------------------------------------------
print("\nPattern counts:")
for pat in ["EAdd", "EMul", "ESin"]:
    print(f"  {pat}: {egg2.countPattern(pat)}")

print("\nToken distribution (top 5):")
dist = egg2.distributionOfTokens()
print(dist.head())

# ---------------------------------------------------------------------------
# 4. Resume work: insert new expressions, re-saturate, and persist back.
# ---------------------------------------------------------------------------
print("\n=== Phase 3: resume — insert more and persist again ===")
new_ids = []
for expr in ["x0 ** 2", "cos(x1)", "x0 / (1.0 + x1)"]:
    eid = egg.insert(expr)
    new_ids.append((int(eid), expr))
    print(f"  inserted {expr} -> eclass {eid}")

egg.eqsat(3)

# Persist to the same file (overwrites) or a new file.
egg.persist(db_file)
n_classes_after = sqlite3.connect(db_file).execute(
    "SELECT COUNT(*) FROM eclass").fetchone()[0]
print(f"\nAfter resuming and inserting 3 more: {n_classes_after} e-classes "
      f"(was {n_classes})")

# ---------------------------------------------------------------------------
# 5. Demonstrate the DB query methods (work on any persisted DB file).
# ---------------------------------------------------------------------------
print("\n=== Phase 4: direct DB queries ===")
print(f"Total e-classes: {egg2.dbCount(db_file, 'EAdd')}")
print("\nTop 3 by fitness (read from SQLite):")
print(egg2.dbTop(db_file, 3)[["Id", "Expression", "Fitness", "Size"]])

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
for f in (data_csv, db_file):
    try:
        os.remove(f)
    except OSError:
        pass

print("\nDone!")
