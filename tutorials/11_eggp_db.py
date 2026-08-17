"""
11 - eggp in DB mode: out-of-core search, and resuming from a persisted database
=================================================================================

This tutorial shows the *DB-backed* eggp loop: instead of (or alongside) the fully
in-memory ``Reggression`` object, you can drive an evolutionary search against a
persisted SQLite e-graph, out of core and in ``O(1)`` memory.

The DB-backed loop is the symmetric counterpart of the in-memory one:

    in-memory                        DB-backed
    --------------                   -------------------------------
    eid = egg.insert(expr)           eid = egg.dbInsert(db, expr)
    egg.eqsat(n)                     egg.dbEqSat(db, n)          # full re-saturation
    -                                egg.dbEqSatFrontier(db, n)  # re-saturate only what changed
    (fitness from insert)            egg.dbSetFit(db, eid, fit)  # record the loop's fitness
    egg.top(n)                       egg.dbTop(db, n)

Notes
-----
* The DB loop never builds an in-memory e-graph for the candidates -- each
  ``dbInsert`` lazily loads the paged graph, writes the content-addressed
  subgraph (marking genuinely-new classes dirty), and closes. This is what makes
  the DB-backed search out of core and bounded-memory.
* Fitness is the loop's job: the loop must evaluate each candidate against its
  training data. Here we use a tiny deterministic stand-in (expression size)
  so the tutorial focuses on the DB mechanics; a real eggp loop would fit the
  model and store its real score.

Key ideas shown here
--------------------
1. **Create a DB-backed e-graph** with ``importDB`` (streams expressions straight
   into SQLite, no in-memory graph is built).
2. **Drive the loop in DB mode**: for each generated candidate, ``dbInsert`` the
   expression (content-addressed; existing subexpressions dedup), evaluate it and
   record the fitness with ``dbSetFit``, and run ``dbEqSat`` once to saturate.
3. **Frontier re-saturation**: after more ``dbInsert`` calls, ``dbEqSatFrontier``
   re-saturates only the recently-inserted classes (avoiding redo on the whole
   graph), then clears the frontier.
4. **Resume the search from a persisted database**: the DB file is the
   authoritative state. We "end" a session, open a fresh ``Reggression`` object
   pointed at the *same* DB file, and continue inserting / re-saturating /
   querying -- the accumulated classes and fitnesses are all still there.

Run from the ``tutorials/`` directory:

    python 11_eggp_db.py
"""

import os
import sys
import csv
import time
import sqlite3
import tempfile

import numpy as np
import pandas as pd

from reggression import Reggression

# ---------------------------------------------------------------------------
# Synthetic dataset + candidate-expression generator (srtree / .tir syntax).
# ---------------------------------------------------------------------------
def _synthetic_dataset(path):
    rng = np.random.default_rng(0)
    n = 60
    cols = {
        "x0": rng.uniform(0, 1, n),
        "x1": rng.uniform(0, 2, n),
        "x2": rng.uniform(0, 3, n),
        "x3": rng.uniform(0, 4, n),
    }
    cols["y"] = (np.sin(cols["x0"]) + 0.5 * cols["x1"]
                 + cols["x2"] * 0.1 + cols["x3"] * 0.05)
    pd.DataFrame(cols).to_csv(path, index=False)


def _random_expr(rng, depth):
    if depth <= 0:
        return rng.choice(["x0", "x1", "x2", "x3", "1.0", "2.0", "3.0"])
    if depth >= 2 and rng.random() < 0.15:
        f = rng.choice(["sin", "cos"])
        return f"{f}({_random_expr(rng, depth - 1)})"
    op = rng.choice(["+", "-", "*"])
    return f"({_random_expr(rng, depth - 1)} {op} {_random_expr(rng, depth - 1)})"


def _write_seed(path, rng, n_seed):
    # The third column is the expression's fitness. We store a deliberately-poor
    # placeholder (-10: a negative-MSE score, so lower = worse), because random
    # seed expressions rarely fit the target; the loop's evaluated candidates
    # (fitness close to 0 or positive) then rank above the seed.
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        for _ in range(n_seed):
            w.writerow([_random_expr(rng, rng.choice([1, 2, 3])), "", "-10.0"])


def fitness_of(expr):
    """A lightweight stand-in for the candidate's fitness.

    The DB layer itself does not fit models -- that is the eggp loop's job, using
    its own evaluation/optimization against the training data. Here we just need
    a deterministic, memory-cheap score to store via ``dbSetFit`` so the query
    layer can rank candidates. We use the negative expression size (fewer
    tokens -> simpler -> higher score), which keeps the same "higher is better"
    convention as the negative-MSE fitness the rest of the code uses. A real
    loop would replace this with the fitted performance of the model.
    """
    return -float(len(expr.split()))


def _n_classes(db):
    """Number of e-classes currently stored in the database."""
    con = sqlite3.connect(db)
    n = con.execute("SELECT COUNT(*) FROM eclass").fetchone()[0]
    con.close()
    return n


def _n_frontier(db):
    """Number of e-classes currently awaiting re-saturation."""
    con = sqlite3.connect(db)
    n = con.execute("SELECT COUNT(*) FROM frontier").fetchone()[0]
    con.close()
    return n


def run_search_session(egg, db, rng, n_candidates, every=10):
    """One 'session' of the DB-backed eggp loop: generate N candidates, insert
    each (content-addressed, marking the frontier), record its fitness, and
    re-saturate the frontier every ``every`` inserts (and once at the end).

    Returns the list of (eid, expr, fitness).

    Note on memory: this is the DB-backed loop -- no in-memory e-graph is ever
    built for the candidates (unlike the in-memory ``insert``). The only
    in-memory state is the lazily loaded paged graph during each short
    ``dbInsert``/``dbEqSatFrontier`` call, which stays bounded.
    """
    inserted = []
    for i in range(n_candidates):
        # shallow candidates: deep supersaturated expressions can make the
        # out-of-core extraction in dbTop very expensive, so keep the demo's
        # candidates small and robust.
        expr = _random_expr(rng, rng.choice([1, 2, 3]))
        # --- DB-backed insert: content-addressed subgraph, marks the frontier -
        eid = egg.dbInsert(db, expr)
        # --- record the fitness (from the loop's own evaluation) --------------
        egg.dbSetFit(db, eid, fitness_of(expr))
        inserted.append((int(eid), expr))
        if (i + 1) % every == 0:
            egg.dbEqSatFrontier(db, iterations=3, ruleset="default")
    egg.dbEqSatFrontier(db, iterations=3, ruleset="default")
    return inserted


def main():
    # --- dataset + seed equation file ---------------------------------------
    data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                            dir=".").name
    _synthetic_dataset(data_csv)
    db = "eggp_db_tutorial.db"
    if os.path.exists(db):
        os.remove(db)

    rng1 = np.random.default_rng(11)
    seed = "eggp_db_seed.tir"
    _write_seed(seed, rng1, 180)

    egg = Reggression(dataset=data_csv, loss="MSE")

    # =======================================================================
    # Session 1: seed the DB and run the loop in DB mode
    # =======================================================================
    print("\n[Session 1] seeding the database directly in SQLite (out-of-core) ...")
    print("  " + egg.importDB(seed, db))
    print(f"  classes after seed: {_n_classes(db)}")

    print("\n[Session 1] running the eggp loop in DB mode (insert -> setFit -> "
          "frontier eqsat) ...")
    t = time.time()
    results1 = run_search_session(egg, db, np.random.default_rng(12), 24, every=6)
    sess1_s = time.time() - t
    print(f"  inserted {len(results1)} candidates; frontier cleared: "
          f"{_n_frontier(db) == 0}")
    print("  first 3 inserted candidates (eid, expression):")
    print(pd.DataFrame(results1[:3], columns=["eid", "expr"]))
    print("  dbTop after session 1 (ranked by stored fitness; simpler = higher "
          "score ranks first):")
    print(egg.dbTop(db, 5))
    print(f"  classes after session 1: {_n_classes(db)} "
          f"({sess1_s:.1f}s for the session)")

    # =======================================================================
    # Session 2: RESUME from the persisted database (fresh object, same file)
    # =======================================================================
    # The DB file is the authoritative state. A *new* Reggression object pointed
    # at the same file "resumes" the search: the prior classes and fitnesses are
    # all still there. We do NOT re-import the seed -- we just keep going.
    print("\n[Session 2] RESUME -- opening a fresh Reggression object against the "
          "SAME database ...")
    egg2 = Reggression(dataset=data_csv, loss="MSE")
    print(f"  classes still present on resume: {_n_classes(db)}")

    print("\n[Session 2] continuing the search (insert more, re-saturate frontier) ...")
    t = time.time()
    results2 = run_search_session(egg2, db, np.random.default_rng(13), 18, every=6)
    sess2_s = time.time() - t
    print(f"  inserted {len(results2)} new candidates this resume; "
          f"frontier clear: {_n_frontier(db) == 0}")
    print("  the DB grew (classes and new candidates accumulated):")
    print(f"    classes now: {_n_classes(db)}")
    print(egg2.dbTop(db, 3))
    best2 = float(egg2.dbTop(db, 1)["Fitness"].iloc[0])
    print(f"  best fitness after resume: {best2:.6f} "
          f"({sess2_s:.1f}s for the resume session)")

    # --- summary -------------------------------------------------------------
    print("\n" + "=" * 64)
    print("DB-mode eggp summary")
    print(f"  total candidates inserted across sessions: "
          f"{len(results1) + len(results2)}")
    print(f"  best fitness: {best2:.6f}")
    print("  the database persists the whole search; a new object can always "
          "pick it back up and continue.")
    print("=" * 64)

    # cleanup
    for f in (data_csv, seed, db):
        try:
            os.remove(f)
        except OSError:
            pass

    sys.exit(0)


if __name__ == "__main__":
    main()
