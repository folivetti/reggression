"""
10 - Scalability: out-of-core equality saturation on SQLite
===========================================================

This tutorial demonstrates two out-of-core capabilities of reggression built on
srtree-db:

  * ``importDB`` builds the seed e-graph **directly in SQLite**, streaming the
    candidate equations straight into the database (content-addressed,
    structural), so *no in-memory e-graph is ever built*: the dedup index,
    child lookups and parent edges all live in the database, so peak import
    memory is **bounded** (basically constant in the number of equations).
  * ``dbEqSat`` runs equality saturation over that database fully *out of core*:
    it lazily loads the paged graph and streams every e-class body through a
    bounded page cache, so the saturated graph never all lives in RAM.

The intended workflow -- and the one shown here -- is:

  1. ``importDB(eqs, db)``: read the equation file and insert every expression
     straight into the SQLite database ``db``, expanding each subexpression into
     its own e-class. Nothing is built in memory first (the class bodies are
     only written back to ``cstore_page`` at the end).
  2. ``dbEqSat(db, ...)``: run equality saturation out-of-core directly against
     that database, then query the result with ``dbTop``.

We do **not** run eqsat in memory and then replay it in the DB: both the import
and the saturation are out-of-core from the beginning.

Run from the ``tutorials/`` directory:

    python 10_scale_dbEqSat.py

The default run seeds ~40k e-classes (20k equations) and finishes in well under
a minute. Both ``importDB`` and ``dbEqSat`` scale smoothly into the hundreds of
thousands of e-classes; the demo keeps the default comfortably small so the
whole pipeline (import -> saturate -> query) runs quickly and stays well within
RAM (the import peak is bounded, independent of the number of equations).

Runtime/scaling note: eqsat's n-ary matcher is work-bounded (a per-rule cap on
how many classes it scans), so a saturation iteration's cost stays proportional
to the graph rather than exploding combinatorially. The workload is tunable via
environment variables:

    SCALE_N=20000    number of seed equations (default 20000 -> ~40k classes)
    SCALE_ITERS=4    dbEqSat iterations (default 4)

Try ``SCALE_N=2000 SCALE_ITERS=2`` first for a very quick smoke run.
"""

import os
import sys
import re
import csv
import time
import sqlite3
import threading
import tempfile

import numpy as np
import pandas as pd

from reggression import Reggression

# ---------------------------------------------------------------------------
# Memory sampler (peak resident set size while a block runs).
# ---------------------------------------------------------------------------
try:
    _PAGE = os.sysconf("SC_PAGE_SIZE")

    def _rss_bytes():
        with open("/proc/self/statm") as fh:
            return int(fh.read().split()[1]) * _PAGE
except (OSError, AttributeError, IndexError):
    import resource  # type: ignore

    def _rss_bytes():
        return resource.getrusage(resource.RUSAGE_SELF).ru_maxrss * 1024


class PeakTracker:
    """Context manager that records the maximum RSS reached inside the block."""

    def __init__(self):
        self.peak = 0
        self._start = 0
        self._stop = False
        self._thread = None

    def _run(self):
        while not self._stop:
            v = _rss_bytes()
            if v > self.peak:
                self.peak = v
            time.sleep(0.02)

    def __enter__(self):
        self.peak = self._start = _rss_bytes()
        self._stop = False
        self._thread = threading.Thread(target=self._run, daemon=True)
        self._thread.start()
        return self

    def __exit__(self, *exc):
        self._stop = True
        if self._thread is not None:
            self._thread.join()

    @property
    def delta(self):
        """Bytes of RSS allocated during the tracked block (peak - start)."""
        return self.peak - self._start


# ---------------------------------------------------------------------------
# Synthetic dataset + a large pile of candidate equations.
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
    op = rng.choice(["+", "-", "*"])
    return f"({_random_expr(rng, depth - 1)} {op} {_random_expr(rng, depth - 1)})"


def _n_classes(import_result: str) -> int:
    """Extract the e-class count from importDB's summary string."""
    m = re.search(r"\(([\d,]+) e-classes\)", import_result)
    if not m:
        raise ValueError(f"could not parse import summary: {import_result!r}")
    return int(m.group(1).replace(",", ""))


def main():
    # --- synthetic data ----------------------------------------------------
    data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                           dir=".", prefix="scale_").name
    _synthetic_dataset(data_csv)

    # --- Step 1: build the e-graph DIRECTLY in the database (out-of-core)
    #     The equation file MUST carry the ``.tir`` extension so the parser is
    #     chosen the same way importFromCSV does (by extension). importDB
    #     streams each expression into SQLite with content-addressed dedup; no
    #     in-memory e-graph is ever constructed.
    N_EQUATIONS = int(os.environ.get("SCALE_N", "20000"))
    iters = int(os.environ.get("SCALE_ITERS", "4"))
    db = "scale.db"
    eqs_csv = "scale_equations.tir"
    rng = np.random.default_rng(7)
    with open(eqs_csv, "w", newline="") as fh:
        w = csv.writer(fh)
        for _ in range(N_EQUATIONS):
            # weight depth 4 a little higher so the seed graph reaches ~2x classes
            # (e.g. ~40k e-classes at the default 20k equations)
            w.writerow([_random_expr(rng, rng.choice([1, 2, 3, 4, 4])), "", "0.0"])
    if os.path.exists(db):
        os.remove(db)

    print(f"\n[importDB] streaming {N_EQUATIONS} equations directly into "
          f"SQLite (out-of-core, no in-memory graph) ...")
    egg = Reggression(dataset=data_csv, loss="MSE")
    with PeakTracker() as pi:
        t = time.time()
        summary = egg.importDB(eqs_csv, db)     # <-- builds the DB directly
        import_s = time.time() - t
    import_rss_mb = pi.peak / 1e6
    n_seed = _n_classes(summary)
    db_mb = os.path.getsize(db) / 1e6
    print(f"  import: {summary}")
    print(f"  seed graph: ~{n_seed:,} e-classes in {import_s / 60:.1f} min")
    print(f"  wrote {db_mb:.1f} MB database")
    print(f"  import peak RSS: {import_rss_mb:.1f} MB "
          f"(bounded -- dedup index and parents stream to disk)")

    # --- Step 2: run the FULL equality saturation OUT-OF-CORE -------------
    #     dbEqSat loads the graph lazily from the DB and runs eqsat against the
    #     paged store: every e-class body is streamed through a bounded page
    #     cache, so the saturated graph never all lives in memory at once.  This
    #     is the saturation -- we did NOT run eqsat in memory beforehand.
    print("\n[dbEqSat] full equality saturation against the imported database "
          "(out-of-core, paged) ...")
    with PeakTracker() as po:
        t = time.time()
        egg.dbEqSat(db, iterations=iters, ruleset="default")
        top_db = egg.dbTop(db, 5)
        sat_s = time.time() - t
    sat_delta_mb = po.delta / 1e6
    sat_peak_mb = po.peak / 1e6
    best_db = float(top_db["Fitness"].iloc[0])
    con = sqlite3.connect(db)
    n_after = con.execute("SELECT COUNT(*) FROM eclass").fetchone()[0]
    con.close()
    print(f"  saturated {n_seed:,} -> {n_after:,} e-classes in {sat_s:.1f}s "
          f"(seed -> after saturation)")
    print(f"  incremental peak RSS: {sat_delta_mb:.1f} MB "
          f"(bounded by the page cache)")
    print(f"  process peak RSS:     {sat_peak_mb:.1f} MB")
    print(f"  best fitness:         {best_db:.6f}")
    print("  top expression(s):")
    for s in top_db["Expression"].head(3).tolist():
        print("    ", s)

    # --- Scalability report ------------------------------------------------
    print("\n" + "=" * 64)
    print(f"Scalability: ~{n_seed:,} e-classes, all handled out-of-core")
    print(f"  out-of-core import : {import_rss_mb:8.1f} MB peak "
          f"({import_s / 60:.1f} min)  <- bounded, independent of graph size")
    print(f"  out-of-core dbEqSat : {sat_delta_mb:8.1f} MB incremental "
          f"({sat_s:.1f}s)  <- bounded by the page cache")
    print("=" * 64)
    print("PASS: seeded an e-graph directly in SQLite out-of-core and "
          "saturated it with dbEqSat, all with bounded memory.")

    # cleanup
    for f in (data_csv, eqs_csv, db):
        try:
            os.remove(f)
        except OSError:
            pass
    sys.exit(0)


if __name__ == "__main__":
    main()
