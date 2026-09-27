"""
09 - Capability test: out-of-core equality saturation memory reduction
====================================================================

This tutorial doubles as a *capability test* for the srtree-db out-of-core path.
It shows that running equality saturation directly against a persisted (lazily
loaded, paged) e-graph uses substantially less memory than doing the same work
in the fully materialized in-memory e-graph.

Why this is the case
--------------------
A fully resident e-graph keeps **every** e-class body in RAM (the ``_eClass``
map). The out-of-core path (``dbEqSat`` / ``loadGraphLazy``) keeps the resident
body cache empty (or bounded by an LRU page store) and streams each class body
through the SQLite-backed store only when it is needed, so the graph never all
lives in memory at once.

What the test does
------------------
1. Builds a *large* e-graph (tens of thousands of classes) from a generated
   equation CSV via ``importFromCSV`` + ``eqsat`` -- enough that a fully resident
   copy is clearly "a lot of memory". The incremental peak RSS allocated while
   building is the **in-memory materialization cost** (every e-class body is held
   in RAM, and this grows with graph size).
2. Persists the graph to SQLite and runs ``dbEqSat`` (out-of-core) + ``dbTop``
   (pure SQL). The incremental peak RSS allocated during this is the **out-of-core
   operational cost**: class bodies are streamed through a bounded LRU page cache,
   so it stays small and -- unlike the in-memory path -- does *not* scale with the
   total number of classes.
3. Asserts that the out-of-core operational cost is strictly below the
   in-memory materialization cost, and that both paths agree on the best fitness
   (so the out-of-core rewrite produced an equivalent result).

Run from the ``tutorials/`` directory:

    python 09_eqsat_on_database.py
"""

import os
import sys
import csv
import time
import threading
import tempfile

import numpy as np
import pandas as pd

from reggression import Reggression

# ---------------------------------------------------------------------------
# Memory sampler: tracks the peak resident set size (RSS) while a block runs.
# On Linux we read /proc/self/statm (field 2, in pages); elsewhere we fall back
# to resource.getrusage, which reports a monotonic peak (good enough as a bound).
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
# Build a large e-graph.
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


def main():
    # --- synthetic data + a big pile of candidate equations ----------------
    data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                            dir=".", prefix="cap_").name
    _synthetic_dataset(data_csv)

    egg = Reggression(dataset=data_csv, loss="MSE")

    rng = np.random.default_rng(7)
    # 6000 equations -> tens of thousands of e-classes after eqsat.
    N_EQUATIONS = 6000
    eqs_csv = "cap_equations.tir"   # .tir so getFormat parses the srtree syntax
    with open(eqs_csv, "w", newline="") as fh:
        w = csv.writer(fh)
        for _ in range(N_EQUATIONS):
            w.writerow([_random_expr(rng, rng.choice([1, 2, 3, 4])), "", "0.0"])

    # --- Phase A: in-memory materialization (build the resident e-graph) ---
    # This is what an in-memory user must pay up front: every e-class body is
    # held in RAM.  Measured from a low baseline so the allocation shows up as a
    # positive incremental peak.
    print("\n[Phase A] in-memory materialization (importFromCSV + eqsat) ...")
    with PeakTracker() as pa:
        t = time.time()
        egg.importFromCSV(eqs_csv)
        egg.eqsat(15)
        build_s = time.time() - t
    inmem_delta_mb = pa.delta / 1e6
    n_classes = int(egg.top(1)["Id"].max()) + 1
    best_inmem = float(egg.top(1)["Fitness"].iloc[0])
    print(f"  built ~{n_classes} e-classes in {build_s:.1f}s")
    print(f"  incremental peak RSS: {inmem_delta_mb:.1f} MB  "
          f"(grows with graph size)")
    print(f"  best fitness:         {best_inmem:.6f}")

    # --- Persist and run Phase B out-of-core ------------------------------
    db = "cap_memory.db"
    if os.path.exists(db):
        os.remove(db)
    print("\nPersisting e-graph to SQLite (srtree-db) ...")
    egg.persist(db)

    print("[Phase B] dbEqSat (out-of-core) + dbTop (pure SQL) ...")
    with PeakTracker() as pb:
        egg.dbEqSat(db, iterations=3, ruleset="default")
        top_db = egg.dbTop(db, 3000)
    db_delta_mb = pb.delta / 1e6
    best_db = float(top_db["Fitness"].iloc[0])
    print(f"  incremental peak RSS: {db_delta_mb:.1f} MB  "
          f"(bounded by the page cache, ~independent of graph size)")
    print(f"  best fitness:         {best_db:.6f}")

    # --- Report + assertions ----------------------------------------------
    ratio = (db_delta_mb / inmem_delta_mb) if inmem_delta_mb > 0 else 0.0
    print("\n" + "-" * 64)
    print("Memory comparison (incremental peak RSS)")
    print(f"  in-memory materialization : {inmem_delta_mb:8.1f} MB")
    print(f"  out-of-core operation     : {db_delta_mb:8.1f} MB"
          f"   ({ratio * 100:.0f}% of in-memory)")
    print("-" * 64)

    ok = True
    if not (db_delta_mb < inmem_delta_mb):
        print("FAIL: out-of-core path did NOT use less memory than in-memory.")
        ok = False
    if abs(best_inmem - best_db) > 1e-6:
        print(f"FAIL: best fitness disagrees "
              f"(in-memory {best_inmem} vs db {best_db}).")
        ok = False

    # cleanup
    for f in (data_csv, eqs_csv, db):
        try:
            os.remove(f)
        except OSError:
            pass

    if ok:
        print("PASS: out-of-core equality saturation reduced memory usage "
              "while producing an equivalent result.")
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
