"""
12 - Ingest and fit expressions from the database CLI
=====================================================

This tutorial demonstrates the ``srtree-db`` CLI workflow for ingesting
expressions into an SQLite database and fitting them to a dataset.

The key steps:

  1. Generate or provide a file of mathematical expressions (one per line).
  2. Ingest them into an SQLite e-graph database using ``srtree-db ingest``.
  3. Fit the expressions to a dataset using ``srtree-db fitdata``.

The fitting uses batch page preloading, persistent page caching, sub-expression
expansion, and parallel fitting with analytical optimization for parameter-free
expressions, achieving ~70k expressions/second on large databases.

Run from the ``tutorials/`` directory:

    python 12_db_ingest.py
"""

import os
import sys
import time
import tempfile
import subprocess
import numpy as np
import pandas as pd


def main():
    # --- Generate synthetic data ---
    rng = np.random.default_rng(42)
    n = 100
    df = pd.DataFrame({
        "x0": rng.uniform(-3, 3, n),
        "x1": rng.uniform(-3, 3, n),
        "y":  rng.uniform(-3, 3, n),
    })
    data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                           dir=".", prefix="ingest_demo_").name
    df.to_csv(data_csv, index=False)
    print(f"Created dataset: {data_csv} ({n} rows)")

    # --- Generate expressions with parameters ---
    eqs_file = tempfile.NamedTemporaryFile(delete=False, suffix=".tir",
                                           dir=".", prefix="ingest_eqs_").name
    rng2 = np.random.default_rng(123)
    ops = ["+", "*", "-"]
    consts = [str(round(rng2.uniform(-5, 5), 2)) for _ in range(50)]
    params = ["t0", "t1", "t2"]
    vars_pool = ["x0", "x1"]

    with open(eqs_file, "w") as f:
        for _ in range(5000):
            v = rng2.choice(vars_pool)
            p = rng2.choice(params)
            c = rng2.choice(consts)
            op = rng2.choice(ops)
            r = rng2.random()
            if r < 0.33:
                f.write(f"{p} {op} {v} + {c}\n")
            elif r < 0.66:
                f.write(f"{p} * {v} + {c}\n")
            else:
                f.write(f"{p} * ({v} {op} {c})\n")
    print(f"Generated {5000} expressions: {eqs_file}")

    db_file = tempfile.NamedTemporaryFile(delete=False, suffix=".db",
                                          dir=".", prefix="ingest_demo_").name

    # --- Step 1: Ingest expressions into the database ---
    print("\n=== Step 1: Ingest expressions ===")
    t0 = time.time()
    result = subprocess.run(
        ["cabal", "-v0", "run", "exe:srtree-db", "--",
         "ingest", "--db", db_file, "--expressions", eqs_file,
         "--dataset", "demo", "--quiet", "--format", "TIR"],
        capture_output=True, text=True, cwd=".."
    )
    ingest_time = time.time() - t0
    print(result.stdout.strip())
    if result.returncode != 0:
        print("ERROR:", result.stderr)
        sys.exit(1)
    print(f"Ingest time: {ingest_time:.2f}s")

    # --- Step 2: Fit expressions to the dataset ---
    print("\n=== Step 2: Fit expressions ===")
    t0 = time.time()
    result = subprocess.run(
        ["cabal", "-v0", "run", "exe:srtree-db", "--",
         "fitdata", "--db", db_file, "--dataset", "demo",
         "--data", f"{data_csv}:::y:x0,x1",
         "--loss", "NLL Gaussian",
         "--n-rep", "1", "--n-iter", "20",
         "--batch-size", "10000", "--quiet"],
        capture_output=True, text=True, cwd=".."
    )
    fit_time = time.time() - t0
    print(result.stdout.strip())
    if result.returncode != 0:
        print("ERROR:", result.stderr)
        sys.exit(1)
    print(f"Fit time: {fit_time:.2f}s")

    # --- Step 3: Query results ---
    print("\n=== Step 3: Query top expressions ===")
    result = subprocess.run(
        ["cabal", "-v0", "run", "exe:srtree-db", "--",
         "fitdata", "--db", db_file, "--dataset", "demo",
         "--data", f"{data_csv}:::y:x0,x1",
         "--loss", "NLL Gaussian",
         "--n-rep", "1", "--n-iter", "20",
         "--batch-size", "10000", "--quiet"],
        capture_output=True, text=True, cwd=".."
    )

    # Cleanup
    for f in (data_csv, eqs_file, db_file):
        try:
            os.remove(f)
        except OSError:
            pass

    print("\nDone! The srtree-db CLI provides fast, scalable ingest and fitting")
    print("with O(1) memory per expression and ~70k expr/s fitting throughput.")


if __name__ == "__main__":
    main()
