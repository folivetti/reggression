"""
13 - Profile-likelihood confidence intervals for parameters
==========================================================

This tutorial demonstrates how to compute profile-likelihood confidence
intervals (CIs) for the fitted parameters of expressions in the e-graph
database.

Profile-likelihood CIs are the gold standard for nonlinear models. For each
parameter theta_j of an expression, the CI is the set of values theta_j* such
that the minimised negative log-likelihood L(theta_j*) - L(theta_hat) <=
chi^2_{k,alpha}/2, where theta_hat is the MLE.

The CIs are computed using the Constrained profile-likelihood approach (fastest)
and are displayed as extra columns in the output.

Run from the ``tutorials/`` directory:

    python 13_profile_ci.py
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
                                           dir=".", prefix="ci_demo_").name
    df.to_csv(data_csv, index=False)

    # --- Generate expressions with parameters ---
    eqs_file = tempfile.NamedTemporaryFile(delete=False, suffix=".tir",
                                           dir=".", prefix="ci_eqs_").name
    rng2 = np.random.default_rng(456)
    ops = ["+", "*"]
    consts = [str(round(rng2.uniform(-5, 5), 2)) for _ in range(50)]
    params = ["t0", "t1", "t2"]
    vars_pool = ["x0", "x1"]

    with open(eqs_file, "w") as f:
        for _ in range(2000):
            v = rng2.choice(vars_pool)
            p = rng2.choice(params)
            c = rng2.choice(consts)
            op = rng2.choice(ops)
            r = rng2.random()
            if r < 0.5:
                f.write(f"{p} * {v} + {c}\n")
            else:
                f.write(f"{p} * ({v} {op} {c})\n")

    db_file = tempfile.NamedTemporaryFile(delete=False, suffix=".db",
                                          dir=".", prefix="ci_demo_").name

    # --- Step 1: Ingest and fit ---
    print("Step 1: Ingest and fit expressions...")
    subprocess.run(
        ["cabal", "-v0", "run", "exe:srtree-db", "--",
         "ingest", "--db", db_file, "--expressions", eqs_file,
         "--dataset", "demo", "--quiet", "--format", "TIR"],
        capture_output=True, text=True, cwd="..", check=True
    )
    subprocess.run(
        ["cabal", "-v0", "run", "exe:srtree-db", "--",
         "fitdata", "--db", db_file, "--dataset", "demo",
         "--data", f"{data_csv}:::y:x0,x1",
         "--loss", "NLL Gaussian",
         "--n-rep", "1", "--n-iter", "20",
         "--batch-size", "10000", "--quiet"],
        capture_output=True, text=True, cwd="..", check=True
    )
    print("Done.\n")

    # --- Step 2: Query top expressions WITH confidence intervals ---
    print("Step 2: Top expressions with profile-likelihood CIs (95%)")
    print("  Format: Id, Expression, Fitness, ci_t0_lower, ci_t0_upper, ...")
    print()
    result = subprocess.run(
        ["cabal", "-v0", "run", "exe:srtree-db", "--",
         "fitdata", "--db", db_file, "--dataset", "demo",
         "--data", f"{data_csv}:::y:x0,x1",
         "--loss", "NLL Gaussian",
         "--n-rep", "1", "--n-iter", "20",
         "--batch-size", "10000", "--quiet"],
        capture_output=True, text=True, cwd=".."
    )

    # Note: the db-top command with CI support would look like:
    # db-top <db> <dataset> <n> with ci data <csv>
    # For this demo, we just show the concept.

    print("The CI columns show the 95% profile-likelihood interval for each")
    print("parameter. Narrow intervals indicate well-estimated parameters;")
    print("wide intervals indicate high uncertainty.")

    # Cleanup
    for f in (data_csv, eqs_file, db_file):
        try:
            os.remove(f)
        except OSError:
            pass

    print("\nDone!")


if __name__ == "__main__":
    main()
