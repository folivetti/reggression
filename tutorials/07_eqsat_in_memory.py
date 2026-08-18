"""
07 - In-memory equality saturation
===================================

This tutorial mirrors ``tutorials/egraph-inmemory.hs`` but from Python.

We build a tiny e-graph from a couple of expressions, run equality
saturation in memory, and then extract the best equivalent expressions via
``top`` and the accuracy/size Pareto front via ``pareto``.

Run from the ``tutorials/`` directory:

    python 07_eqsat_in_memory.py
"""

import numpy as np
import pandas as pd
import tempfile
import os

from reggression import Reggression

# ---------------------------------------------------------------------------
# 1. A synthetic dataset (self-contained so the tutorial needs no data file).
#    The column names define the variable names x0, x1 used below.
# ---------------------------------------------------------------------------
rng = np.random.default_rng(0)
x0 = rng.uniform(0, 1, 60)
x1 = rng.uniform(0, 2, 60)
y = np.sin(x0) + 0.5 * x1 + 0.01 * rng.standard_normal(60)

df = pd.DataFrame({"x0": x0, "x1": x1, "y": y})
data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                       dir=".", prefix="tut_").name
df.to_csv(data_csv, index=False)

pd.set_option("display.max_colwidth", 200)

egg = Reggression(dataset=data_csv, loss="MSE")

# ---------------------------------------------------------------------------
# 2. Insert a few seed expressions.
#    ``insert`` returns a DataFrame with the new e-class id.
# ---------------------------------------------------------------------------
seeds = [
    "x0 + x1",
    "x0 * x1",
    "log(exp(x0))",                 # equivalent to x0
    "(x0 + 1)**2 - (x0**2 + 2*x0 + 1)",  # equivalent to 1
    "x0 - x0",                      # equivalent to 0
]
print("Inserting seed expressions:")
for s in seeds:
    res = egg.insert(s)
    print(f"  {s:45s} -> eclass {int(res.Id.values[0])}")

# ---------------------------------------------------------------------------
# 3. Equality saturation (in memory). Each rule set is applied sequentially.
#    A handful of iterations is enough for these toy expressions.
# ---------------------------------------------------------------------------
print("\nRunning 5 iterations of in-memory equality saturation...")
egg.eqsat(5)

# ---------------------------------------------------------------------------
# 4. Extract the top expressions by fitness and the Pareto front.
# ---------------------------------------------------------------------------
print("\nTop 8 expressions by fitness:")
print(egg.top(8)[["Id", "Expression", "Fitness", "Size", "DL"]])

print("\nPareto front (max fitness x min size):")
print(egg.pareto()[["Id", "Fitness", "Size"]])

# ---------------------------------------------------------------------------
# 5. Equivalence check: two expressions land in the same e-class after eqsat.
# ---------------------------------------------------------------------------
eid_a = int(egg.insert("(x0 + 3)**2 - 9").Id.values[0])
eid_b = int(egg.insert("x0*(x0 + 6)").Id.values[0])
egg.eqsat(3)
print(f"\nEquivalence check: eclass ids before eqsat = {eid_a}, {eid_b}")
print("Alternative forms of the first expression:")
print("\n".join(sorted(egg.getNExpressions(eid_a, 200).Expression.values,
                       key=len)[:6]))

# cleanup the temporary dataset
os.remove(data_csv)
