"""
16 - Pattern wildcard mapping and e-class terminals
====================================================

This tutorial demonstrates two new introspection tools:

* ``patternMap(pattern)`` — shows what each wildcard variable (v0, v1, …)
  actually matched in every occurrence of a pattern, along with the e-class
  ID of each match.
* ``eclassTerminals(eid)`` — lists all unique terminals (variables x*,
  parameters t*, constants) that appear inside a given e-class.

Together these let you trace wildcards back to concrete expressions and
inspect the building blocks of any e-class.

Run from the ``tutorials/`` directory:

    python 16_pattern_map_and_terminals.py
"""

import numpy as np
import pandas as pd
import tempfile
import os

from reggression import Reggression

# ---------------------------------------------------------------------------
# 1. Create a synthetic dataset.
# ---------------------------------------------------------------------------
rng = np.random.default_rng(42)
x0 = rng.uniform(0, 1, 80)
x1 = rng.uniform(0, 2, 80)
y = 2.5 * x0 + 0.3 * x1 + 0.1 * rng.standard_normal(80)

df = pd.DataFrame({"x0": x0, "x1": x1, "y": y})
data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                       dir=".", prefix="tut16_").name
df.to_csv(data_csv, index=False)

pd.set_option("display.max_colwidth", 200)
pd.set_option("display.width", 200)

egg = Reggression(dataset=data_csv, loss="MSE")

# ---------------------------------------------------------------------------
# 2. Seed expressions and run equality saturation.
# ---------------------------------------------------------------------------
seeds = [
    "x0 + x1",
    "x0 * x1",
    "x0 ** 2",
    "sin(x0)",
    "exp(x0)",
    "t0 * x0 + t1",
    "sin(x0 + x1)",
    "exp(x0) * x1",
    "(x0 + x1) ** 2",
    "log(1 + x0)",
]
print("Inserting seed expressions:")
for s in seeds:
    egg.insert(s)

print("Running 5 iterations of equality saturation...")
egg.eqsat(5)

# ---------------------------------------------------------------------------
# 3. patternMap — see what wildcards matched.
# ---------------------------------------------------------------------------
print("\n" + "=" * 60)
print("patternMap — inspecting wildcard bindings")
print("=" * 60)

# Simple pattern with two wildcards
print("\n--- patternMap('v0 + v1') ---")
pm = egg.patternMap("v0 + v1")
print(pm)

# Pattern with a function and wildcards
print("\n--- patternMap('v0 * v1') ---")
pm2 = egg.patternMap("v0 * v1")
print(pm2)

# Pattern with three wildcards
print("\n--- patternMap('v0 + v1 * v2') ---")
pm3 = egg.patternMap("v0 + v1 * v2")
print(pm3)

# Use the limit parameter
print("\n--- patternMap('v0 + v1', limit=2) ---")
pm4 = egg.patternMap("v0 + v1", limit=2)
print(pm4)

# Pattern with no wildcards — just find exact matches
print("\n--- patternMap('x0 + x1') (no wildcards) ---")
pm5 = egg.patternMap("x0 + x1")
print(pm5)

# ---------------------------------------------------------------------------
# 4. eclassTerminals — list terminals inside an e-class.
# ---------------------------------------------------------------------------
print("\n" + "=" * 60)
print("eclassTerminals — listing terminals in e-classes")
print("=" * 60)

# First, get some e-class IDs from the top expressions
top = egg.top(6)
print("\nTop expressions:")
print(top[["Id", "Expression", "Fitness"]])

print()
for _, row in top.iterrows():
    eid = int(row["Id"])
    expr = row["Expression"]
    terms = egg.eclassTerminals(eid)
    print(f"  eclass {eid:3d} ({expr:30s}) terminals: {list(terms.Name.values)}")

# ---------------------------------------------------------------------------
# 5. Combining both tools — trace wildcards to their terminals.
# ---------------------------------------------------------------------------
print("\n" + "=" * 60)
print("Combining patternMap + eclassTerminals")
print("=" * 60)

pm = egg.patternMap("v0 + v1")
for _, row in pm.iterrows():
    v0_eid = int(row["v0_eid"])
    v1_eid = int(row["v1_eid"])
    v0_terms = egg.eclassTerminals(v0_eid)
    v1_terms = egg.eclassTerminals(v1_eid)
    print(f"\n  Match: {row['Expression']}")
    print(f"    v0 = {row['v0']:20s} (eid {v0_eid:3d})  terminals: {list(v0_terms.Name.values)}")
    print(f"    v1 = {row['v1']:20s} (eid {v1_eid:3d})  terminals: {list(v1_terms.Name.values)}")

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
os.remove(data_csv)
print("\nDone.")
