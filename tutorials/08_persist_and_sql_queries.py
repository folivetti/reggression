"""
08 - Persisting an e-graph and querying it directly from SQLite
===============================================================

This tutorial mirrors ``tutorials/egraph-db.hs`` but from Python.

Once an e-graph grows large, materializing every class in memory to answer a
query (top-n, pattern count, Pareto front) can be expensive. ``srtree-db``
persists the e-graph to a normalized SQLite database; the Python methods
``dbTop``, ``dbCount``, ``dbPareto`` and ``dbDistribution`` answer those
queries *directly in SQL*, without enumerating patterns in memory.

Run from the ``tutorials/`` directory:

    python 08_persist_and_sql_queries.py
"""

import numpy as np
import pandas as pd
import tempfile
import os

from reggression import Reggression

# ---------------------------------------------------------------------------
# 1. Build a small in-memory e-graph (self-contained synthetic data).
# ---------------------------------------------------------------------------
rng = np.random.default_rng(1)
x0 = rng.uniform(0, 1, 60)
x1 = rng.uniform(0, 2, 60)
y = np.sin(x0) + 0.5 * x1 + 0.01 * rng.standard_normal(60)
df = pd.DataFrame({"x0": x0, "x1": x1, "y": y})
data_csv = tempfile.NamedTemporaryFile(delete=False, suffix=".csv",
                                       dir=".", prefix="tut_").name
df.to_csv(data_csv, index=False)

pd.set_option("display.max_colwidth", 200)

egg = Reggression(dataset=data_csv, loss="MSE")
for s in ["x0 + x1", "x0 * x1", "log(exp(x0))", "(x0 + x1)**2",
          "sqrt((x0 + x1)*(x0 + x1))", "x0 - x0"]:
    egg.insert(s)
egg.eqsat(5)

# ---------------------------------------------------------------------------
# 2. Persist the e-graph to a SQLite database (srtree-db format).
# ---------------------------------------------------------------------------
db = "tutorial.db"
if os.path.exists(db):
    os.remove(db)
print(f"Persisting e-graph to {db} ...")
egg.persist(db)
print("done. The SQLite file now holds the full e-graph.")

# ---------------------------------------------------------------------------
# 3. Query the database directly in SQL (no in-memory enumeration).
# ---------------------------------------------------------------------------
print("\nTop 6 e-classes by fitness (read straight from SQLite):")
print(egg.dbTop(db, 6)[["Id", "Expression", "Fitness", "Size", "DL"]])

print("\nNumber of e-classes containing an addition (EAdd):")
print(egg.dbCount(db, "EAdd"))

print("\nPareto front (max fitness x min size), read from SQLite:")
print(egg.dbPareto(db))

print("\nDistribution of evaluated e-classes per model size (size <= 30):")
print(egg.dbDistribution(db, 30))

# ---------------------------------------------------------------------------
# 4. Reload the persisted graph into memory when you do need it resident.
# ---------------------------------------------------------------------------
print("\nReloading the persisted graph into memory:")
egg.loadDB(db)
print(egg.top(5)[["Id", "Expression", "Fitness", "Size"]])

os.remove(data_csv)
