# rEGGression — Python Library Documentation

**Version:** 2.4.0  
**Author:** Fabricio Olivetti de Franca  
**License:** See LICENSE  
**Python:** >= 3.9 (3.9–3.14 supported)  
**Platforms:** Linux (x86\_64, aarch64), macOS (x86\_64, arm64), Windows (AMD64)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Core Concepts](#core-concepts)
5. [Constructor](#constructor)
6. [Configuration Methods](#configuration-methods)
7. [Query Methods](#query-methods)
8. [Pattern Analysis Methods](#pattern-analysis-methods)
9. [Insertion and Equality Saturation](#insertion-and-equality-saturation)
10. [Persistence Methods](#persistence-methods)
11. [Import Methods](#import-methods)
12. [DB-Only Operations](#db-only-operations)
13. [Visualization](#visualization)
14. [Pattern Syntax](#pattern-syntax)
15. [Dataset Specification Format](#dataset-specification-format)
16. [Loss Functions](#loss-functions)
17. [Import Formats](#import-formats)
18. [Split-DB Architecture](#split-db-architecture)
19. [Tutorials Index](#tutorials-index)
20. [Method-Mode Matrix](#method-mode-matrix)
21. [Citation](#citation)

---

## Introduction

rEGGression (r🥚ression) is an interactive tool for exploring and querying symbolic regression models using **e-graphs** (equality graphs). It can ingest expressions from multiple symbolic regression algorithms (PySR, Operon, TIR, etc.), merge them into a single e-graph, and provide rich queries over the combined model space.

### Key Features

- **Load and analyze** symbolic regression models from e-graph files or SQLite databases
- **Import expressions** from various SR tools (TIR, HeuristicLab, Operon, BINGO, GP-GOMEA, PySR, SBP, EPLEX, FEAT, BRUSH)
- **Query expressions** by patterns, size, parameters, and complexity
- **Analyze** expression distributions, building blocks, and token frequencies
- **Extract Pareto fronts** of accuracy vs. expression size
- **Equality saturation** to discover equivalent expressions
- **DB-backed mode** for out-of-core operation on graphs with millions of expressions (O(1) memory)
- **Profile-likelihood confidence intervals** for fitted parameters
- **Scikit-learn compatible API** for symbolic regression
- **Multiple loss functions** for regression, classification, and quantile tasks

### Quick Example

```python
from reggression import Reggression

# Load from an existing e-graph file
egg = Reggression(dataset="train_data.csv", loadFrom="my_models.egraph")

# Get the top 10 expressions by fitness
top_models = egg.top(10)
print(top_models)

# Find expressions containing sine
sine_models = egg.top(5, pattern="sin(v0)")

# Get the Pareto front
pareto = egg.pareto()
```

---

## Installation

### Requirements

- `libz`
- `libnlopt`
- `libgmp`

### Install via pip

```bash
pip install reggression
```

### Install from source

```bash
# Install system dependencies (Ubuntu/Debian)
apt install libz libnlopt libgmp

# Install Haskell toolchain via ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Build and install
cabal build
pip install .
```

---

## Quick Start

### Example 1: Load and Query an Existing E-graph

```python
from reggression import Reggression

egg = Reggression(
    dataset="train_data.csv",
    testData="test_data.csv",
    loadFrom="models.egraph"
)

# Top 5 expressions by fitness
print(egg.top(5))

# Top 5 with size filter
print(egg.top(5, filters=["size < 10", "parameters <= 3"]))

# Pareto front
print(egg.pareto())
```

### Example 2: Import from Another SR Tool

```python
from reggression import Reggression

egg = Reggression(
    dataset="train_data.csv",
    parseCSV="operon_results.operon",  # extension determines parser
    parseParams=True
)

print(egg.top(10))
egg.save("merged.egraph")
```

### Example 3: Build from Scratch

```python
from reggression import Reggression

egg = Reggression(dataset="data.csv", loss="MSE")

# Insert expressions
egg.insert("t0 * x0 + t1")
egg.insert("sin(x0) * x1")

# Run equality saturation to discover equivalences
egg.eqsat(10)

# See equivalent forms of the first expression
print(egg.getNExpressions(0, 10))
```

### Example 4: DB-Only Session (Out-of-Core)

```python
from reggression import Reggression

# Assumes egraph.db was created with srtree-db CLI
egg = Reggression(
    dataset="train_data.csv",
    loss="Gaussian",
    db="egraph.db",
    fitDb="fit_train.db",
    dataset_name="my_dataset"
)

# All queries go directly to SQLite — O(1) memory
print(egg.top(10))
print(egg.pareto())
print(egg.report(42))
```

---

## Core Concepts

### E-graphs

An **e-graph** (equality graph) is a data structure that compactly represents many equivalent expressions. When you insert `x0 + x1` and `(x0 + x1) * 1`, they are stored in the same **e-class** because they are semantically equivalent.

### E-classes

An **e-class** is a set of equivalent expression nodes. Each e-class has a unique integer ID. All queries (`top`, `pareto`, `report`, etc.) operate on e-class IDs.

### Equality Saturation

**Equality saturation** (eqsat) applies rewrite rules (e.g., `x * 1 → x`, `x + 0 → x`) to discover new equivalent expressions. Running eqsat on an e-graph can merge e-classes and reveal that two apparently different expressions are actually equivalent.

### Fitness and Description Length

- **Fitness** measures how well an expression fits the training data (higher is better for most loss functions).
- **Description Length (DL)** is a complexity measure combining model fit and complexity penalty (lower is better).

---

## Constructor

```python
Reggression(
    dataset,               # str — path to training CSV (required)
    testData="",           # str — path to test CSV
    loss="MSE",            # str — loss function
    loadFrom="",           # str — path to .egraph binary file
    parseCSV="",           # str — CSV of expressions from another SR tool
    parseParams=True,      # bool — extract numeric params from expressions
    refit=False,           # bool — re-fit all expressions on load
    simpleOutput=False,    # bool — restrict output to Id/Latex/Fitness
    dataset_name="",       # str — dataset name for DB tables
    db="",                 # str — SQLite egraph DB path
    fitDb="",              # str — separate fit DB path
    pinball_tau=0.5,       # float — quantile for Pinball loss
)
```

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `dataset` | str | **required** | Path to the training dataset CSV file. Must have a header row. |
| `testData` | str | `""` | Path to the test dataset CSV. If provided, test metrics appear in `report()` and `top()`. |
| `loss` | str | `"MSE"` | Loss function. See [Loss Functions](#loss-functions). |
| `loadFrom` | str | `""` | Path to a binary `.egraph` file to load. The e-graph must match the dataset and loss. |
| `parseCSV` | str | `""` | CSV file with expressions from another SR tool. Extension determines the parser format. |
| `parseParams` | bool | `True` | If `True`, extract numeric constants from expressions as parameters. |
| `refit` | bool | `False` | If `True`, re-fit all loaded expressions against the dataset. |
| `simpleOutput` | bool | `False` | If `True`, DataFrames are restricted to `Id`, `Latex`, `Fitness` columns. |
| `dataset_name` | str | `""` | Name for the dataset in DB tables. Defaults to the CSV filename stem. |
| `db` | str | `""` | Path to a SQLite e-graph database. Enables DB-only mode (all queries go to SQLite, O(1) memory). |
| `fitDb` | str | `""` | Path to a separate fit database (split-DB architecture). See [Split-DB Architecture](#split-db-architecture). |
| `pinball_tau` | float | `0.5` | Quantile for Pinball loss. Must be between 0 and 1 (exclusive). |

### Behavior Notes

- If `db` is provided, the session operates in **DB-only mode**: all queries go directly to SQLite.
- If `db` is empty, a temporary SQLite file is created in the current directory and cleaned up when the object is destroyed.
- The `dataset` CSV is always required (for column names and description-length computation).
- If both `loadFrom` and `parseCSV` are provided, `loadFrom` takes precedence.

---

## Configuration Methods

### `set_simple_output(b)`

Toggle simple output mode. When `True`, DataFrames returned by queries are restricted to columns `['Id', 'Latex', 'Fitness']`.

```python
egg.set_simple_output(True)
print(egg.top(5))  # Only Id, Latex, Fitness columns
```

**Parameters:**
| Parameter | Type | Description |
|-----------|------|-------------|
| `b` | bool | `True` to enable simple output |

### `set_varnames(names)`

Set custom variable names for display. Pass a list of strings matching the dataset column order.

```python
egg.set_varnames(["temperature", "pressure", "flow_rate"])
```

**Parameters:**
| Parameter | Type | Description |
|-----------|------|-------------|
| `names` | list[str] | Variable names in column order |

---

## Query Methods

### `top(n, filters, criteria, pattern, isRoot, negate, ci)`

Returns the top-N expressions ranked by a criterion.

```python
# Top 5 by fitness
egg.top(5)

# Top 10 by description length, filtered by size
egg.top(10, criteria="dl", filters=["size < 15"])

# Top 5 matching a pattern
egg.top(5, pattern="v0 * x0")

# Top 5 NOT matching a pattern
egg.top(5, pattern="sin(v0)", negate=True)

# With confidence intervals
egg.top(5, ci=True)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n` | int | `5` | Number of expressions to return |
| `filters` | list[str] | `[]` | Filters in format `"field op value"`. Fields: `size`, `parameters`, `cost`. Ops: `<`, `<=`, `=`, `>=`, `>`. Example: `["size < 10", "parameters > 2"]` |
| `criteria` | str | `"fitness"` | Sort by `"fitness"` (maximize) or `"dl"` (description length, minimize) |
| `pattern` | str | `""` | Pattern expression. See [Pattern Syntax](#pattern-syntax). |
| `isRoot` | bool | `False` | If `True`, pattern must match at the root of the expression |
| `negate` | bool | `False` | If `True`, return expressions NOT matching the pattern |
| `ci` | bool | `False` | If `True`, include profile-likelihood confidence intervals |

**Returns:** `pd.DataFrame` with columns `Id`, `Expression`, `Fitness`, `Size`, `DL`, and optionally CI columns.

---

### `pareto(byFitness, ci)`

Returns the Pareto front of accuracy vs. expression size.

```python
# Pareto front by fitness
pareto = egg.pareto()

# Pareto front by description length
pareto_dl = egg.pareto(byFitness=False)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `byFitness` | bool | `True` | If `True`, first objective is fitness; if `False`, uses description length |
| `ci` | bool | `False` | Include profile-likelihood confidence intervals |

**Returns:** `pd.DataFrame` with Pareto-optimal expressions.

---

### `distribution(filters, limitedAt, dsc, byFitness, atLeast, fromTop)`

Returns the distribution of common structural patterns across expressions.

```python
# Top 20 patterns by frequency
dist = egg.distribution(limitedAt=20, byFitness=False)

# Patterns in the top 1000 expressions
dist = egg.distribution(fromTop=1000, atLeast=50)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `filters` | list[str] | `[]` | Size filters (e.g., `["size < 10"]`) |
| `limitedAt` | int | `25` | Max number of patterns to display |
| `dsc` | bool | `True` | Sort descending (`True`) or ascending (`False`) |
| `byFitness` | bool | `True` | Sort by average fitness (`True`) or frequency (`False`) |
| `atLeast` | int | `1000` | Minimum pattern frequency |
| `fromTop` | int | `5000` | Subset size (capped at 10000) |

**Returns:** `pd.DataFrame` with columns `Pattern`, `Count`, `AvgFitness`.

---

### `modularity(n, filters, byFitness)`

Finds expressions with repeated sub-expressions (reusable building blocks).

```python
# Top 10 modular expressions
mod = egg.modularity(10)

# With size filter on sub-components
mod = egg.modularity(5, filters=["> 2"])
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n` | int | **required** | Number of top equations to return (capped at 10000) |
| `filters` | list[str] | `["> 1"]` | Size filters for sub-components |
| `byFitness` | bool | `True` | Sort by fitness or frequency |

**Returns:** `pd.DataFrame` showing modular expressions with repeated sub-components.

---

### `countPattern(pattern)`

Counts how many expressions contain a given pattern.

```python
count = egg.countPattern("sin(v0)")
# Returns: "sin(v0) appears in 42 equations."

count = egg.countPattern("v0 * x0 + v1")
```

**Parameters:**
| Parameter | Type | Description |
|-----------|------|-------------|
| `pattern` | str | Pattern string. See [Pattern Syntax](#pattern-syntax). |

**Returns:** `str` — human-readable count message.

---

### `report(n, ci)`

Detailed report of an e-class: expression, fitness, MSE, R², NLL, DL, parameters, and optionally confidence intervals.

```python
report = egg.report(42)
print(report)

# With confidence intervals
report_ci = egg.report(42, ci=True)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n` | int | **required** | E-class ID |
| `ci` | bool | `False` | Include profile-likelihood confidence intervals |

**Returns:** `pd.DataFrame` with `Info`, `Training`, `Test` columns showing metrics.

---

### `optimize(n, ci)`

Re-optimize the parameters of e-class `n` using NLopt.

```python
result = egg.optimize(42)
print(result)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n` | int | **required** | E-class ID |
| `ci` | bool | `False` | Include profile-likelihood confidence intervals |

**Returns:** `str` — optimization result message.

---

### `subtrees(n)`

Returns the e-class IDs of all sub-expressions in e-class `n`.

```python
sub = egg.subtrees(42)
print(sub)  # Comma-separated string of e-class IDs
```

**Parameters:**
| Parameter | Type | Description |
|-----------|------|-------------|
| `n` | int | E-class ID |

**Returns:** `str` — comma-separated e-class IDs.

---

### `getNExpressions(eid, n)`

Returns up to N equivalent expressions from a single e-class.

```python
# Get 10 equivalent forms of e-class 42
exprs = egg.getNExpressions(42, 10)
print(exprs.Expression)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `eid` | int | **required** | E-class ID |
| `n` | int | `10` | Max number of expressions |

**Returns:** `pd.DataFrame` with `Expression` column.

---

### `getNEclasses(eid, n)`

Returns e-class ID sets for up to N expression variants from an e-class.

```python
ids = egg.getNEclasses(42, 5)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `eid` | int | **required** | E-class ID |
| `n` | int | `10` | Max number of variants |

**Returns:** `pd.DataFrame` with `EClassIds` column (comma-separated IDs per variant).

**Note:** DB-only method.

---

## Pattern Analysis Methods

### `extractPattern(eid)`

Enumerates all structural sub-patterns found in a single expression.

```python
patterns = egg.extractPattern(42)
print(patterns)
#   Pattern  Count
# 0   v0+v1      3
# 1   v0*v1      2
# ...
```

**Parameters:**
| Parameter | Type | Description |
|-----------|------|-------------|
| `eid` | int | E-class ID |

**Returns:** `pd.DataFrame` with `Pattern`, `Count` columns.

---

### `distributionOfTokens(top)`

Counts token (operator) frequencies and their average fitness.

```python
# All tokens
tokens = egg.distributionOfTokens()

# Top 100 expressions only
tokens = egg.distributionOfTokens(top=100)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `top` | int | `-1` | Number of top expressions to analyze. `-1` = all (capped at 10000). |

**Returns:** `pd.DataFrame` with `Token`, `Count`, `AvgFitness` columns.

---

### `patternMap(pattern, limit)`

Shows what each wildcard variable (v0, v1, ...) matched in every occurrence of a pattern, along with e-class IDs.

```python
# See what v0 and v1 matched in "v0 * v1"
pm = egg.patternMap("v0 * v1")
print(pm)
#   Match  Expression      v0        v0_eid  v1        v1_eid
# 0     0  t0 * x0         t0            3  x0            1
# 1     1  sin(x1) * x0    sin(x1)       7  x0            1

# Limit to 5 matches
pm = egg.patternMap("v0 + v1", limit=5)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `pattern` | str | **required** | Pattern with wildcards (e.g., `"v0 * v1"`, `"exp(v0) + v1"`) |
| `limit` | int | `-1` | Max matches to return. `-1` = all (capped at 10000). |

**Returns:** `pd.DataFrame` with columns `Match`, `Expression`, `v0`, `v0_eid`, `v1`, `v1_eid`, ...

---

### `eclassTerminals(eid)`

Lists all unique terminals (variables, parameters, constants) inside an e-class, with cycle detection.

```python
terms = egg.eclassTerminals(42)
print(terms)
#    Type   Name
# 0   Var     x0
# 1   Var     x1
# 2  Param    t0
```

**Parameters:**
| Parameter | Type | Description |
|-----------|------|-------------|
| `eid` | int | E-class ID |

**Returns:** `pd.DataFrame` with `Type` (one of `"Var"`, `"Param"`, `"Const"`) and `Name` (e.g., `"x0"`, `"t1"`, `"3.14"`).

---

## Insertion and Equality Saturation

### `insert(expr, alg)`

Insert a new expression into the e-graph and optimize its parameters.

```python
result = egg.insert("sin(x0) + t0 * x1")
print(result)
#    Id  Expression         Fitness  Size
# 0  42  sin(x0) + t0*x1   0.95      7

# Using Operon format
result = egg.insert("sin($0) + $1 * $2", alg="OPERON")
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `expr` | str | **required** | Expression string |
| `alg` | str | `"TIR"` | Parser algorithm: `TIR`, `HL`, `OPERON`, `BINGO`, `GOMEA`, `PYSR`, `SBP`, `EPLEX`, `NEOGP` |

**Returns:** `pd.DataFrame` with `Id` (e-class ID), `Expression`, `Fitness`, `Size`.

---

### `eqsat(n)`

Run N steps of equality saturation on the in-memory e-graph. Each rule is applied sequentially.

```python
egg.eqsat(5)   # 5 iterations
egg.eqsat(20)  # 20 iterations
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n` | int | `1` | Number of eqsat iterations |

**Returns:** `str` — status message.

**Warning:** Can be slow and memory-intensive on large graphs. For large graphs, use `dbEqSat()` instead.

---

## Persistence Methods

### `save(fname)` / `load(fname)`

Save or load the e-graph as a binary `.egraph` file. Aliases for `persist()` and `loadDB()`.

```python
egg.save("my_models.egraph")
egg.load("my_models.egraph")
```

---

### `persist(fname, fitDb)`

Save the current e-graph to a SQLite database (srtree-db format).

```python
egg.persist("egraph.db")

# With split-DB
egg.persist("egraph.db", fitDb="fit_train.db")
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `fname` | str | `""` | SQLite DB filename. If empty, uses `self.db`. |
| `fitDb` | str | `""` | Separate fit DB filename (for split-DB mode). |

---

### `loadDB(fname, fitDb)`

Load an e-graph previously persisted with `persist()` into memory.

```python
egg.loadDB("egraph.db")

# With split-DB
egg.loadDB("egraph.db", fitDb="fit_train.db")
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `fname` | str | `""` | SQLite DB filename. If empty, uses `self.db`. |
| `fitDb` | str | `""` | Separate fit DB filename. |

---

## Import Methods

### `importFromCSV(fname, extractParameters)`

Import expressions from a CSV file. The file extension determines the parser format.

```python
# Import from Operon output
egg.importFromCSV("results.operon")

# Import from PySR output, keep literal constants
egg.importFromCSV("pysr_output.pysr", extractParameters=False)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `fname` | str | **required** | CSV file path. Extension determines format. |
| `extractParameters` | bool | `True` | Convert float constants to parameters |

**CSV format:** `expression,parameters,fitness` (one expression per line).

---

### `importDB(eqs, fname, extractParameters, fitDb)`

Build an e-graph directly in SQLite by streaming expressions from a CSV file. No in-memory e-graph is ever built. The resulting DB is identical to `persist()` of the equivalent in-memory seed.

```python
# Out-of-core import — O(1) memory
egg.importDB("expressions.tir", fname="egraph.db")

# With split-DB
egg.importDB("expressions.tir", fname="egraph.db", fitDb="fit.db")
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `eqs` | str | **required** | Path to CSV file of expressions |
| `fname` | str | `""` | SQLite DB filename. If empty, uses `self.db`. |
| `extractParameters` | bool | `True` | Convert float constants to parameters |
| `fitDb` | str | `""` | Separate fit DB filename |

**Returns:** `str` — summary message.

---

## DB-Only Operations

These methods operate directly on a SQLite database without loading the full e-graph into memory. They are available when the session was created with `db=` or when called with an explicit `fname` parameter.

### `dbEqSat(fname, iterations, ruleset, fitDb)`

Run equality saturation out-of-core against a lazily loaded (paged) e-graph in SQLite.

```python
egg.dbEqSat(fname="egraph.db", iterations=20)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `fname` | str | `""` | SQLite DB filename |
| `iterations` | int | `10` | Number of eqsat iterations |
| `ruleset` | str | `"default"` | Rule set (`"default"` or `"params"`) |
| `fitDb` | str | `""` | Separate fit DB filename |

---

### `dbEqSatFrontier(fname, iterations, ruleset, fitDb)`

Re-saturate only the frontier (recently created/merged classes) of a paged e-graph. Avoids re-working unchanged parts of the graph.

```python
# After inserting new expressions, re-saturate only what changed
egg.dbEqSatFrontier(fname="egraph.db", iterations=10)
```

**Parameters:** Same as `dbEqSat`.

---

### `dbInsert(fname, expr, alg, fitDb)`

Insert a single expression into the DB-backed e-graph. Content-addressed (deduplicates existing subexpressions). Marks genuinely-new classes as part of the frontier.

```python
eid = egg.dbInsert(fname="egraph.db", expr="sin(x0) + t0 * x1")
print(f"Inserted as e-class {eid}")
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `fname` | str | `""` | SQLite DB filename |
| `expr` | str | `""` | Expression to insert |
| `alg` | str | `"TIR"` | Parser algorithm |
| `fitDb` | str | `""` | Separate fit DB filename |

**Returns:** `int` — the root e-class ID.

---

### `dbSetFit(fname, eid, fitness, fitDb)`

Record the fitness of a single e-class in `dataset_fit`, so a newly-inserted expression can be ranked by the query layer.

```python
eid = egg.dbInsert(fname="egraph.db", expr="t0 * x0")
# ... evaluate fitness externally ...
egg.dbSetFit(fname="egraph.db", eid=eid, fitness=0.95)
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `fname` | str | `""` | SQLite DB filename |
| `eid` | int | `0` | E-class ID |
| `fitness` | float | `0.0` | Fitness value |
| `fitDb` | str | `""` | Separate fit DB filename |

---

### `dbPushFit(fname, fitDb)`

Write the current in-memory e-graph's fitness/DL metrics into the fit table of the SQLite DB.

```python
egg.dbPushFit(fname="egraph.db")
```

---

### `dbRefreshFitness(fname, fitDb)`

Overwrite in-memory fitness values with those stored in the SQLite DB's fit table.

```python
egg.dbRefreshFitness(fname="egraph.db")
```

---

### `dbStream(fname, op, budget, fitDb)`

Stream the enode table by operator through a SQLite cursor. Reports total count and first `budget` matches. Useful for validating O(1)-memory streaming.

```python
egg.dbStream(fname="egraph.db", op="EAdd", budget=100)
```

---

### DB-Backed eggp Loop Pattern

The typical DB-backed GP loop uses these methods together:

```python
from reggression import Reggression

egg = Reggression(dataset="data.csv", db="egraph.db", dataset_name="mydata")

# Insert a new expression
eid = egg.dbInsert(fname="egraph.db", expr="sin(x0) + t0 * x1")

# Re-saturate only the frontier
egg.dbEqSatFrontier(fname="egraph.db", iterations=5)

# Record fitness after evaluation
egg.dbSetFit(fname="egraph.db", eid=eid, fitness=0.95)

# Query results
print(egg.top(10))
```

---

## Visualization

### `profilePlot(eid, dataPath, dataSpec, saveTo)`

Compute profile-likelihood data and plot tau-vs-theta curves and pairwise theta-by-theta contour plots.

```python
# Show plot interactively
fig = egg.profilePlot(42)

# Save to file
fig = egg.profilePlot(42, saveTo="profile_plot.png")
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `eid` | int | **required** | E-class ID of the expression to profile |
| `dataPath` | str | `None` | Path to dataset CSV. If `None`, uses `self.dataset`. |
| `dataSpec` | str | `None` | Full data spec (e.g., `"file.csv:::y:0,1"`). Overrides `dataPath`. |
| `saveTo` | str | `None` | File path to save plot. If `None`, shows interactively. |

**Returns:** `matplotlib.Figure`

**Requirements:**
- Expression must have >= 2 parameters
- Requires `matplotlib`

---

## Pattern Syntax

Patterns are mathematical expressions with special variables:

| Syntax | Meaning | Example |
|--------|---------|---------|
| `x0`, `x1`, ... | Input variables (concrete) | `x0 + x1` |
| `t0`, `t1`, ... | Numerical parameters (concrete) | `t0 * x0` |
| `v0`, `v1`, ... | Wildcard variables (match anything) | `v0 * x0` |

### Supported Operators

**Binary operators:**
`+`, `-`, `*`, `/`, `^` (or `**`), `aq` (analytic quotient)

**Unary operators:**
`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `exp`, `log`, `sqrt`, `cbrt`, `abs`, `square`, `cube`, `recip`, `logabs` (or `|log|`), `sqrtabs` (or `|sqrt|`)

### Examples

| Pattern | Matches |
|---------|---------|
| `t0 * x0` | Exactly `t0 * x0` |
| `v0 * x0` | Any expression multiplied by `x0` (e.g., `t0 * x0`, `sin(x1) * x0`) |
| `v0 + v1` | Any addition (e.g., `x0 + x1`, `t0 + sin(x0)`) |
| `v0 ^ v0` | Any expression raised to itself (e.g., `x0^x0`) |
| `sin(v0)` | Sine of any expression |
| `exp(v0) + v1` | `exp` of something plus something else |
| `v0 + x0 * v1^v0` | Complex nested pattern |

---

## Dataset Specification Format

The `dataset` and `testData` parameters accept a path with optional colon-separated arguments:

```
filename.ext:start_row:end_row:target:features
```

| Field | Description | Example |
|-------|-------------|---------|
| `filename.ext` | CSV/TSV file path | `data.csv` |
| `start_row:end_row` | Row range (0-indexed) | `20:100` |
| `target` | Target column name or index | `y` or `5` |
| `features` | Comma-separated feature names/indices | `x1,x2` or `0,1,2` |

### Examples

| Spec | Description |
|------|-------------|
| `data.csv` | Use all rows, last column as target |
| `data.csv:20:100` | Rows 20–99, last column as target |
| `data.tsv:20:100:price:m2,rooms` | Rows 20–99, `price` as target, `m2` and `rooms` as features |
| `data.csv:::5:0,1,2` | All rows, column 5 as target, columns 0,1,2 as features |

---

## Loss Functions

| Loss | Use Case | Description |
|------|----------|-------------|
| `"MSE"` | Regression (default) | Mean squared error |
| `"LOG10"` | Regression, multiplicative noise | Log10-scale squared error |
| `"MAE"` | Regression, robust to outliers | Mean absolute error |
| `"MAPE"` | Regression, scale-independent | Mean absolute percentage error |
| `"Gaussian"` | Regression with noise estimation | Gaussian negative log-likelihood (fits noise term) |
| `"Bernoulli"` | Binary classification | Bernoulli negative log-likelihood |
| `"Poisson"` | Count data | Poisson negative log-likelihood |
| `"LeastSquares"` | Regression, no noise term | Least-squares as negative log-likelihood |
| `"Pinball"` | Quantile regression | Quantile/pinball loss. Uses `pinball_tau` parameter. |

---

## Import Formats

The `parseCSV` parameter and `importFromCSV` method support expressions from various symbolic regression tools. The file extension determines the parser:

| Extension | Algorithm(s) |
|-----------|-------------|
| `.tir` | TIR, ITEA |
| `.hl` | HeuristicLab |
| `.operon` | Operon |
| `.bingo` | BINGO |
| `.gomea` | GP-GOMEA |
| `.pysr` | PySR |
| `.sbp` | SBP |
| `.eplex` | EPLEX, FEAT, BRUSH |

**CSV format:** `expression,parameters,fitness` (one expression per line). The `parameters` field is a semicolon-separated list of numeric parameter values.

---

## Split-DB Architecture

When `fitDb` is provided, the e-graph structure and per-dataset fitness data live in separate SQLite files:

| File | Contents | Mode |
|------|----------|------|
| **E-graph DB** (`db`) | `enode`, `enode_child`, `eclass`, `eclass_node`, `cstore_page`, `frontier`, `meta` | WAL mode, read-only during fitting |
| **Fit DB** (`fitDb`) | `dataset`, `dataset_fit`, `expression_index` | DELETE mode, one per dataset |

### Benefits

- Eliminates WAL bloat during fitting
- One e-graph can serve multiple datasets with independent fitness data
- E-graph DB stays read-only during fitting (safe for concurrent reads)

### Usage

```python
# Create with split-DB
egg = Reggression(
    "train_data.csv",
    db="egraph.db",
    fitDb="fit_gaussian.db",
    dataset_name="gaussian"
)

# Multiple fit DBs for the same e-graph
egg_gauss = Reggression("train.csv", db="egraph.db", fitDb="fit_train.db")
egg_test  = Reggression("test.csv",  db="egraph.db", fitDb="fit_test.db")
```

---

## Tutorials Index

| # | File | Topic |
|---|------|-------|
| 01 | `01_creating_egraph.py` | Create e-graph from eggp, load and query |
| 02 | `02_retrieving_top_expressions.py` | Top-N queries with filters and criteria |
| 03 | `03_retrieving_top_expressions_with_pattern_matching.py` | Pattern matching syntax |
| 04 | `04_playing_with_building_blocks.py` | Building blocks, modularity, tokens |
| 05 | `05_building_from_csv.py` | Import from Operon/Bingo CSV, merge |
| 06 | `06_starting_from_nothing.py` | Empty e-graph, insert, eqsat, equivalence |
| 07 | `07_eqsat_in_memory.py` | In-memory eqsat, top, Pareto, equivalence |
| 08 | `08_persist_and_sql_queries.py` | Persist to SQLite, SQL queries |
| 09 | `09_eqsat_on_database.py` | Out-of-core vs in-memory RSS comparison |
| 10 | `10_scale_dbEqSat.py` | Scalability: importDB + dbEqSat at scale |
| 11 | `11_eggp_db.py` | DB-backed eggp loop, resume from DB |
| 12 | `12_db_ingest.py` | srtree-db CLI: ingest + fitdata workflow |
| 13 | `13_profile_ci.py` | Profile-likelihood confidence intervals |
| 14 | `14_load_existing_db.py` | Load pre-existing DB, query, resume |
| 15 | `15_split_db_refit_status.py` | Split-DB: multiple fit DBs, refit |
| 16 | `16_pattern_map_and_terminals.py` | Pattern wildcard mapping, e-class terminals |
| 17 | `17_db_only_session.py` | Full DB-only session (no in-memory graph) |
| 18 | `18_profile_plots.py` | Profile-likelihood CI visualization |

---

## Method-Mode Matrix

| Method | In-Memory | DB-Only | Notes |
|--------|:---------:|:-------:|-------|
| `top()` | Yes | Yes | Auto-routes based on `db` |
| `pareto()` | Yes | Yes | |
| `distribution()` | Yes | Yes | DB: bounded N ≤ 10000 |
| `modularity()` | Yes | Yes | DB: bounded N ≤ 10000 |
| `countPattern()` | Yes | Yes | |
| `report()` | Yes | Yes | |
| `optimize()` | Yes | Yes | |
| `subtrees()` | Yes | Yes | |
| `getNExpressions()` | Yes | Yes | |
| `getNEclasses()` | — | Yes | |
| `extractPattern()` | Yes | Yes | DB: bounded N ≤ 10000 |
| `distributionOfTokens()` | Yes | Yes | DB: bounded N ≤ 10000 |
| `patternMap()` | Yes | Yes | DB: bounded N ≤ 10000 |
| `eclassTerminals()` | Yes | Yes | |
| `eqsat()` | Yes | — | Use `dbEqSat` for DB |
| `insert()` | Yes | Yes | DB: content-addressed + frontier |
| `save()` / `persist()` | → DB | — | |
| `load()` / `loadDB()` | DB → | — | |
| `importFromCSV()` | Yes | — | Use `importDB` for DB |
| `importDB()` | — | Yes | Out-of-core, O(1) memory |
| `dbEqSat()` | — | Yes | |
| `dbEqSatFrontier()` | — | Yes | |
| `dbInsert()` | — | Yes | |
| `dbSetFit()` | — | Yes | |
| `dbPushFit()` | Yes | — | |
| `dbRefreshFitness()` | — | Yes | |
| `dbStream()` | — | Yes | PoC |
| `profilePlot()` | — | Yes | Requires matplotlib |
| `runQuery()` | Yes | Yes | Raw command passthrough |

---

## Citation

If you use rEGGression in your research, please cite:

```bibtex
@inproceedings{rEGGression,
  author    = {de Franca, Fabricio Olivetti and Kronberger, Gabriel},
  title     = {rEGGression: an Interactive and Agnostic Tool for the Exploration of Symbolic Regression Models},
  year      = {2025},
  isbn      = {9798400714658},
  publisher = {Association for Computing Machinery},
  address   = {New York, NY, USA},
  url       = {https://doi.org/10.1145/3712256.3726385},
  doi       = {10.1145/3712256.3726385},
  booktitle = {Proceedings of the Genetic and Evolutionary Computation Conference},
  pages     = {},
  numpages  = {9},
  keywords  = {Genetic programming, Symbolic regression, Equality saturation, e-graphs},
  location  = {Malaga, Spain},
  series    = {GECCO '25},
  archivePrefix = {arXiv},
  eprint    = {2501.17859},
  primaryClass  = {cs.LG},
}
```

---

## Acknowledgments

The bindings were created following the example by [wenkokke](https://github.com/wenkokke/example-haskell-wheel).

Fabricio Olivetti de Franca is supported by Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq) grant 301596/2022-0.

Gabriel Kronberger is supported by the Austrian Federal Ministry for Climate Action, Environment, Energy, Mobility, Innovation and Technology, the Federal Ministry for Labour and Economy, and the regional government of Upper Austria within the COMET project ProMetHeus (904919) supported by the Austrian Research Promotion Agency (FFG).
