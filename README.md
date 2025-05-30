# rEGGression (r🥚ression) - Nonlinear regression models exploration and query system with e-graphs (egg).

*r🥚ression* an interactive tool that can help SR users to explore alternative models generated from different sources. These sources can be: the final population of a single run, the Pareto front, the entire history of visited expressions during the search, or a combination of those sources from multiple runs of the same or different algorithms. This can provide a rich library of alternative expressions generated from different biases, induced by different hyper-parameters or algorithms, that can bring invaluable information about the data.
This tool supports simple queries such as querying for the top-N models filtered by size, complexity, and number of numerical parameters; insert and evaluate new expressions; list and evaluate sub-expressions of the already visited expressions, and also, more advanced queries such as calculate the frequency of common patterns (i.e., building blocks) observed in the set of models; and filter the expressions by patterns with a natural syntax.

This repository provides a CLI and a Python package for rEGGression with a scikit-learn compatible API for symbolic regression.

Instructions:

- [CLI version](#cli)
- [Python version](#python)

## Changelog

### v1.0.6

- included method `importFromCSV` to import equations from other SR algorithms
- fixed bug that may create fake duplicates in e-graph
- added top-n option in distributionOfTokens
- fixed bug in pattern matching

### v1.0.2

- Fused the CLI tool and Python wrapper, you can install both with `pip install`. The executable name is `reggression`.
- Improved Python interface
- Added the command `distribution-tokens` (method `distributionOfTokens` in Python) that shows the distributions of tokens and average fitness (requested by @gbomarito)
- Added the command `extract-pattern` (method `extractPattern` in Python) that shows all the patterns that can be extracted from a single expression (idea from @juliareuter)

## CLI

### How to use 

```bash
r🥚ression - Nonlinear regression models exploration and query system with
e-graphs (egg).

Usage: reggression (-d|--dataset INPUT-FILE) [-t|--test ARG] 
                   [--distribution ARG] [--dump-to ARG] [--load-from ARG] 
                   [--parse-csv ARG] [--convert ARG] [--parse-parameters] 
                   [--to ARG] [--calculate-dl]

  Exploration and query system for a database of regression models using
  e-graphs.

Available options:
  -d,--dataset INPUT-FILE  CSV dataset.
  -t,--test ARG            test data (default: "")
  --distribution ARG       distribution of the data. (default: Gaussian)
  --dump-to ARG            dump final e-graph to a file. (default: "")
  --load-from ARG          load initial e-graph from a file. (default: "")
  --parse-csv ARG          parse-csv CSV file with the format
                           expression,parameters,fitness. The fitness value
                           should be maximization and the parameters a ;
                           separated list (there must be an additional parameter
                           for sigma in the Gaussian distribution). The format
                           of the equation is determined by the extension of the
                           file, supported extensions are operon, pysr, tir,
                           itea, hl (heuristiclab), gomea, feat, etc.
                           (default: "")
  --convert ARG            convert FROM TO, converts equation format from a
                           given format (see 'parse-csv') to either 'math' or
                           'numpy'. The 'math' format is compatible with the tir
                           format, so you can use this to standardize the
                           equations from multiple sources into a single file.
                           The output will be written to stdout. (default: "")
  --parse-parameters       Extract the numerical parameters from the expression.
                           In this case the csv file should be formatted as
                           "equation,error,fitness, where 'error' is the error
                           term used in Gaussia likelihood, it can be empty if
                           using other distributions."
  --to ARG                 Format to convert to. (default: MATH)
  --calculate-dl           (re)calculate DL.
  -h,--help                Show this help text
```

The dataset file must contain a header with each features name, and the `--dataset` and `--test` arguments can be accompanied by arguments separated by ':' following the format:

`filename.ext:start_row:end_row:target:features`

where each ':' field is optional. The fields are:

- **start_row:end_row** is the range of the training rows (default 0:nrows-1).
   every other row not included in this range will be used as validation
- **target** is either the name of the  (if the datafile has headers) or the index
   of the target variable
- **features** is a comma separated list of names or indices to be used as
  input variables of the regression model.

Example of valid names: `dataset.csv`, `mydata.tsv`, `dataset.csv:20:100`, `dataset.tsv:20:100:price:m2,rooms,neighborhood`, `dataset.csv:::5:0,1,2`.

The format of the file will be determined by the extension (e.g., csv, tsv,...). 

### Demo

[![asciicast](https://asciinema.org/a/713509.svg)](https://asciinema.org/a/713509)

### Installation 

To install rEGGression you'll need:

- `libz`
- `libnlopt`
- `libgmp`
- `ghc-9.6.6`
- `cabal` or `stack`

### Method 1: PIP

Simply run:

```bash
pip install reggression 
```

under your Python environment.

## Method 2: cabal

After installing the dependencies (e.g., `apt install libz libnlopt libgmp`), install [`ghcup`](https://www.haskell.org/ghcup/#)

For Linux, macOS, FreeBSD or WSL2:

```bash 
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

For Windows, run the following in a PowerShell:

```bash
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

After the installation, run `ghcup tui` and install the latest `stack` or `cabal` together with `ghc-9.6.6` (select the items and press `i`).
To install `srsimplify` simply run:

```bash 
cabal install
```

## Python

### Features

- Load and analyze symbolic regression models from e-graph files
- Import expressions from various symbolic regression tools (TIR, HeuristicLab, Operon, etc.)
- Query expressions by patterns, size, parameters, and complexity
- Analyze expression distributions and patterns
- Extract Pareto fronts of accuracy vs. expression size
- Support for different loss functions for various problem types
- Optimization and reporting capabilities

### Usage

You can find a Jupyter Notebook with examples [here](https://github.com/folivetti/pyreggression/tree/main/test)

### Basic Usage

```python
from pyreggression import PyReggression

# Load from an existing e-graph file
egg = PyReggression(
    dataset="train_data.csv", 
    loadFrom="my_models.egraph"
)

# Get the top 10 expressions by fitness
top_models = egg.top(10)
print(top_models)

# Save the e-graph to a new file
egg.save("updated_models.egraph")
```

### Importing from Other Symbolic Regression Tools

```python
from pyreggression import PyReggression

# Import expressions from a CSV file generated by another tool
egg = PyReggression(
    dataset="train_data.csv",
    parseCSV="operon_results.operon",  # File extension indicates the source tool
    parseParams=True
)

# Get the top models
best_models = egg.top(5)
print(best_models)
```

### Pattern Matching and Filtering

```python
from pyreggression import PyReggression

egg = PyReggression(dataset="train_data.csv", loadFrom="models.egraph")

# Find expressions with specific characteristics
filtered = egg.top(
    n=10,
    filters=["size < 15", "parameters <= 3"],
    criteria="fitness",
    pattern="v0 * x0",  # Match any expression multiplied by x0
    isRoot=False
)
print(filtered)

# Count occurrences of a pattern
count = egg.countPattern("sin(v0)")
print(f"Number of expressions containing sine: {count}")
```

### Distribution Analysis

```python
from pyreggression import PyReggression

egg = PyReggression(dataset="train_data.csv", loadFrom="models.egraph")

# Analyze pattern distribution
dist = egg.distribution(
    filters=["size <= 10"],
    limitedAt=20,
    dsc=True,
    byFitness=True,
    atLeast=100,
    fromTop=5000
)
print(dist)
```

### Testing on New Data

```python
from pyreggression import PyReggression

egg = PyReggression(
    dataset="train_data.csv",
    testData="test_data.csv",
    loadFrom="models.egraph"
)

# Get the top models evaluated on test data
test_results = egg.top(10)
print(test_results)
```

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `dataset` | str | required | Filename of the training dataset in CSV format |
| `testData` | str | "" | Filename of the test dataset in CSV format |
| `loss` | str | "MSE" | Loss function: "MSE", "Gaussian", "Bernoulli", or "Poisson" |
| `loadFrom` | str | "" | Filename of an e-graph to load |
| `parseCSV` | str | "" | CSV file with expressions from another tool |
| `parseParams` | bool | True | Whether to extract parameter values from expressions |

### Supported File Extensions for parseCSV

PyReggression can import expressions from various symbolic regression tools:

- `.tir` - TIR and ITEA
- `.hl` - HeuristicLab
- `.operon` - Operon
- `.bingo` - BINGO
- `.gomea` - GP-GOMEA
- `.pysr` - PySR
- `.sbp` - SBP
- `.eplex` - EPLEX, FEAT, BRUSH

### Methods

### Query Methods
- `top(n, filters, criteria, pattern, isRoot, negate)`: Returns top expressions by criteria
- `distribution(filters, limitedAt, dsc, byFitness, atLeast, fromTop)`: Returns pattern distribution
- `countPattern(pattern)`: Counts occurrences of a pattern
- `pareto(byFitness)`: Returns the Pareto front of accuracy vs. size

### Analysis Methods
- `report(n)`: Detailed report of e-class n
- `optimize(n)`: Re-optimize parameters for e-class n
- `subtrees(n)`: Return subtrees of e-class n

### Manipulation Methods
- `insert(expr)`: Insert a new expression
- `save(fname)`: Save the e-graph file
- `load(fname)`: Load an e-graph file
- `runQuery(query, df)`: Run a custom query against the e-graph

## Pattern Syntax

Pattern matching uses the following syntax:
- `x0, x1, ...` - Input variables
- `t0, t1, ...` - Model parameters
- `v0, v1, ...` - Pattern variables (match any expression)

Examples:
- `t0 * x0` - Match exactly this expression
- `v0 * x0` - Match any expression multiplied by x0
- `sin(v0)` - Match sine of any expression
- `v0 + v1` - Match any addition
- `v0 + x0 * v1^v0` - Match an expression `v0` added to `x0` multiplied by the expression `v1` to the power of `v0`. E.g., `t0*x1 + x0 * t1^(t0*x1)`

## License

[LICENSE]

## Citation

If you use PyReggression in your research, please cite:

```
@inproceedings{rEGGression,
author = {de Franca, Fabricio Olivetti and Kronberger, Gabriel},
title = {rEGGression: an Interactive and Agnostic Tool for the Exploration of Symbolic Regression Models},
year = {2025},
isbn = {9798400714658},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
url = {https://doi.org/10.1145/3712256.3726385},
doi = {10.1145/3712256.3726385},
booktitle = {Proceedings of the Genetic and Evolutionary Computation Conference},
pages = {},
numpages = {9},
keywords = {Genetic programming, Symbolic regression, Equality saturation, e-graphs},
location = {Malaga, Spain},
series = {GECCO '25},
archivePrefix = {arXiv},
       eprint = {2501.17859},
 primaryClass = {cs.LG},
}
```

## Acknowledgments

The bindings were created following the amazing example written by [wenkokke](https://github.com/wenkokke/example-haskell-wheel)

Fabricio Olivetti de Franca is supported by Conselho Nacional de Desenvolvimento Cient\'{i}fico e Tecnol\'{o}gico (CNPq) grant 301596/2022-0.

Gabriel Kronberger is supported by the Austrian Federal Ministry for Climate Action, Environment, Energy, Mobility, Innovation and Technology, the Federal Ministry for Labour and Economy, and the regional government of Upper Austria within the COMET project ProMetHeus (904919) supported by the Austrian Research Promotion Agency (FFG). 
