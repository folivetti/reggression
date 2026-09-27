# Changelog for reggression

## 2.4.0

### Split-DB Architecture Fixes

- **DBImport**: writes fit data to the fit DB in split-DB mode; report/optimize loss consistency
- **DBInsert**: ensures egraph schema before insert; reorders `recordExpressionIndex` after `saveGraph`
- **DBInsert**: seeds an empty paged graph when the DB has no e-graph yet
- **Insert**: added equation-format/algorithm argument

### DBReport Improvements

- **DBReport**: fills Test Fitness column
- **DBReport**: fills Test column from a separate test-data path
- **DBReport**: emits a CSV like the in-memory Report (Id/Expr/Numpy/Nodes/params/Fitness/MSE/R^2/nll/DL) + CI rows

### DBOptimize Fixes

- **DBOptimize**: passes dataset name AND data path separately so fitness lands in the right dataset
- **DBOptimize**: derives dataset name from its basename

### Bug Fixes

- Fixed `egg.insert` crash on empty DB: defensive `putEOL` + default temp DB
- Fixed issue #38

### Dependencies

- Updated dependency: srtree >= 3.0.0.4, srtree-db >= 0.1.3.0

## 2.3.0

### DB-Only Session (major feature)

- **DB-backed `Reggression` constructor**: `Reggression("data.csv", db="egraph.db", fitDb="fit.db")` operates entirely through SQLite — no tempfile, no binary serialization, no in-memory EGraph state. All queries go directly to SQLite.
- **In-memory-free architecture**: When `db` is set, no `.egraph` tempfile is created, no `Data.Binary` serialization occurs, and no EGraph state is held between calls. The Haskell side dispatches to `regressionDB` which opens its own SQLite connections per command.
- **All Python methods route to DB-native commands** in DB mode: `top`, `pareto`, `report`, `optimize`, `eqsat`, `insert`, `subtrees`, `getNExpressions`, `getNEclasses`, `eclassTerminals`, `extractPattern`, `distributionOfTokens`, `patternMap`, `distribution`, `modularity`, `countPattern`, `importFromCSV`.
- **Double guard**: Python `_isDBQuery` + Haskell `isDBCommand` provide defense-in-depth for the binary round-trip skip.

### New DB-Native Commands

- **`DBTopPattern`**: Memory-bounded pattern matching using rank-first-then-match. Queries top-M candidates by fitness, extracts trees via `extractBestFromDB`, checks pattern match via pure `patternMatches`/`patternMatchesAny`. Supports `isRoot` (root-only vs anywhere) and `negate` flags.
- **`DBReport`**: Extract expression, fitness, and optional profile-likelihood CI from DB pages. No in-memory graph needed.
- **`DBOptimize`**: Extract expression, re-fit with NLopt, write fitness back to fit DB. Accepts loss parameter.
- **`DBSubtrees`**: Walk `_best` tree via `expandTreeFromDB`, collect all reachable eclass IDs.
- **`DBGetNExprs`**: Extract N expression variants from a single e-class by reading its page.
- **`DBGetNEclasses`**: Collect e-class ID sets per e-node variant.
- **`DBEClassTerminals`**: Walk all e-nodes in a class, collect unique terminals with cycle detection.
- **`DBDistribution`**: Pattern enumeration over top-N expressions (bounded N ≤ 10000).
- **`DBModularity`**: Find reusable sub-components across top-N expressions.
- **`DBCountPat`**: Structural pattern counting in top-N expressions.
- **`DBPatternMap`**: Show wildcard bindings for pattern matches in top-N expressions.
- **`DBExtractPat`**: Enumerate patterns in a single expression.
- **`DBDistTokens`**: Token frequency counting in top-N expressions.

### Pure Helper Functions

- `getAllPatternsOnTree :: Fix SRTree -> Map Pattern Int` — pattern enumeration on reconstructed trees.
- `getAllTokensOnTree :: Fix SRTree -> Map Pattern Int` — token frequency counting.
- `patternMatches` / `patternMatchesAny` — structural pattern matching with `isRoot` support.

### Schema & Infrastructure

- **`enode_parent` reverse index table**: Populated during import and eqsat write-through for all node types (EUni, EBin, ENAry). Enables SQL-native parent/ancestor walks.
- **`parentsOf` / `ancestorsOf`** query functions in `Query.hs` (recursive CTE for ancestor walks).
- **`topNIn`** query function for ranking a filtered set of eclass IDs.
- **`expandTreeFromDB`** in `Extract.hs` — walk `_best` tree through DB pages, collect all reachable eclass IDs.
- **`backfill-parents` CLI command** in `srtree-db` — populate `enode_parent` for pre-existing databases.

### Bug Fixes

- Fixed `isRoot` semantics inversion in `dbTopPatternCmd` (Py.hs).
- Fixed `negate` parameter being silently ignored in `DBTopPattern`.
- Fixed `DBOptimize` hardcoding `NLL Gaussian` — now accepts loss parameter.
- Fixed `pareto(byFitness=False)` ignoring `byFitness` in DB mode — added `byFitness` to `DBPareto` constructor.
- Fixed `save(fname)` and `load(fname)` to redirect to `persist()`/`loadDB()` in DB mode.
- Fixed `hlpMap` misalignment (36 entries for 41 commands) — rebuilt with all entries.
- Fixed multi-word pattern truncation in `dbTopPatternCmd` (`v0 * x0` → `v0`).
- Fixed `patChildrenOf` for `patternMatchesAny` — recursive sub-expression matching.
- Added `Data.Ord (Down(..))` import for sorting in bounded-N commands.
- Fixed CSV header in `DBPareto` to toggle between "Size" and "DL" based on `byFitness`.

### Cleanup

- Removed redundant `db*()` Python methods: `dbTop`, `dbPareto`, `dbDistribution`, `dbCount`, `dbEqSat`. Main methods now route to DB commands automatically.
- Removed dead constructors `Clean` and `GetEClassIds` from `Command` data type.
- Removed `expandTreeFromDB` from `Extract.hs` export list (duplicate with `Commands.hs` version).
- Marked unused `fitPath` parameters with `_` prefix in `DBSubtrees`, `DBGetNExprs`, `DBGetNEclasses`, `DBEClassTerminals`.

### Documentation

- **Tutorial 17**: DB-only session tutorial demonstrating all features.
- **AGENTS.md**: Updated committed state, marked 6 larger features as done.

## 2.2.1

- Pattern wildcard mapping + e-class terminals
- eggp performance overhaul (3x speedup with -N4)
- Profile-likelihood CI overhaul
- Split-DB architecture
- NaN propagation
- DB bloat fixes
