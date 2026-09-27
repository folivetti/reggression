# Python tutorials

Run each script from this `tutorials/` directory (some reference the
`datasets/` folder or create a temporary synthetic dataset).

## Getting started
- `01_creating_egraph.py` — create an e-graph from a dataset.
- `02_retrieving_top_expressions.py` — `top` / filters.
- `03_retrieving_top_expressions_with_pattern_matching.py` — pattern matching.
- `04_playing_with_building_blocks.py` — building blocks API.
- `05_building_from_csv.py` — import candidate expressions from CSV.
- `06_starting_from_nothing.py` — empty e-graph, `insert`, in-memory `eqsat`, equivalence checks.

## In-memory equality saturation
- `07_eqsat_in_memory.py` — build a small e-graph, run `eqsat` in memory,
  extract with `top` / `pareto`, and check equivalence. (Mirrors
  `egraph-inmemory.hs`.)

## SQL-backed persistence and out-of-core eqsat (srtree-db)
- `08_persist_and_sql_queries.py` — `persist` an e-graph to SQLite, then answer
  `dbTop` / `dbCount` / `dbPareto` / `dbDistribution` *directly in SQL*, with no
  in-memory pattern enumeration. (Mirrors `egraph-db.hs`.)
- `09_eqsat_on_database.py` — **capability test** for the out-of-core path.
  Builds a large e-graph (~tens of thousands of classes), then compares the
  incremental peak RSS of *materializing it in memory* vs *running `dbEqSat`
  (out-of-core) + `dbTop`* against the persisted SQLite DB. Asserts the
  out-of-core operation uses strictly less memory (bounded by the page cache,
  independent of graph size) while producing an equivalent result. (Mirrors
  `egraph-db.hs` lazy-rewrite + `egraph-db-stream.hs`.)
- `10_scale_dbEqSat.py` — **scalability example**: seeds a ~500k-class e-graph
  with `importFromCSV` (the one-time in-memory load of the raw equations),
  `persist`s it, then runs `dbEqSat` (out-of-core) which performs the **full**
  equality saturation against the persisted, lazily-loaded graph. Shows the
  operation streams through a bounded page cache (incremental peak RSS ~0 MB,
  independent of the half-million class count). No in-memory `eqsat` is run first. (No in-memory eqsat comparison at this scale.)

## eggp integration and DB mode
- `11_eggp_db.py` — **DB-backed eggp loop**: `dbInsert`, `dbSetFit`,
  `dbEqSatFrontier`, and resume from a persisted database. Shows the
  out-of-core search pattern: insert → setFit → frontier eqsat → query.
- `12_db_ingest.py` — **srtree-db CLI workflow**: ingest expressions and fit
  them to a dataset using the `srtree-db` CLI (`ingest` + `fitdata`).

## Profile-likelihood confidence intervals
- `13_profile_ci.py` — **profile-likelihood CIs**: query top expressions with
  confidence interval columns for each fitted parameter.

## Loading and querying pre-existing databases
- `14_load_existing_db.py` — **load and resume**: load a persisted DB, query
  it, insert new expressions, re-saturate, and persist back.

## Split-DB architecture
- `15_split_db_refit_status.py` — **split-DB refit and status**: the egraph is
  dataset-agnostic; fitness lives in per-dataset fit DBs. Shows refit (clear
  and re-fit with different parameters), status queries, and multiple datasets
  sharing one egraph.

See also the Haskell equivalents `egraph-inmemory.hs`, `egraph-db.hs` and
`egraph-db-stream.hs` in this folder, and the design note in
`srtree-db/README.md`.
