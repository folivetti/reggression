# `Reggression.jl` — Julia wrapper for reggression

**Status:** plan / RFC  
**Target:** Julia 1.10+ (LTS), Linux x86_64/aarch64, macOS x86_64/arm64, Windows x86_64  
**Upstream:** `reggression` 2.5.x (Haskell core + SQLite/Postgres storage)  
**Author:** planning draft

---

## 1. Goal

Provide a native Julia package, `Reggression.jl`, that exposes the
`reggression` query engine with Julia idioms:

```julia
using Reggression, DataFrames

egg = ReggressionSession("train.csv"; loss = :Gaussian, db = "egraph.db", fit_db = "fit_train.db")
top(egg, 10)
top(egg, 5; filters = ["size < 10", "parameters <= 3"], criteria = :fitness)
pareto(egg; by_fitness = true)
profile_data(egg, 42)
```

Non-goals:

- Reimplementing equality saturation, fitting, or SQLite storage in Julia.
- Supporting the removed legacy in-memory e-graph path in the Python package.
- A Julia clone of `eggp` (separate project; may reuse the same C ABI later).

The wrapper should be *thin*: command construction, FFI marshalling, CSV →
`DataFrame`, small typed helpers, plotting extension.

---

## 2. What the existing architecture gives us

Verified from the repository:

1. **One tiny FFI surface.** The Python package is a thin layer over
   `hs_reggression_run`:

   ```c
   char *hs_reggression_run(
       char *myCmd, char *dataset, char *testData, char *loss,
       char *loadFrom, char *dumpTo, char *parseCSV,
       int parseParams, int calcDL, int calcFit, char *varnames);
   ```

   Everything else (`top`, `eqsat`, `insert`, `profile-data`, ...) is a
   **DB-native command string** dispatched in `src/Py.hs` (`ReggressionDB`).
   No in-memory e-graph is loaded or dumped by the Python binding anymore.

2. **The current shared library is Python-specific.** `lib_binding.so`
   includes the SWIG wrapper (`binding_wrap.c`), which supplies
   `unsafe_py_write_stdout/stderr` using `PySys_FormatStdout/Stderr`, and has
   undefined `Py*` symbols. It is not safe to `dlopen` from Julia.

3. **The Haskell side needs no Python at all.** The only coupling is two
   `foreign import ccall` stubs in `src/Reggression.hs`:

   ```haskell
   foreign import ccall unsafe_py_write_stdout :: CString -> IO ()
   foreign import ccall unsafe_py_write_stderr :: CString -> IO ()
   ```

   A tiny C file with `fputs(stdout/stderr)` implementations can satisfy the
   same symbols. The exported `hs_reggression_*` functions are otherwise pure
   C ABI.

4. **All queries return strings.** Tabular commands return CSV; messages and
   profile/prediction data return plain text/CSV sections. Python parses them
   with `pandas.read_csv` or hand-rolled parsers.

5. **GHC RTS is process-global.** `hs_init`/`hs_exit` must happen once; all
   Haskell calls must be serialized; a second Haskell shared library loaded
   later is unsafe (the documented `eggp`/`reggression` multi-RTS issue).

This makes a direct `ccall` wrapper both feasible and small.

---

## 3. Architecture options

| Option | How | Pros | Cons | Verdict |
|---|---|---|---|---|
| **A. Native C ABI (`libreggression`)** | Add a second `foreign-library` target to `reggression.cabal` with a non-Python C shim; Julia `ccall`s `reggression_init/run/free`. | Fast, no Python runtime, small stable ABI, mirrors Python exactly, reusable by other languages. | Requires a Haskell build change and prebuilt binaries per platform. | **Recommended** |
| **B. `PythonCall.jl`** | Install `reggression` (Python wheel), call its API. | Zero Haskell work, full API on day one, plotting via matplotlib. | Requires Python + wheel; double runtime; pandas/matplotlib objects leak into the Julia API; slow; fragile in CI. | Good 1-day spike / fallback only. |
| **C. Subprocess CLI** | Spawn `reggression` with a command, read stdout. | Crash isolation; no in-process RTS. | CLI is an interactive in-memory REPL and does not expose the DB-native command set; process startup per query; awkward error/CSV capture. | Not recommended. |
| **D. Reimplement queries in Julia/SQL** | Read SQLite directly. | No Haskell dependency for read-only queries. | Duplicates fitness/pattern/eqsat semantics; impossible for `eqsat`, `insert`, CIs. | Reject. |

**Decision:** Option A. It keeps a single source of truth (the Haskell command
strings), gives the best runtime characteristics, and creates a reusable C ABI
that `EGGP.jl` or other tooling could also use.

---

## 4. Deliverables

Three artifacts, developed together:

1. **`reggression` Haskell package:** new `foreign-library reggression_capi`
   target + `src/reggression/capi.c` + optional safe-runner export. The
   artifact is `libreggression_capi.{so,dylib}` / `reggression_capi.dll`; it
   can be renamed to `libreggression` during packaging if desired.
2. **`Reggression.jl`:** the Julia package (this plan's main deliverable).
3. **`Reggression_jll` / artifact:** prebuilt, self-contained
   `libreggression_capi` for each supported platform, produced by the existing
   wheel CI matrix.

---

## 5. Haskell-side work (small)

### 5.1 New `capi.c`

A Python-free C shim that:

- satisfies the two symbols the Haskell code imports;
- exposes friendly `reggression_*` names;
- owns `init`/`exit`/`free` so Julia never needs GHC headers.

```c
#include <stdio.h>
#include <stdlib.h>

/* Symbols imported by src/Reggression.hs. */
void unsafe_py_write_stdout(char *s) { fputs(s, stdout); fflush(stdout); }
void unsafe_py_write_stderr(char *s) { fputs(s, stderr); fflush(stderr); }

/* GHC RTS entry points (declared manually to avoid RtsAPI.h in the C shim). */
extern void hs_init_with_rtsopts(int *argc, char **argv[]);
extern void hs_exit(void);

/* Generated by `foreign export ccall` in Reggression.hs. */
extern char *hs_reggression_version(void);
extern char *hs_reggression_run_safe(
    char *, char *, char *, char *, char *, char *,
    char *, int, int, int, char *, int *);

int reggression_init(int argc, char **argv) {
  hs_init_with_rtsopts(&argc, &argv);
  return 0;
}

void reggression_exit(void) { hs_exit(); }

const char *reggression_version(void) { return hs_reggression_version(); }

char *reggression_run(
    char *cmd, char *dataset, char *test, char *loss,
    char *load_from, char *dump_to, char *parse_csv,
    int parse_params, int calc_dl, int calc_fit, char *varnames,
    int *status) {
  return hs_reggression_run_safe(cmd, dataset, test, loss, load_from, dump_to,
                                 parse_csv, parse_params, calc_dl, calc_fit,
                                 varnames, status);
}

void reggression_free(char *s) { free(s); }
```

Notes:

- `hs_reggression_version` returns a `newCString` allocation. Either free it
  after the first copy in Julia or make the C shim cache it. Simplest: Julia
  calls it once, copies, and calls `reggression_free` on the returned pointer.
- `free` is correct because `newCString` uses `malloc`.
- On Windows, keep `reggression_free` in the same DLL/CRT as the allocation.

### 5.2 Safe runner in `Reggression.hs`

The current `hs_reggression_run` does **not** catch exceptions; an unexpected
Haskell exception crossing the FFI boundary terminates the process. Julia must
not be killable by a bad query.

Add (without changing the existing export used by Python):

```haskell
foreign export ccall hs_reggression_run_safe
  :: CString -> CString -> CString -> CString -> CString -> CString -> CString
  -> CInt -> CInt -> CInt -> CString -> Ptr CInt -> IO CString

hs_reggression_run_safe ... status = do
  r <- try (hs_reggression_run ...)   -- or a refactored pure worker
  case r of
    Right s -> poke status 0 >> pure s
    Left (e :: SomeException) ->
      -- Do not swallow Ctrl-C / async exceptions.
      case fromException e of
        Just (ec :: AsyncException) -> throwIO ec
        Nothing -> poke status 1 >> newCString (displayException e)
```

`try`/`fromException` need `Control.Exception`. Keep the existing
`hs_reggression_run` for the Python SWIG wrapper (or make it delegate).

### 5.3 Cabal target

```cabal
common reggression-core
  hs-source-dirs:   src
  other-modules:
    Reggression CLI Py Commands Util Paths_reggression
  autogen-modules:  Paths_reggression
  default-language: Haskell2010
  build-depends:    -- the current _binding dependency list, factored out
    ...

foreign-library _binding
  import: warnings, reggression-core
  type: native-shared
  c-sources: src/reggression/binding_wrap.c
  -- unchanged: Python wheel path

foreign-library reggression_capi
  import: warnings, reggression-core
  type: native-shared
  c-sources: src/reggression/capi.c
  ghc-options: -threaded -O2 -rtsopts -with-rtsopts=-F1.2
  if os(osx) || os(windows)
    options: standalone
```

Validation points:

- `nm -D --defined-only libreggression_capi.so | grep reggression_` shows the ABI.
- `nm -D -u libreggression_capi.so | grep -i py` is empty.
- `ldd libreggression_capi.so` has no `libHS*` entries when `standalone` works; if
  it does not on Linux, ship the Haskell closure in the artifact and set
  `RUNPATH=$ORIGIN`.
- Existing Python tests still pass (`_binding` unchanged).

### 5.4 RTS options (M0 finding)

GHC warns that `-rtsopts` and `-with-rtsopts` have **no effect** with
`-shared`; the RTS is initialised from the host's `argv` by
`hs_init_with_rtsopts`, but `+RTS ... -RTS` flags are not honoured in a
shared library. The M0 target therefore omits both flags and passes a bare
`["reggression"]` argv. If RTS tuning is needed, switch `reggression_init`
to `hs_init_ghc` with an explicit `RtsConfig` in a later milestone, or set
`GHCRTS` before the first Haskell call (the RTS reads it during init).

---

## 6. Julia package design

### 6.1 Layout

```
Reggression.jl/
├── Project.toml
├── Artifacts.toml              # or a Reggression_jll dependency
├── src/
│   ├── Reggression.jl          # module, exports, init
│   ├── library.jl              # dlopen, ccall, RTS lifecycle, free
│   ├── session.jl              # ReggressionSession struct, constructor, close
│   ├── query.jl                # top/pareto/distribution/report/...
│   ├── patterns.jl             # patternMap/countPattern/terminals/...
│   ├── storage.jl              # persist/load/import/insert/eqsat/fit
│   ├── profile.jl              # profile-data/prediction-data parsers
│   ├── parse.jl                # tolerant CSV → DataFrame helpers
│   └── validate.jl             # argument validation
├── ext/
│   └── ReggressionPlotsExt.jl  # Plots.jl recipes/plots
├── test/
│   ├── runtests.jl
│   ├── ffi.jl
│   ├── queries.jl
│   ├── patterns.jl
│   ├── storage.jl
│   ├── profile.jl
│   ├── plots.jl
│   ├── errors.jl
│   └── data/                   # small CSV/SQLite fixtures
└── docs/
    ├── make.jl
    └── src/{index.md, tutorial/*.md}
```

### 6.2 Dependencies

| Dependency | Why |
|---|---|
| `Libdl` (stdlib) | `dlopen`, `dlsym`, `Libc.free` fallback |
| `CSV` | parse command output |
| `DataFrames` | tabular API (pandas analog) |
| `Tables` | optional interop |
| `PrecompileTools` | precompile common query paths |
| `Plots` (weakdep) | plotting extension |
| `Reggression_jll` or `Artifacts.toml` | prebuilt native library |

No `PythonCall`/`PyCall` in the core.

### 6.3 Session type

The package/module is `Reggression`, so the session type cannot also be named
`Reggression` (inside a module, `Reggression` always resolves to the module
binding; `Reggression(...)` would try to call the module). Name the type
`ReggressionSession` and export it. If a shorter spelling is desired, add a
lowercase constructor alias `reggression(...)`; do not shadow the module name.

```julia
mutable struct ReggressionSession
    dataset::String
    test_data::String
    loss::Symbol                 # :MSE, :LOG10, ..., :Pinball
    pinball_tau::Float64
    load_from::String
    parse_csv::String
    parse_params::Bool
    refit::Bool
    simple_output::Bool
    dataset_name::String
    db::String
    fit_db::String
    varnames::Vector{String}
    owns_db::Bool
end
```

Constructor mirrors the Python signature:

```julia
ReggressionSession(dataset;
    test_data = "", loss = :MSE, load_from = "", parse_csv = "",
    parse_params = true, refit = false, simple_output = false,
    dataset_name = "", db = "", fit_db = "", pinball_tau = 0.5,
    rtsopts = String[])
```

Behavior:

- Validate `dataset` exists and `loss` is known; validate `pinball_tau`.
- If `db == ""`, create a temp SQLite file in `pwd()` (same as Python) and
  mark `owns_db = true`. SQLite `:memory:` is not viable because each command
  opens its own connection.
- Derive `dataset_name = splitext(basename(dataset))[1]` when empty.
- Read the header via `CSV.File(dataset)` (or `Tables.columnnames`) to build
  `varnames`, matching pandas' `df.columns`.
- `Base.close(reg)` and a finalizer remove the temp DB; never remove a
  user-provided DB.

`loss_arg(reg)` returns `"MSE"`, `"Gaussian"`, ..., or `"Pinball 0.75"`.

### 6.4 FFI layer

```julia
const _rts_lock = ReentrantLock()
const _rts_ready = Ref(false)
const _lib = Ref{String}("")

function _library()
    isempty(_lib[]) || return _lib[]
    p = get(ENV, "REGRESSION_LIB", "")
    isempty(p) && (p = Reggression_jll.libreggression_capi)  # or artifact path
    _lib[] = p
end

function init(; rtsopts = String[])
    lock(_rts_lock) do
        _rts_ready[] && return
        # M0: pass a bare argv (see §5.4: -rtsopts is ignored for -shared).
        argv = ["reggression"]
        # allocate NUL-terminated char**
        GC.@preserve argv begin
            cargv = [Base.unsafe_convert(Cstring, a) for a in argv]
            push!(cargv, Ptr{UInt8}(0))
            ccall((:reggression_init, _library()), Cint,
                  (Cint, Ptr{Ptr{UInt8}}), length(argv), cargv)
        end
        atexit(_shutdown)
        _rts_ready[] = true
    end
end
```

Run:

```julia
function run_raw(reg::ReggressionSession, query::AbstractString)::String
    init()
    lock(_rts_lock) do
        status = Ref{Cint}(0)
        ptr = ccall((:reggression_run, _library()), Ptr{UInt8},
            (Cstring, Cstring, Cstring, Cstring, Cstring, Cstring, Cstring,
             Cint, Cint, Cint, Cstring, Ptr{Cint}),
            query, reg.dataset, reg.test_data, loss_arg(reg), "", "",
            reg.parse_csv, Cint(reg.parse_params), 0, 0,
            join(reg.varnames, ","), status)
        ptr == C_NULL && error("reggression: null result")
        s = unsafe_string(ptr)
        ccall((:reggression_free, _library()), Cvoid, (Ptr{UInt8},), ptr)
        status[] == 0 || throw(ReggressionError(s))
        s
    end
end
```

Design notes:

- **One global lock.** GHC RTS calls are not safe concurrently from arbitrary
  Julia threads unless serialized. Long `eqsat` calls block other reggression
  tasks; document this and offer `run_raw_async` using `Threads.@spawn` plus
  the same lock.
- **`unsafe` vs normal `ccall`.** Use a normal `ccall`; Julia can schedule
  other tasks while the calling task is blocked. Do not mark it `@ccall` with
  `unsafe` semantics unless measured.
- **Memory.** Every result is copied with `unsafe_string` and freed through
  `reggression_free` (never `Libc.free` directly) to avoid cross-CRT issues on
  Windows.
- **Init once.** Guard with `_rts_ready`; register `atexit(_shutdown)`.
- **Peer Haskell libraries.** If another Julia package embeds a GHC RTS
  (`EGGP.jl` in the future), they must share one RTS. Document a
  `Reggression.init()`-first convention and expose `Reggression.rts_ready()`.

### 6.5 Query construction

Keep command construction identical to `src/reggression/__init__.py` so the
Haskell side stays the single source of truth.

```julia
_db_spec(reg) = isempty(reg.fit_db) ? reg.db : "$(reg.db):$(reg.fit_db)"

function top(reg, n = 5; filters = String[], criteria = :fitness,
             pattern = "", root = false, negate = false,
             ci = false, data = nothing)
    criteria in (:fitness, :dl) || throw(ArgumentError(...))
    if isempty(pattern)
        cistr  = ci ? " with ci" : ""
        datastr = ci ? " data $(something(data, reg.dataset))" : ""
        filt   = isempty(filters) ? "" :
                 " " * join(("[$f]" for f in filters), " ")
        query = "top $(_db_spec(reg)) $(reg.dataset_name) $n$filt$cistr$datastr"
        return run_query(reg, query)
    else
        rootstr = root ? " root" : ""
        notstr  = negate ? " not" : ""
        cistr   = ci ? " ci" : ""
        datastr = ci ? " data $(something(data, reg.dataset))" : ""
        query = "top-pattern $(_db_spec(reg)) $(reg.dataset_name) $n " *
                "$pattern$rootstr$notstr$cistr$datastr"
        return run_query(reg, query)
    end
end
```

`run_query(reg, query; dataframe = true)`:

- `dataframe = false` → return the raw string (messages, profile data).
- empty/whitespace output → empty `DataFrame`.
- otherwise `CSV.read(IOBuffer(s), DataFrame; copycols = false)`.
- apply `simple_output` post-filter (`Id`, `Latex`, `Fitness`) when the
  columns exist, mirroring Python.

---

## 7. API mapping (Python → Julia)

| Python | Julia | Return |
|---|---|---|
| `Reggression(...)` | `ReggressionSession(...)` | session |
| `runQuery(q, df)` | `run_query(reg, q; dataframe=df)` | `DataFrame` / `String` |
| `top(...)` | `top(reg, n; ...)` | `DataFrame` |
| `pareto(...)` | `pareto(reg; ...)` | `DataFrame` |
| `distribution(...)` | `distribution(reg; ...)` | `DataFrame` |
| `modularity(n, ...)` | `modularity(reg, n; ...)` | `DataFrame` |
| `countPattern(p)` | `count_pattern(reg, p)` | `String` (or `parse_count`) |
| `report(n; ci)` | `report(reg, n; ci)` | `String` (ragged report; optional `parse_report`) |
| `optimize(n; ci)` | `optimize(reg, n; ci)` | `String` |
| `eqsat(n)` | `eqsat(reg, n=1)` | `String` |
| `getNExpressions(eid, n)` | `get_nexpressions(reg, eid, n=10)` | `DataFrame` (one `Expression` column) |
| `getNEclasses(eid, n)` | `get_neclasses(reg, eid, n=10)` | `Vector{Vector{Int}}` |
| `subtrees(n)` | `subtrees(reg, n)` | `Vector{Int}` |
| `insert(expr, alg)` | `insert(reg, expr; alg=:TIR)` | `String` |
| `extractPattern(eid)` | `extract_pattern(reg, eid)` | `DataFrame` |
| `distributionOfTokens(top)` | `distribution_of_tokens(reg; top=-1)` | `DataFrame` |
| `patternMap(p, limit)` | `pattern_map(reg, p; limit=-1)` | `DataFrame` |
| `eclassTerminals(eid)` | `eclass_terminals(reg, eid)` | `DataFrame` |
| `getENodeInfo(eid)` | `get_enode_info(reg, eid)` | `DataFrame` |
| `persist(fname, fitDb)` | `persist!(reg; fname, fit_db)` | `String` |
| `loadDB(fname, fitDb)` | `load!(reg; fname, fit_db)` | `String` |
| `importDB(eqs, ...)` | `import!(reg, eqs; ...)` | `String` |
| `importFromCSV(fname, ...)` | `import_from_csv!(reg, fname; ...)` | `String` |
| `dbStream(...)` | `stream(reg; ...)` | `String` |
| `dbPushFit(...)` | `push_fit!(reg; ...)` | `String` |
| `dbRefreshFitness(...)` | `refresh_fitness!(reg; ...)` | `String` |
| `dbEqSatFrontier(...)` | `eqsat_frontier(reg; ...)` | `String` |
| `dbInsert(...)` | `db_insert!(reg, expr; ...)` | `InsertResult(eid, existed)` |
| `dbSetFit(...)` | `set_fit!(reg; eid, fitness, ...)` | `String` |
| `patternInstances(...)` | `pattern_instances(reg; ...)` | `DataFrame` |
| `profilePlot(...)` | `profile_plot(reg, eid; ...)` | `Plots.Plot` (ext) |
| `predictionPlot(...)` | `prediction_plot(reg, eid; ...)` | `Plots.Plot` (ext) |
| `predictionData(...)` | `prediction_data(reg, eid; ...)` | `DataFrame` |
| `set_simple_output(b)` | `set_simple_output!(reg, b)` | `reg` |
| `set_varnames(names)` | `set_varnames!(reg, names)` | `reg` |
| `version()` | `version()` | `VersionNumber`/`String` |

Also expose the low-level `run_raw(reg, query)` for forward compatibility:
new Haskell commands work in Julia before a typed wrapper exists.

---

## 8. Parsing details

### 8.1 Output kinds

The command output is not uniformly CSV. The typed wrappers should branch on
the output kind instead of forcing everything through one parser:

| Kind | Commands | Parser |
|---|---|---|
| Regular table | `top`, `pareto`, `distribution`, `modularity`, `pattern-map`, `eclass-terminals`, `getENodeInfo`, `distribution-tokens`, `extract-pattern`, `pattern-instances` | `CSV.read(IOBuffer(s), DataFrame)` |
| Single-column table | `getNExprs` (`Expression`) | `CSV.read` or split lines |
| ID list | `subtrees` (`1,2,3`) | `parse.(Int, split(s, ','))` |
| ID-set list | `getNEclasses` (`EClassIds` header + comma-separated rows) | `Vector{Vector{Int}}` |
| Message / ragged report | `report`, `optimize`, `insert`, `eqsat*`, `persist`, `load`, `import`, `stream`, `push-fit`, `refresh-fitness` | return `String`; `report` may get a `parse_report` helper |
| Human-readable count | `count-pattern` (`pattern '...' matched N of M expressions`) | `String`; optional regex → `NamedTuple` |
| Sectioned data | `profile-data`, `prediction-data` | dedicated parser (Section 8.2) |

For regular tables:

- Haskell emits mostly unquoted CSV (e.g. `Id,Expression,Fitness`); some
  commands (notably `pattern-instances`) quote strings.
- Use `missingstring = [""]` if empty cells should be `missing`;
- `stringtype = String` for expression columns;
- post-process `Name`/`Symbol` columns to `String` as Python does for
  `eclassTerminals`.

Because many fields are unquoted, a comma inside a LaTeX/expression field
would break both Python and Julia. Add a hardening item in the Haskell package
(quote CSV fields) but keep the wrapper compatible with the current output.

`run_query(reg, q; dataframe=true)` should be documented as a convenience for
regular tables; the typed wrappers use `run_raw` for the irregular kinds and
parse explicitly.

### 8.2 `profile-data`

Sections are blank-line separated:

```
param,tau,theta_0,...,theta_k,opt
...
i,j,theta_i,theta_j
...
method,profile|laplace
x0,yhat,lower,upper
...
```

Expose a typed result:

```julia
struct ProfileData
    profiles::Dict{Int, NamedTuple{(:taus, :thetas, :opt), Tuple{Vector{Float64}, Vector{Vector{Float64}}, Float64}}}
    contours::Dict{Tuple{Int,Int}, NamedTuple{(:theta_i, :theta_j), Tuple{Vector{Float64}, Vector{Float64}}}}
    prediction::Union{Nothing, NamedTuple{(:method, :x0, :yhat, :lower, :upper), ...}}
    raw::String
end
```

`profile_data(reg, eid; data = nothing, data_spec = nothing)`:
- run `profile-data` with `dataframe = false`;
- detect the same error prefixes as Python (`"theta has fewer"`, `"The id"`,
  `"profiling failed"`, ...) and throw `ReggressionError`;
- parse sections by header rather than by position.

`prediction_data` returns a `DataFrame` with `x0, yhat, lower, upper` and a
`method` attribute (Julia: a named tuple/`DataFrame` metadata; or a
`prediction_method` column). Mirror Python's empty-result behavior.

### 8.3 Plotting extension

`ReggressionPlotsExt` (weakdep `Plots`):

- `profile_plot(reg, eid; save_to = nothing)`: one tau-vs-theta panel per
  parameter, then one square contour panel per `(i, j)` pair, matching the
  Python `profilePlot` layout.
- `prediction_plot(reg, eid; save_to = nothing)`: `yhat` line + shaded 95%
  prediction band.
- Return a `Plots.Plot`, not a file, unless `save_to` is given.
- Keep `profile_data`/`prediction_data` in core so Makie users are not
  blocked on a Plots dependency.

---

## 9. Lifecycle, errors, concurrency

### 9.1 RTS lifecycle

- `init()` is idempotent and serialized by `ReentrantLock`.
- `atexit(_shutdown)` calls `reggression_exit()` once.
- Expose `Reggression.shutdown()` for tests that need to restart the RTS
  (though GHC does not support re-initialization reliably; tests should avoid
  it).
- Never call `hs_exit` while another Haskell library is still using the RTS.
- Document Ctrl-C behavior: Haskell installs signal handlers; test
  `SIGINT` during `eqsat` and, if needed, start the RTS with the relevant
  `-V`/signal options or isolate long jobs.

### 9.2 Error mapping

```julia
struct ReggressionError <: Exception
    msg::String
end
Base.showerror(io, e::ReggressionError) = print(io, "reggression: ", e.msg)
```

- `status != 0` from `reggression_run` → `ReggressionError`.
- Known message strings that Haskell returns as *success* with a text body
  (e.g. `"The id must be an integer."`) are detected in the typed wrappers
  and raised as `ArgumentError` or `ReggressionError`.
- Invalid Julia arguments are caught before the FFI call.

### 9.3 Concurrency

- A single global lock is the safe default.
- Add `run_raw_async` using `Threads.@spawn` for callers who want to overlap
  Julia work with a long DB query; the lock still serializes Haskell calls.
- Document that a single `ReggressionSession` is not safe to use from
  multiple threads without the wrapper's lock.

### 9.4 Multiple Haskell runtimes

The known GHC limitation (one RTS per process; late `dlopen` of another
Haskell library can crash) applies. Plan:

- Only one wrapper embeds the RTS in a given process.
- If `EGGP.jl` is added later, both packages must share the same
  `libreggression_capi`/RTS or preload each other's libraries through a shared
  `HaskellRTS.jl` helper.
- Add an integration test that loads `libreggression_capi` after Julia has already
  started and runs a query (the normal case), and document the multi-library
  constraint.

---

## 10. Packaging & distribution

### 10.1 Local development

1. Build the C ABI library from the sibling repo:

   ```bash
   cd reggression
   cabal build reggression:foreign-library:reggression_capi
   ```

2. Point Julia at it:

   ```bash
   REGRESSION_LIB=/path/to/libreggression_capi.so julia --project -e 'using Reggression; ...'
   ```

3. Fallback: `deps/build.jl` runs `cabal build` if GHC is available and the
   sibling checkout exists. This is for developers only, never for the
   registry.

### 10.2 Prebuilt artifacts (recommended)

Reuse the **existing Python wheel build matrix**, which already solves the
hard part (GHC + `libnlopt` + `libpq`/OpenSSL + `zlib` + Haskell shared-lib
closure + `auditwheel`/`delocate`/`delvewheel`):

- Add the `reggression_capi` target to the same CI job that builds the wheel.
- Package the repaired `libreggression_capi.{so,dylib,dll}` (plus its RPATH-bundled
  dependency directory) as a per-platform tarball.
- Attach tarballs to the GitHub release / PyPI release.
- `Reggression.jl` downloads them via `Artifacts.toml` (SHA256 pinned) or a
  generated `Reggression_jll`.

This avoids reimplementing the Haskell toolchain in BinaryBuilder and keeps
the Python and Julia binaries in lockstep.

### 10.3 `Reggression_jll` (for the General registry)

- Generate a JLL from the release tarballs (JLLGenerator or hand-written).
- Product: `libreggression_capi` (and, if needed, the bundled dependency dir).
- Add `Reggression_jll` as a normal dependency; keep
  `REGRESSION_LIB` as an override for local/patched builds.
- Test `Pkg.add("Reggression")` on a clean machine for each supported
  platform.

### 10.4 Versioning

- `Reggression.jl` tracks the upstream `reggression` minor version
  (e.g. `0.1.0` for `reggression 2.5.x`), with a `compat` entry on the JLL.
- `version()` calls `reggression_version()` and warns (or errors) if the
  native version's major/minor differs from the Julia package's expectation.
- Provide `Reggression.native_version()`.

---

## 11. Testing strategy

### 11.1 Unit tests (no native lib)

- loss/argument validation;
- command-string construction for every typed wrapper (compare against the
  exact strings Python builds);
- profile/prediction section parsing on recorded fixtures;
- CSV edge cases (empty result, message body, missing columns);
- `simple_output` filtering;
- temp-DB lifecycle (`close`, finalizer).

### 11.2 FFI smoke tests

```julia
@test Reggression.native_version() isa VersionNumber
@test occursin("Id,Expression", run_raw(reg, "top ..."))
@test_throws ReggressionError run_raw(reg, "top /nonexistent.db ds 5")
```

Check:

- `reggression_init` idempotent;
- returned strings freed (run 10k queries, assert RSS is flat-ish);
- `status` path returns an error instead of killing the process;
- UTF-8 dataset/expression names.

### 11.3 Integration / golden tests

- Generate a small DB with `persist!`/`import!` from the tutorial datasets.
- Run `top`, `pareto`, `distribution`, `modularity`, `pattern_map`,
  `eclass_terminals`, `get_enode_info`, `profile_data`, `prediction_data`.
- Compare against golden CSV/text captured from the Python package at the
  same reggression version.
- Store fixtures in `test/data/` (small: a few hundred KB of CSV/SQLite).

### 11.4 Plot tests

- Load the extension (`using Plots`), build both plots, save to a temp PNG,
  assert the file is non-empty and the parsed `profile_data` has the expected
  number of parameters/contours.
- Keep plots out of the default test run if `Plots` is slow; use a test
  group / `JULIA_REGRESSION_PLOTS=1`.

### 11.5 CI matrix

| OS | Arch | Julia | Native lib |
|---|---|---|---|
| Ubuntu 22.04 | x86_64 | 1.10, 1.11 | release artifact |
| Ubuntu 24.04 | aarch64 | 1.11 | release artifact |
| macOS 14 | arm64 | 1.10, 1.11 | release artifact |
| macOS 13 | x86_64 | 1.10 | release artifact |
| Windows Server 2022 | x86_64 | 1.10, 1.11 | release artifact |

Plus a job that builds the Haskell C ABI from source on Linux to catch
upstream breakage.

### 11.6 Upstream compatibility test

A scheduled job checks out `reggression` `main`, builds `reggression_capi`,
and runs the Julia test suite against it. Failures flag ABI/command drift.

---

## 12. Documentation & tutorials

- `README.md`: install, 5-line quick start, session modes (temp DB, existing
  DB, split DB), API table.
- `docs/src/index.md`: full API reference with docstrings.
- Port the Python tutorials to Julia (at least):
  - 01 create/insert,
  - 02 top with filters,
  - 03 pattern matching,
  - 04 building blocks,
  - 05 import from CSV,
  - 08 persist and SQL queries,
  - 09 eqsat on DB,
  - 12 DB ingest,
  - 13 profile CI,
  - 15 split DB,
  - 16 pattern map/terminals,
  - 17 DB-only session,
  - 18 profile plots.
- Document the Python ↔ Julia naming differences and the `run_raw` escape
  hatch.
- Add a "coming from Python" section (constructor kwargs, loss symbols,
  DataFrame columns, plotting).

---

## 13. Milestones

| Milestone | Deliverable | Acceptance | Effort |
|---|---|---|---|
| **M0: FFI spike** | `reggression_capi` target + `capi.c`; Julia script dlopens it and runs `version()` + `top` | No Python symbols in `nm -u`; query returns CSV; Python tests still pass | 0.5–1 d |
| **M1: Minimal package** | `Project.toml`, `ReggressionSession` struct, `init`, `run_raw`, `run_query`, `top`, `persist!`, `version` | Tests pass on Linux against local build | 1–2 d |
| **M2: API parity** | All query/pattern/storage methods from the mapping table | Feature matrix complete; golden tests vs Python | 2–3 d |
| **M3: Profiles + plots** | `ProfileData` parser, `prediction_data`, Plots extension | Tutorial 13/18 equivalents produce correct plots | 1–2 d |
| **M4: Packaging** | CI builds artifacts for all platforms; `Artifacts.toml`/JLL; `deps/build.jl` | Clean-machine `Pkg.add` works on Linux/macOS/Windows | 2–4 d |
| **M5: Hardening** | Safe runner, error mapping, RTS lifecycle, thread/memory tests | Malformed query returns `ReggressionError`; no process death; RSS stable | 1 d |
| **M6: Docs + release** | Docs, tutorials, registry metadata, v0.1.0 tag | Docs build; package installs from registry | 1 d |

Total: roughly **8–14 working days** for one developer familiar with both
sides, with M0 and M4 carrying most of the uncertainty.

---

## 14. Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| GHC RTS signal/thread interactions with Julia | Crashes or Ctrl-C misbehavior | Serialize with one lock; test SIGINT during `eqsat`; document; consider a dedicated worker thread |
| Haskell exception crossing FFI kills the process | Julia session loss | `hs_reggression_run_safe` + `status` out-param (M0/M5) |
| Native dependency closure (`libHS*`, `libpq`, OpenSSL, nlopt, zlib) | Hard to ship | Reuse wheel CI + `auditwheel`/`delocate`/`delvewheel`; `standalone`; RPATH to `$ORIGIN`; JLL |
| Unquoted CSV fields | Parse errors on unusual expressions | Golden tests; harden Haskell CSV quoting; tolerant fallback parser |
| API drift Python ↔ Julia | Missing/broken methods | Single command-string builder shared by design; parity test table; scheduled upstream build |
| Long queries block Julia tasks | Poor UX | Document; `run_raw_async`; keep the lock coarse but clear |
| Second Haskell library in the process | `ARR_WORDS`/segfault | Shared-RTS convention; `HaskellRTS.jl` helper later; integration test |
| Windows CRT/`free` mismatch | Heap corruption | `reggression_free` in the same DLL; avoid `Libc.free` on Haskell pointers |
| Julia/Python DataFrame semantics differ | Confusing results | Document dtypes/`missing`; post-process string columns like Python |

---

## 15. Open questions / decisions

1. **Package/type names:** package `Reggression.jl`, module `Reggression`,
   session type `ReggressionSession` (recommended; avoids the
   module/type name collision) vs a package named `Regression.jl` (conflicts
   with the existing Julia `Regression.jl` / GLM ecosystem — avoid). A
   lowercase constructor alias `reggression(...)` can be provided.
2. **Minimum Julia version:** 1.10 LTS (recommended) or 1.11.
3. **Distribution:** artifact download + `deps/build.jl` first, JLL later
   (recommended), or JLL from day one.
4. **Plotting backend:** `Plots.jl` extension first (recommended, closest to
   matplotlib's ubiquity), optional Makie extension later.
5. **API style:** Julia snake_case + `!` mutators (recommended) vs exact
   Python camelCase names for copy-paste parity. Could provide `@deprecate`
   aliases for the most-used methods (`getNExpressions`, `patternMap`,
   `eclassTerminals`).
6. **`refit`/`calcDL`/`calcFit`:** the current DB-native path passes zeros;
   confirm these are dead in DB-only mode and either expose them later or
   document them as unsupported.
7. **Legacy `.egraph` binary:** Python's `loadFrom` is still accepted but the
   DB-native path ignores it (`reggressionDB`). Confirm the intended Julia
   behavior: reject with a clear message or implement via a Haskell helper.
8. **C ABI ownership:** prefer a Haskell-exported `reggression_free` or rely
   on the C shim's `free` (recommended: C shim, one allocator boundary).

---

## 16. First concrete step (M0)

```bash
# 1. Add the target and capi.c (Section 5), then:
cd reggression
cabal build reggression:foreign-library:reggression_capi
SO=$(cabal list-bin reggression:foreign-library:reggression_capi)

# 2. Verify the ABI is Python-free:
nm -D -u "$SO" | grep -i py && echo "FAIL: Python symbols" || echo "OK"
nm -D --defined-only "$SO" | grep reggression_

# 3. Smoke-test from Julia:
REGRESSION_LIB="$SO" julia -e '
using Libdl
lib = dlopen(ENV["REGRESSION_LIB"])
@show dlsym(lib, :reggression_version)
# then call reggression_init / reggression_run with a tiny temp DB
'
```

If M0 succeeds, the rest is mostly mechanical API coverage and packaging.
If `standalone`/dependency closure turns out to be the blocker, fall back to
shipping the already-repaired wheel's dependency closure alongside the C ABI
library (Section 10.2).

---

## 17. M0 implementation status

Implemented on branch `feature/julia-capi`:

- `src/reggression/capi.c` — Python-free stdio shim + `reggression_*` ABI.
- `src/reggression/reggression_capi.def` — Windows exports.
- `reggression.cabal` — shared `reggression-lib` common stanza and the
  `reggression_capi` foreign-library target.
- `src/Reggression.hs` — `hs_reggression_run_safe` (status out-param,
  synchronous exceptions returned as text; async exceptions rethrown).
- `scripts/julia_capi_smoke.jl` — Julia `dlopen`/`ccall` smoke test.

Verified:

```bash
cabal build reggression:foreign-library:reggression_capi
cabal build reggression:foreign-library:_binding
SO=$(cabal list-bin reggression:foreign-library:reggression_capi)
julia scripts/julia_capi_smoke.jl "$SO" test/sine.csv
```

The smoke test exercises `reggression_version`, `insert`, `set-fit`, `top`,
and a deliberately throwing command (`top` against a missing DB directory),
which returns status 1 instead of terminating Julia. `nm -D -u` shows no
Python symbols and `ldd` shows no libpython. The existing `_binding` target
still builds and a ctypes init/version/exit smoke test passes.

## 18. M1 implementation status

The minimal Julia package lives in the sibling repository
`Reggression.jl/`:

- `Project.toml` (CSV, DataFrames, Libdl; Julia 1.10+).
- `src/library.jl` — `dlopen`/`dlsym`, one-time RTS init, `run_raw`,
  `native_version`, `ReggressionError`, library auto-discovery.
- `src/session.jl` — `ReggressionSession`, dataset-header/varnames handling,
  temp-DB lifecycle, `close`.
- `src/query.jl` — `top`, `run_query`, `db_insert!`, `set_fit!`, `persist!`
  (file-level DB snapshot), `set_simple_output!`, `set_varnames!`.
  `db_insert!` calls the native `insert-check` command and returns
  `InsertResult(eid, existed)`, so insertion also answers "was this
  expression already structurally present?" without fitting.
- `test/runtests.jl` — 32 passing tests covering version, insert/top,
  filters, patterns, simple output, error status, snapshot persistence,
  temporary DB cleanup.

`db_insert!` is used instead of `insert!` because `insert!` collides with
`Base.insert!`. The native `insert-check` command is implemented in
`src/Commands.hs` (`DBInsertCheck`) and shares `dbInsertImpl` with `insert`;
it reports existence from the pre-insert e-class watermark, so it does not
depend on `expression_index` being populated. Auto-discovery finds the newest sibling
`dist-newstyle/.../reggression_capi/libreggression_capi.so`; `REGRESSION_LIB`
and `Reggression.set_library!` override it.

Additional native commands/wrappers added for the eqsat → fitted-DB workflow:

- `canonical-eid` / `canonical_eid` — resolve merged eids after eqsat.
- `canonical-expressions` / `all_expressions` — enumerate canonical roots as
  `Id,Expression,Size,Key`.
- `fitted-expressions` / `fitness_lookup` — enumerate fitted classes as
  `Id,Expression,Fitness,Key` and build a `Key => Fitness` dictionary.
- `eqsat-frontier` / `eqsat_frontier`.
- `getNExprs` / `get_nexpressions`.
- `report`.

`Key` is a commutative/associative structural key (`relabelParams`, then
flatten+sort `Add`/`Mul` operands), so it can join independently built e-graph
DBs even when eids and operand order differ. The key remains structural (not a
complete semantic normal form); merging the DBs via `db_insert!` + `set_fit!`
is the sound fallback.
