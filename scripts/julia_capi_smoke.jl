#!/usr/bin/env julia
#
# M0 smoke test for the Python-free reggression C ABI.
#
# Usage:
#   julia scripts/julia_capi_smoke.jl /path/to/libreggression_capi.so [dataset.csv]
#
# The dataset path is only used to derive the variable names passed to the
# Haskell printer; `insert`/`top` operate on the SQLite database built in a
# temporary directory.

using Libdl

const LIB = let
    p = isempty(ARGS) ? get(ENV, "REGRESSION_LIB", "") : ARGS[1]
    isempty(p) && error("usage: julia julia_capi_smoke.jl <libreggression_capi.so> [dataset.csv]")
    p
end

const DATASET = length(ARGS) >= 2 ? ARGS[2] :
    joinpath(@__DIR__, "..", "test", "sine.csv")

const PTR = Ptr{UInt8}

"""Call a `char *`-returning ABI function and copy/free the result."""
function take_string(ptr::Ptr{UInt8})
    ptr == C_NULL && error("reggression returned a null pointer")
    s = unsafe_string(ptr)
    ccall((:reggression_free, LIB), Cvoid, (Ptr{UInt8},), ptr)
    return s
end

"""Run one DB-native command. Returns (status, output)."""
function run_cmd(cmd, dataset, varnames;
                 loss = "MSE", parse_params = 0)
    status = Ref{Cint}(0)
    ptr = ccall((:reggression_run, LIB), Ptr{UInt8},
        (Cstring, Cstring, Cstring, Cstring, Cstring, Cstring, Cstring,
         Cint, Cint, Cint, Cstring, Ptr{Cint}),
        cmd, dataset, "", loss, "", "", "",
        Cint(parse_params), Cint(0), Cint(0), varnames, status)
    return (Int(status[]), take_string(ptr))
end

function main()
    dlopen(LIB)  # fail early if the library cannot be loaded

    # Initialise the GHC RTS once. No +RTS options: -rtsopts has no effect in
    # a shared library (GHC warns about this); use hs_init_ghc later if RTS
    # tuning is needed.
    argv = String["reggression"]
    ptrs = Vector{Ptr{UInt8}}(undef, length(argv) + 1)
    GC.@preserve argv ptrs begin
        for (i, a) in enumerate(argv)
            ptrs[i] = Base.unsafe_convert(Cstring, a)
        end
        ptrs[end] = C_NULL
        ccall((:reggression_init, LIB), Cint,
              (Cint, Ptr{Ptr{UInt8}}), length(argv), ptrs)
    end

    ver = take_string(ccall((:reggression_version, LIB), Ptr{UInt8}, ()))
    println("reggression version: ", ver)

    header = first(eachline(DATASET))
    varnames = join(strip.(split(header, ',')), ",")
    dsname = "smoke"
    db = joinpath(mktempdir(), "smoke.db")

    # 1. Insert an expression; the output is the root e-class id.
    status, out = run_cmd("insert $db $dsname TIR t0 * x0", DATASET, varnames)
    @assert status == 0 "insert failed (status=$status): $out"
    eid = tryparse(Int, strip(out))
    @assert eid !== nothing "insert did not return an e-class id: $(repr(out))"
    println("inserted e-class: ", eid)

    # 1b. `insert` only writes structure; record a fitness so `top` can rank it.
    status, out = run_cmd("set-fit $db $dsname $eid 0.9", DATASET, varnames)
    @assert status == 0 "set-fit failed (status=$status): $out"
    println("set-fit: ", strip(out))

    # 1c. `insert-check` reports whether the expression already existed.
    status, check = run_cmd("insert-check $db $dsname TIR t0 * x0", DATASET, varnames)
    @assert status == 0 "insert-check failed (status=$status): $check"
    @assert strip(check) == "$eid,1" "expected existing expression, got: $(repr(check))"
    status, check = run_cmd("insert-check $db $dsname TIR t0 + x0", DATASET, varnames)
    @assert status == 0 "insert-check failed (status=$status): $check"
    @assert endswith(strip(check), ",0") "expected new expression, got: $(repr(check))"
    println("insert-check: existing -> ", eid, ",1; new -> ", strip(check))

    # 1d. Canonical expression enumeration and keyed fitness lookup.
    status, canon = run_cmd("canonical-eid $db $dsname $eid", DATASET, varnames)
    @assert status == 0 && parse(Int, strip(canon)) == eid "canonical-eid failed: $canon"
    status, exprs = run_cmd("canonical-expressions $db $dsname 100", DATASET, varnames)
    @assert status == 0 && occursin("Id,Expression,Size,Key", exprs) "canonical-expressions failed:\n$exprs"
    status, fits = run_cmd("fitted-expressions $db $dsname 100", DATASET, varnames)
    @assert status == 0 && occursin("Id,Expression,Fitness,Key", fits) "fitted-expressions failed:\n$fits"

    # 2. Query the top expressions back out.
    status, top = run_cmd("top $db $dsname 5", DATASET, varnames)
    @assert status == 0 "top failed (status=$status): $top"
    @assert occursin("Id,Expression", top) "unexpected top output:\n$top"
    @assert occursin("x0", top) && occursin("t0", top) "top did not return the inserted expression:\n$top"
    println("top output:\n", top)

    # 3. A throwing command must come back as status=1, not kill the process.
    bad_db = joinpath(tempdir(), "reggression-no-such-dir-$(getpid())", "smoke.db")
    status, err = run_cmd("top $bad_db $dsname 5", DATASET, varnames)
    @assert status == 1 "expected status=1 for a missing DB directory, got $status: $err"
    println("expected error path: ", first(split(strip(err), '\n')))

    ccall((:reggression_exit, LIB), Cvoid, ())
    println("M0 smoke test: OK")
end

main()
