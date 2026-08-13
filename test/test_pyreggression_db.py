from reggression import Reggression
import os
import tempfile

reg = Reggression(dataset="test/nikuradse_1.csv", loss="MSE", parseCSV="test/equations.bingo")

fd, fname = tempfile.mkstemp(suffix=".sqlite")
os.close(fd)
os.remove(fname)

try:
    msg = reg.persist(fname)
    assert os.path.isfile(fname), "persist did not create the database"
    assert "persisted" in msg, f"unexpected persist message: {msg!r}"

    top_db = reg.dbTop(fname, 3)
    assert not top_db.empty, "dbTop returned no rows"
    assert {"Id", "Expression"}.issubset(top_db.columns), top_db.columns

    dist_db = reg.dbDistribution(fname, 20)
    assert not dist_db.empty, "dbDistribution returned no rows"
    assert {"Size", "Count"}.issubset(dist_db.columns), dist_db.columns

    for op in ("EAdd", "EMul"):
        c = reg.dbCount(fname, op)
        assert isinstance(c, str) and int(c.split(":")[-1]) >= 0, f"dbCount {op} -> {c!r}"

    pareto_db = reg.dbPareto(fname)
    assert not pareto_db.empty, "dbPareto returned no rows"
    assert {"Id", "Fitness", "Size"}.issubset(pareto_db.columns), pareto_db.columns

    msg_load = reg.loadDB(fname)
    assert "loaded" in msg_load, f"unexpected loadDB message: {msg_load!r}"

    top_mem = reg.top(3)
    assert not top_mem.empty, "in-memory top after loadDB returned no rows"
    assert {"Id", "Expression", "Fitness"}.issubset(top_mem.columns), top_mem.columns

    dist_mem = reg.distribution(limitedAt=3, atLeast=1, fromTop=100)
    assert not dist_mem.empty, "in-memory distribution after loadDB returned no rows"

    print("test_pyreggression_db: OK")
    print(top_mem[["Id", "Size"]].head(3).to_string(index=False))
finally:
    if os.path.exists(fname):
        os.remove(fname)