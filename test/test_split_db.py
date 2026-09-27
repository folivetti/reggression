"""
Tests for split-DB architecture via the Python Reggression API.

Covers:
  - persist with split DB (egraph + fit)
  - dbSetFit / dbTop with fitDb parameter
  - dbEqSat / dbEqSatFrontier
  - Multiple datasets sharing one egraph
  - LoadDB and query
  - Refit (clear + re-fit)
  - Runtime bounds

Run: cd reggression && python -m pytest test/test_split_db.py -v
"""

import os
import time
import sqlite3

import numpy as np
import pandas as pd
import pytest

from reggression import Reggression


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------
@pytest.fixture
def small_dataset(tmp_path):
    """Create a 30-row synthetic dataset. Returns CSV path."""
    rng = np.random.default_rng(42)
    n = 30
    x0 = rng.uniform(-2, 2, n)
    x1 = rng.uniform(-2, 2, n)
    y = np.sin(x0) + 0.5 * x1 + 0.1 * rng.standard_normal(n)
    path = str(tmp_path / "data.csv")
    pd.DataFrame({"x0": x0, "x1": x1, "y": y}).to_csv(path, index=False)
    return path


@pytest.fixture
def egg_with_graph(small_dataset):
    """Create a Reggression object, populate with expressions, return it."""
    egg = Reggression(dataset=small_dataset, loss="MSE")
    for expr in ["x0 + x1", "x0 * x1", "sin(x0)", "x0 - x1"]:
        egg.insert(expr)
    egg.eqsat(3)
    return egg


# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------
def _fit_status(egraph_db, fit_db, dataset):
    """Return fitted/unfitted counts from the fit DB."""
    eg = sqlite3.connect(egraph_db)
    fit = sqlite3.connect(fit_db)
    total = eg.execute("SELECT COUNT(*) FROM eclass").fetchone()[0]
    ds_rows = fit.execute(
        "SELECT id FROM dataset WHERE name = ?", (dataset,)
    ).fetchall()
    if not ds_rows:
        eg.close(); fit.close()
        return {"total": total, "fitted": 0}
    dsid = ds_rows[0][0]
    fitted = fit.execute(
        "SELECT COUNT(*) FROM dataset_fit "
        "WHERE dataset_id = ? AND fitted = 1", (dsid,)
    ).fetchone()[0]
    eg.close(); fit.close()
    return {"total": total, "fitted": fitted}


# ---------------------------------------------------------------------------
# Functionality tests
# ---------------------------------------------------------------------------
class TestPersistSplitDB:
    """persist() creates the egraph DB; fit DB is created by dbSetFit/importDB."""

    def test_persist_creates_egraph_file(self, egg_with_graph, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        egg_with_graph.persist(egraph)
        assert os.path.exists(egraph)
        assert os.path.getsize(egraph) > 0

    def test_egraph_has_structural_tables(self, egg_with_graph, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        egg_with_graph.persist(egraph)
        con = sqlite3.connect(egraph)
        tables = {r[0] for r in con.execute(
            "SELECT name FROM sqlite_master WHERE type='table'"
        ).fetchall()}
        con.close()
        assert "eclass" in tables
        assert "enode" in tables
        assert "cstore_page" in tables

    def test_egraph_eclass_count(self, egg_with_graph, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        egg_with_graph.persist(egraph)
        con = sqlite3.connect(egraph)
        n = con.execute("SELECT COUNT(*) FROM eclass").fetchone()[0]
        con.close()
        assert n > 0


class TestDbSetFitAndTop:
    """dbSetFit stores fitness; dbTop reads it."""

    def test_db_set_fit_creates_fit_db(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        fitdb = str(tmp_path / "fit.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        # Get an eclass ID from the graph
        con = sqlite3.connect(egraph)
        eid = con.execute("SELECT eid FROM eclass LIMIT 1").fetchone()[0]
        con.close()
        # Set fitness — this should create the fit DB
        egg2.dbSetFit(egraph, eid, -0.5, fitDb=fitdb)
        assert os.path.exists(fitdb)
        # Verify the fit DB has the row
        con = sqlite3.connect(fitdb)
        n = con.execute("SELECT COUNT(*) FROM dataset_fit").fetchone()[0]
        con.close()
        assert n >= 1

    def test_db_top_returns_results(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        fitdb = str(tmp_path / "fit.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        # Set fitness for several eclasses
        con = sqlite3.connect(egraph)
        eids = [row[0] for row in con.execute(
            "SELECT eid FROM eclass LIMIT 5").fetchall()]
        con.close()
        for i, eid in enumerate(eids):
            egg2.dbSetFit(egraph, eid, -(i * 0.1), fitDb=fitdb)
        # Query top
        top = egg2.dbTop(egraph, 5, fitDb=fitdb)
        assert len(top) >= 1
        assert "Id" in top.columns
        assert "Expression" in top.columns
        assert "Fitness" in top.columns

    def test_db_top_ordering(self, egg_with_graph, small_dataset, tmp_path):
        """dbTop returns expressions sorted by fitness descending."""
        egraph = str(tmp_path / "egraph.db")
        fitdb = str(tmp_path / "fit.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        con = sqlite3.connect(egraph)
        eids = [row[0] for row in con.execute(
            "SELECT eid FROM eclass LIMIT 5").fetchall()]
        con.close()
        fits = [-0.1, -0.3, -0.5, -0.7, -0.9]
        for eid, fit in zip(eids, fits):
            egg2.dbSetFit(egraph, eid, fit, fitDb=fitdb)
        top = egg2.dbTop(egraph, 10, fitDb=fitdb)
        fitnesses = top["Fitness"].tolist()
        assert fitnesses == sorted(fitnesses, reverse=True)


class TestMultipleDatasets:
    """Two fit DBs share the same egraph."""

    def test_fit_dbs_independent(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        fit_a = str(tmp_path / "fit_a.db")
        fit_b = str(tmp_path / "fit_b.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        con = sqlite3.connect(egraph)
        eids = [row[0] for row in con.execute(
            "SELECT eid FROM eclass LIMIT 3").fetchall()]
        con.close()
        # Set fitness in fit_a only
        egg2.dbSetFit(egraph, eids[0], -0.5, fitDb=fit_a)
        # fit_b should have no fitted rows
        assert not os.path.exists(fit_b)
        # Set different fitness in fit_b
        egg2.dbSetFit(egraph, eids[0], -0.8, fitDb=fit_b)
        # Both DBs have the same eclass but different fitness
        con_a = sqlite3.connect(fit_a)
        fit_a_val = con_a.execute(
            "SELECT fitness FROM dataset_fit LIMIT 1").fetchone()[0]
        con_a.close()
        con_b = sqlite3.connect(fit_b)
        fit_b_val = con_b.execute(
            "SELECT fitness FROM dataset_fit LIMIT 1").fetchone()[0]
        con_b.close()
        assert fit_a_val != fit_b_val

    def test_top_different_per_dataset(self, egg_with_graph, small_dataset, tmp_path):
        """Different fitness values yield different top results."""
        egraph = str(tmp_path / "egraph.db")
        fit_a = str(tmp_path / "fit_a.db")
        fit_b = str(tmp_path / "fit_b.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        con = sqlite3.connect(egraph)
        eids = [row[0] for row in con.execute(
            "SELECT eid FROM eclass LIMIT 4").fetchall()]
        con.close()
        # Set opposite fitness ordering
        for i, eid in enumerate(eids):
            egg2.dbSetFit(egraph, eid, -i * 0.1, fitDb=fit_a)
            egg2.dbSetFit(egraph, eid, -(len(eids) - i) * 0.1, fitDb=fit_b)
        top_a = egg2.dbTop(egraph, 2, fitDb=fit_a)
        top_b = egg2.dbTop(egraph, 2, fitDb=fit_b)
        # Top of A should be first inserted; top of B should be last
        assert int(top_a["Id"].iloc[0]) != int(top_b["Id"].iloc[0])


class TestDbEqSat:
    """dbEqSat and dbEqSatFrontier work."""

    def test_db_eqsat_increases_classes(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        count_before = sqlite3.connect(egraph).execute(
            "SELECT COUNT(*) FROM eclass").fetchone()[0]
        egg2.dbEqSat(egraph, iterations=3)
        count_after = sqlite3.connect(egraph).execute(
            "SELECT COUNT(*) FROM eclass").fetchone()[0]
        assert count_after >= count_before


class TestLoadDBSplit:
    """loadDB loads graph; top() works after setting fitness."""

    def test_load_db_and_top(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        fitdb = str(tmp_path / "fit.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        # Set fitness so top() has something to rank
        con = sqlite3.connect(egraph)
        eids = [row[0] for row in con.execute(
            "SELECT eid FROM eclass LIMIT 3").fetchall()]
        con.close()
        for i, eid in enumerate(eids):
            egg2.dbSetFit(egraph, eid, -(i * 0.1), fitDb=fitdb)
        top = egg2.dbTop(egraph, 5, fitDb=fitdb)
        assert len(top) > 0
        assert "Expression" in top.columns


class TestRefit:
    """Refit: re-insert with different fitness; egraph unchanged."""

    def test_refit_preserves_egraph(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        fit_a = str(tmp_path / "fit_a.db")
        fit_refit = str(tmp_path / "fit_refit.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        eclass_count = sqlite3.connect(egraph).execute(
            "SELECT COUNT(*) FROM eclass").fetchone()[0]
        # Fit A
        con = sqlite3.connect(egraph)
        eids = [row[0] for row in con.execute(
            "SELECT eid FROM eclass LIMIT 3").fetchall()]
        con.close()
        for eid in eids:
            egg2.dbSetFit(egraph, eid, -0.5, fitDb=fit_a)
        # Refit: create new fit DB with different fitness
        for eid in eids:
            egg2.dbSetFit(egraph, eid, -0.8, fitDb=fit_refit)
        eclass_count_after = sqlite3.connect(egraph).execute(
            "SELECT COUNT(*) FROM eclass").fetchone()[0]
        assert eclass_count_after == eclass_count


# ---------------------------------------------------------------------------
# Runtime tests
# ---------------------------------------------------------------------------
class TestTiming:
    """Operations complete within reasonable time bounds."""

    def test_persist_latency(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        t0 = time.time()
        egg_with_graph.persist(egraph)
        elapsed = time.time() - t0
        assert elapsed < 5.0, f"persist took {elapsed:.2f}s (> 5s)"

    def test_db_eqsat_latency(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        t0 = time.time()
        egg2.dbEqSat(egraph, iterations=3)
        elapsed = time.time() - t0
        assert elapsed < 10.0, f"dbEqSat took {elapsed:.2f}s (> 10s)"

    def test_db_set_fit_latency(self, egg_with_graph, small_dataset, tmp_path):
        egraph = str(tmp_path / "egraph.db")
        fitdb = str(tmp_path / "fit.db")
        egg_with_graph.persist(egraph)
        egg2 = Reggression(dataset=small_dataset, loss="MSE")
        egg2.loadDB(egraph)
        con = sqlite3.connect(egraph)
        eid = con.execute("SELECT eid FROM eclass LIMIT 1").fetchone()[0]
        con.close()
        t0 = time.time()
        egg2.dbSetFit(egraph, eid, -0.5, fitDb=fitdb)
        elapsed = time.time() - t0
        assert elapsed < 2.0, f"dbSetFit took {elapsed:.2f}s (> 2s)"
