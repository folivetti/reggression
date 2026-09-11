import atexit
from contextlib import contextmanager
from threading import Lock
from typing import Iterator, List
from io import StringIO

import csv
import os
import tempfile

import numpy as np
import pandas as pd

# GHC auto-initializes the RTS when the shared library is loaded (during the
# _binding import below) and reads the GHCRTS env var for flags; if unset,
# defaults are used. Set a default here so the embedded runtime uses tighter GC
# (-F lower heap growth factor, -A smaller nursery): out-of-core eqsat reads
# every class body, and GHC otherwise keeps a large reserved heap high-water
# mark (which it does not return to the OS), inflating RSS.
if os.environ.get("GHCRTS") is None:
    os.environ["GHCRTS"] = "-F1.2"

from ._binding import (
    unsafe_hs_reggression_version,
    unsafe_hs_reggression_main,
    unsafe_hs_reggression_run,
    unsafe_hs_reggression_init,
    unsafe_hs_reggression_exit,
)

VERSION: str = "2.3.0"


_hs_rts_init: bool = False
_hs_rts_lock: Lock = Lock()


def hs_rts_exit() -> None:
    global _hs_rts_lock
    with _hs_rts_lock:
        unsafe_hs_reggression_exit()


@contextmanager
def hs_rts_init(args: List[str] = []) -> Iterator[None]:
    global _hs_rts_init
    global _hs_rts_lock
    with _hs_rts_lock:
        if not _hs_rts_init:
            _hs_rts_init = True
            unsafe_hs_reggression_init(args)
            atexit.register(hs_rts_exit)
    yield None


def version() -> str:
    with hs_rts_init():
        return unsafe_hs_reggression_version()


def main(args: List[str] = []) -> int:
    with hs_rts_init(args):
        return unsafe_hs_reggression_main()

def reggression_run(myCmd : str, dataset : str, testData : str, loss : str, loadFrom : str, dumpTo : str, parseCSV : str, parseParams : int, calcDL : int, calcFit : int, varnames : list) -> str:
    with hs_rts_init(["reggression", "+RTS", "-F1.2", "-RTS"]):
        return unsafe_hs_reggression_run(myCmd, dataset, testData, loss, loadFrom, dumpTo, parseCSV, parseParams, calcDL, calcFit, varnames)

class Reggression():
    """ Starts up the r🥚ression engine.

    Parameters
    ----------
    dataset : str
        Filename of the training dataset in csv format.

    testData : str
        Filename of the test set in csv format.

    loss : {"MSE", "Gaussian", "Bernoulli", "Poisson"}, default="MSE"
        Loss function used to evaluate the expressions:
        - MSE (mean squared error) should be used for regression problems.
        - Gaussian likelihood should be used for regression problem when you want to
          fit the error term.
        - Bernoulli likelihood should be used for classification problem.
        - Poisson likelihood should be used when the data distribution follows a Poisson.

    loadFrom : str, default=""
        If not empty, it will load an e-graph and resume the search.
        The user must ensure that the loaded e-graph is from the same
        dataset and loss function.

    parseCSV : str
        CSV file with expressions to be loaded instead of an e-graph file.
        The CSV must follow the format expression,parameters,fitness
        and the filename extension must be the name of the algorithm from
        which the expressions were generated, one of:
        - tir (works for TIR and ITEA)
        - hl (HeuristicLab)
        - operon (Operon)
        - bingo (BINGO)
        - gomea (GP-GOMEA)
        - pysr (PYSR)
        - sbp (SBP)
        - eplex (works for EPLEX, FEAT, BRUSH)

    parseParams : bool, default=True
        Whether to extract the parameters values from the
        expression in the CSV file.

    Examples
    --------
    >>> from reggression import Reggression
    >>> egg = PyReggression("data.csv", loadFrom="myData.egraph")
    >>> egg.top(10)
    """
    def __init__(self, dataset, testData="", loss="MSE", loadFrom="", parseCSV="", parseParams=True, refit=False, simpleOutput=False, dataset_name="", db="", fitDb=""):
        losses = ["MSE", "Gaussian", "Bernoulli", "Poisson", "MAPE"]
        if loss not in losses:
            raise ValueError('loss must be one of ', losses)
        if len(dataset) == 0:
            raise ValueError('you must provide a dataset filename')
        if not os.path.isfile(dataset):
            raise ValueError('dataset does not exist')
        if not isinstance(parseParams, bool):
            raise ValueError('parseParams must be a boolean')
        if not isinstance(refit, bool):
            raise ValueError('refit must be a boolean')
        self.dataset = dataset
        self.dataset_name = dataset_name if dataset_name else os.path.splitext(os.path.basename(dataset))[0]
        self.testData = testData
        self.loss = loss
        self.loadFrom = loadFrom
        self.parseCSV = parseCSV
        self.parseParams = int(parseParams)
        self.refit = refit
        self.simpleOutput = simpleOutput
        if db:
            self.db = db
            self._db_is_temp = False
            # DB-only mode: commands open their own SQLite connections.
            print("Using DB-backed mode: " + db)
        else:
            # default to a temp SQLite DB so db-native methods (insert/top/...)
            # always have a valid target
            tmp = tempfile.NamedTemporaryFile(delete=False, suffix=".db",
                                              dir=os.getcwd())
            self.db = tmp.name
            self._db_is_temp = True
            tmp.close()
            print("Welcome to r🥚ression")
        self.fitDb = fitDb

        df = pd.read_csv(dataset)
        self.varnames = ','.join(df.columns)
    def __del__(self):
        if getattr(self, "_db_is_temp", False):
            try:
                os.remove(self.db)
            except OSError:
                pass
    def set_simple_output(self, b):
        '''
        Sets to simple output when printing a dataframe.
        This will select only the e-class id, latex, fitness columns.

        Parameters
        ----------

        b : bool
            Whether to set simple output
        '''
        self.simpleOutput = b

    def set_varnames(self, names):
        self.varnames = ','.join(names)

    def runQuery(self, query, df=True):
        '''  Runs a query.

        Parameters
        ----------

        query : str
            A string with the query to send to r🥚ression.

        df : bool, default=True
            Whether the query returns a DataFrame.
        '''
        # DB-native: pass empty loadFrom/dumpTo (skip binary round-trip)
        csv_data = reggression_run(query, self.dataset, self.testData, self.loss, "", "", self.parseCSV, self.parseParams, 0, 0, self.varnames)
        if df and len(csv_data) > 0:
            csv_io = StringIO(csv_data.strip())
            self.results = pd.read_csv(csv_io, header=0)
        else:
            self.results = pd.DataFrame() if df else csv_data
        cols = self.results.columns if df else []
        if self.simpleOutput and all(['Id' in cols, 'Latex' in cols, 'Fitness' in cols]):
            return self.results[['Id', 'Latex', 'Fitness']]
        return self.results

    def _dbSpec(self):
        """Return the db:fitDb spec string for DB-native commands."""
        return f"{self.db}:{self.fitDb}" if self.fitDb else self.db

    def top(self, n=5, filters=[], criteria="fitness", pattern="", isRoot=False, negate=False, ci=False):
        ''' Returns the top-n expressions following a certain criteria.

        Parameters
        ----------

        n : int, default=5
            Returns a DataFrame with the top-n expressions

        filters : list[str], default=[]
            A list of filters in the format "criteria op number"
            where criteria can be 'size' (number of symbols in the expression),
            'parameters' (number of numerical parameters), or 'cost' (complexity cost),
            op is one of <,<=,>,>=,=
            E.g., ["size < 10", "parameters > 2", "cost=5"]

        criteria : {"fitness", "dl"}, default="fitness"
            Whether to sort the expressions by fitness (maximization) or description length (minimization).

        pattern : str, default=""
            A pattern that must be part of the expressions. The syntax
            should follow a mathematical expression where x0, x1, ... is one
            of the variables, t0, t1, ... is one of the parameters, and
            v0, v1, v2, ... is a pattern variable (match all).
            E.g., t0 * x0 will match only this single expression
            v0 * x0 will match any expression multiplied by x0 (t0 * x0, sin(x0 + t1) * t0 * x0)
            v0 ** v0 will match any expression to the power of itself (x0^x0, (x0 + t0)^(x0 + t0))

        isRoot : bool, default=False
            Whether the matched expression should match the pattern at the root.
            E.g., v0 + v1 will match x0 + (t0 * x1) but not (x0 + t0) * x1.

        negate : bool, default=False
           Whether to retrieve expressions NOT matching the pattern

        '''
        if criteria not in ["fitness", "dl"]:
            raise ValueError('criteria must be either fitness or dl')
        if not isinstance(isRoot, bool):
            raise TypeError('isRoot must be a boolean')
        if not isinstance(negate, bool):
            raise TypeError('negate must be a boolean')

        if not pattern:
            cistr = " with ci" if ci else ""
            query = f"top {self._dbSpec()} {self.dataset_name} {n}{cistr}"
            return self.runQuery(query)
        else:
            rootStr = " root" if isRoot else ""
            notStr = " not" if negate else ""
            cistr = " ci" if ci else ""
            query = f"top-pattern {self._dbSpec()} {self.dataset_name} {n} {pattern}{rootStr}{notStr}{cistr}"
            return self.runQuery(query)

    def distribution(self, filters=[], limitedAt=25, dsc=True, byFitness=True, atLeast=1000, fromTop=5000):
        ''' Returns the distribution of the top patterns following a certain criteria.

        Parameters
        ----------

        filters : list[str], default=[]
            A list of filters limiting the size of the pattern
            following the format "size op number"
            where op is one of <,<=,>,>=,=
            E.g., ["size < 10", "parameters > 2", "cost=5"]

        limitedAt : int, default=25
            The maximum number of patterns to display.

        dsc : bool, default=True
            Whether to sort the patterns in ascending or descending order

        byFitness : bool, default=True
            Whether to sort the patterns by fitness or frequency of occurrence

        atLeast : int, default=1000
            The minimum frequency of the pattern

        fromTop : int, default=5000
            The size of the subset of expressions to extract the pattern.
            This value shouldn't be more than 10000 due to the exponential
            number of possible patterns.
        '''
        if not isinstance(dsc, bool):
            raise TypeError('dsc must be a boolean')
        if not isinstance(byFitness, bool):
            raise TypeError('byFitness must be a boolean')
        if fromTop > 10000:
            raise ValueError('fromTop should be less than 10000')

        n = min(fromTop, 10000)
        query = f"distribution-pattern {self._dbSpec()} {self.dataset_name} {n}"
        return self.runQuery(query)
    def modularity(self, n, filters=["> 1"], byFitness=True):
        ''' Returns the top-N equations presenting repeated patterns with size defined by filters.

        Parameters
        ----------

        n : int
            Number of top equations to return.

        filters : list[str], default=[]
            A list of filters limiting the size of the pattern
            following the format "size op number"
            where op is one of <,<=,>,>=,=
            E.g., ["size < 10", "parameters > 2", "cost=5"]

        byFitness : bool, default=True
            Whether to sort the patterns by fitness or frequency of occurrence
        '''
        query = f"modularity {self._dbSpec()} {self.dataset_name} {min(n, 10000)}"
        return self.runQuery(query)
    def countPattern(self, pattern):
        ''' Count the frequency of a certain pattern

        Parameters
        ----------

        pattern : str
            Pattern that should be counted
        '''
        query = f"count-pattern {self._dbSpec()} {self.dataset_name} {pattern} 10000"
        return self.runQuery(query, df=False)
    def report(self, n, ci=False):
        ''' Detailed report of e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        ci : bool, default=False
            Whether to include profile-likelihood confidence intervals
        '''
        cistr = " with ci" if ci else ""
        test = self.testData if self.testData else "-"
        return self.runQuery(f"report {self._dbSpec()} {self.dataset_name} {self.dataset} {test} {n}{cistr} {self.loss}")
    def optimize(self, n, ci=False):
        ''' (re)optimize e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        ci : bool, default=False
            Whether to include profile-likelihood confidence intervals
        '''
        cistr = " with ci" if ci else ""
        return self.runQuery(f"optimize {self._dbSpec()} {self.dataset_name} {self.dataset} {n}{cistr} {self.loss}", df=False)
    def eqsat(self, n=1):
        ''' run n steps of equality saturation
        sequentially for each rule (see https://github.com/folivetti/srtree/blob/main/src/Algorithm/EqSat/Simplify.hs)
        Note: if the e-graph is large, this will take some seconds. This will not ensure saturation as it will run each rule
        sequentially.
        '''
        query = f"eqsat {self._dbSpec()} {self.dataset_name} {n} default"
        return self.runQuery(query, df=False)
    def getNExpressions(self, eid, n=10):
        ''' return n expressions described by e-class id eid 
        '''
        return self.runQuery(f"getNExprs {self._dbSpec()} {self.dataset_name} {n} {eid}")
    def subtrees(self, n):
        ''' Return the subtrees of e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        '''
        return self.runQuery(f"subtrees {self._dbSpec()} {self.dataset_name} {n}", df=False)
    def insert(self, expr, alg="TIR"):
        ''' Insert a new expression

        Parameters
        ----------
        expr : str
            Expression to be inserted
        alg : str, default="TIR"
            Equation format/algorithm used to parse the expression (one of
            TIR, HL, OPERON, BINGO, GOMEA, PYSR, SBP, EPLEX, NEOGP).
        '''
        return self.runQuery(f"insert {self._dbSpec()} {self.dataset_name} {alg} {expr}", df=False)
    def pareto(self, byFitness=True, ci=False):
        ''' Return the Pareto front of accuracy x size

        Parameters
        ----------

        byFitness : bool, default=True
            Whether the first objective is fitness or description length
        ci : bool, default=False
            Whether to include profile-likelihood confidence intervals
        '''
        cistr = " with ci" if ci else ""
        byFitnessStr = " by fitness" if byFitness else " by dl"
        front = self.runQuery(f"pareto {self._dbSpec()} {self.dataset_name}{cistr}{byFitnessStr}")
        col = 'Fitness' if byFitness else 'DL'
        return front[front[col] >= front[col].cummax()]
    def extractPattern(self, eid):
        ''' Returns the patterns and counts of matches for a single expression

        Parameters
        ----------
        eid : int
            e-class id of the expression.
        '''
        return self.runQuery(f"extract-pattern {self._dbSpec()} {self.dataset_name} {eid}")
    def distributionOfTokens(self, top=-1):
        ''' Return the counts and average fitness of tokens.

        '''
        n = top if top > 0 else 10000
        query = f"distribution-tokens {self._dbSpec()} {self.dataset_name} {min(n, 10000)}"
        return self.runQuery(query)

    def patternMap(self, pattern, limit=-1):
        ''' Shows what each wildcard variable matched in pattern expressions.

        For a pattern like "v0 * v1", this returns every match with columns
        showing the expression each wildcard (v0, v1, ...) resolved to, along
        with its e-class ID.

        Parameters
        ----------
        pattern : str
            A pattern with wildcards v0, v1, ...
            E.g., "v0 * v1", "exp(v0) + v1", "(v0 + v1) * v2"

        limit : int, default=-1
            Max number of matches to return. -1 = all.

        Returns
        -------
        pd.DataFrame with columns:
            Match : int
                Match index
            Expression : str
                The full matched expression at the root
            v0 : str
                Expression that v0 matched
            v0_eid : int
                E-class ID of v0's match
            v1 : str, v1_eid : int, ...
                Same for each additional wildcard
        '''
        n = limit if limit > 0 else 10000
        query = f"pattern-map {self._dbSpec()} {self.dataset_name} {pattern} {min(n, 10000)}"
        return self.runQuery(query)

    def eclassTerminals(self, eid):
        ''' List all unique terminals (variables, parameters, constants) inside an e-class.

        Parameters
        ----------
        eid : int
            E-class id to inspect.

        Returns
        -------
        pd.DataFrame with columns:
            Type : str
                One of "Var", "Param", or "Const"
            Name : str
                The terminal name, e.g. "x0", "t1", "3.14"
        '''
        df = self.runQuery(f"eclass-terminals {self._dbSpec()} {self.dataset_name} {eid}")
        if "Name" in df.columns:
            df["Name"] = df["Name"].astype(str)
        return df

    def save(self, fname):
        ''' Save the e-graph file

        Parameters
        ----------
        fname : str
            Filename
        '''
        return self.persist(fname)
    def load(self, fname):
        ''' Load an e-graph file

        Parameters
        ----------
        fname : str
            Filename
        '''
        return self.loadDB(fname)
    def persist(self, fname="", fitDb=""):
        ''' Save the current e-graph to the SQLite database file fname
        (srtree-db). A later top/distribution/count/pareto on the
        same file runs the query directly in SQLite.

        Parameters
        ----------
        fname : str, default=""
            SQLite database filename. If empty, uses self.db.
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"persist {dbSpec} {self.dataset_name}", df=False)
    def loadDB(self, fname="", fitDb=""):
        ''' Load an e-graph previously persisted with `persist` into memory.

        Parameters
        ----------
        fname : str, default=""
            SQLite database filename. If empty, uses self.db.
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"load {dbSpec} {self.dataset_name}", df=False)
    def importDB(self, eqs, fname="", extractParameters=True, fitDb=""):
        ''' Build an e-graph directly in the SQLite database `fname`,
        out-of-core, by streaming the expressions in the CSV file `eqs` into
        the database (structural, content-addressed, bounded memory) -- no
        in-memory e-graph is built first. The resulting database is identical
        to `persist` of the corresponding in-memory seed and can be saturated
        with `dbEqSat`.

        IMPORTANT: the extension of the CSV file must match the source
        algorithm (e.g. `.tir`), as with `importFromCSV`.

        Parameters
        ----------
        eqs : str
            Path to the CSV file of expressions.
        fname : str, default=""
            SQLite database filename to build. If empty, uses self.db.
        extractParameters : bool, default=True
            Whether to extract parameter values from the expressions.
        fitDb : str, default=""
            Optional separate fit database filename.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"import {dbSpec} {eqs} {self.dataset_name}", df=False)
    def dbStream(self, fname="", op="", budget=1000, fitDb=""):
        ''' PoC: stream the enode table by operator through a SQLite cursor and
        report the total count of matching nodes and the first `budget` matched
        e-classes, to validate O(1)-memory streaming matching.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"stream {dbSpec} {op} {budget}", df=False)
    def dbPushFit(self, fname="", fitDb=""):
        ''' Write the current e-graph's fitness/DL metrics into the @fit@ table
        of the SQLite database fname (the graph structure is left intact).

        Parameters
        ----------
        fname : str, default=""
            SQLite database filename. If empty, uses self.db.
        fitDb : str, default=""
            Optional separate fit database filename.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"push-fit {dbSpec} {self.dataset_name}", df=False)
    def dbRefreshFitness(self, fname="", fitDb=""):
        ''' Overwrite the in-memory fitness values with those stored in the
        @fit@ table of the SQLite database fname (per e-class, by canonical
        id).

        Parameters
        ----------
        fname : str, default=""
            SQLite database filename. If empty, uses self.db.
        fitDb : str, default=""
            Optional separate fit database filename.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"refresh-fitness {dbSpec} {self.dataset_name}", df=False)
    def dbEqSatFrontier(self, fname="", iterations=10, ruleset="default", fitDb=""):
        ''' Re-saturate only the frontier of a lazily loaded (out-of-core) e-graph:
        the e-classes that were created or merged since the last re-saturation pass
        (tracked in the DB's `frontier` table). The matcher's candidate roots are
        restricted to the frontier, so unchanged parts of the graph are not re-worked.
        The frontier is cleared afterwards. O(1) memory (paged). The pure in-memory
        eggp loop and a full `dbEqSat` are unaffected.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"eqsat-frontier {dbSpec} {self.dataset_name} {iterations} {ruleset}", df=False)
    def dbInsert(self, fname="", expr="", alg="TIR", fitDb=""):
        ''' Local eggp delta: insert a single expression into the DB-backed
        (out-of-core) e-graph in `fname`. Its subgraph is written through and
        content-addressed (existing subexpressions dedup against the live
        tables); every genuinely-new class is marked as part of the
        re-saturation frontier, so a later `dbEqSatFrontier` re-saturates only
        what changed. Returns the root e-class id (as an int), symmetric with
        the in-memory `insert`. O(subgraph) work, O(1) memory.

        Parameters
        ----------
        fname : str, default=""
            SQLite database filename. If empty, uses self.db.
        expr : str
            Expression to insert
        fitDb : str, default=""
            Optional separate fit database filename.

        Returns
        -------
        int : the root e-class id
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        v = self.runQuery(f"insert {dbSpec} {self.dataset_name} {alg} {expr}", df=False)
        return int(str(v).strip())
    def dbSetFit(self, fname="", eid=0, fitness=0.0, fitDb=""):
        ''' Record the fitness of a single e-class in `dataset_fit`, so a
        newly-inserted DB expression (from `dbInsert`) can be ranked by the query
        layer once the eggp loop has evaluated it.
        '''
        db = fname or self.db
        fd = fitDb or self.fitDb
        dbSpec = f"{db}:{fd}" if fd else db
        return self.runQuery(f"set-fit {dbSpec} {self.dataset_name} {eid} {fitness}", df=False)
    def importFromCSV(self, fname, extractParameters=True):
        ''' import equations from a CSV file
        IMPORTANT: the extension of the CSV file must match the source
        algorithm used to generate the equations: tir, itea, operon, pysr, bingo, eplex, feat, gomea.
        The format of the file should be a comma separated list of equation,parameters,fitness

        Parameters
        ----------
        fname : str
            Filename
        extractParameters : bool
            whether to convert floating points in the expression to parameters
        '''
        return self.importDB(fname, self.db, extractParameters)

    def profilePlot(self, eid, dataPath=None, dataSpec=None, saveTo=None):
        ''' Compute profile-likelihood data and plot tau vs theta curves
        and pairwise theta x theta contour plots.

        Parameters
        ----------
        eid : int
            The e-class ID of the expression to profile.

        dataPath : str, default=None
            Path to the dataset CSV. If None, uses self.dataset.

        dataSpec : str, default=None
            Full data spec for loadDataset (e.g. "file.csv:::y:0,1").
            If provided, overrides dataPath. The spec format is:
            file:start:end:target:features:yerr

        saveTo : str, default=None
            If provided, save the plot to this file path instead of showing.

        Returns
        -------
        fig : matplotlib Figure
        '''
        import matplotlib
        if saveTo:
            matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        from matplotlib.colors import Normalize
        from matplotlib.cm import ScalarMappable

        data = dataSpec or dataPath or self.dataset
        raw = self.runQuery(
            f"profile-data {self._dbSpec()} {self.dataset_name} {eid} {data}",
            df=False
        )

        # Check for error messages from Haskell
        if not raw or raw.startswith("theta has fewer") or raw.startswith("extraction failed") or raw.startswith("The id") or raw.startswith("profiling failed"):
            raise ValueError(raw.strip() if raw else "Empty response from profile-data")

        # Parse the two sections
        sections = raw.strip().split("\n\n")
        profile_section = sections[0] if len(sections) > 0 else ""
        contour_section = sections[1] if len(sections) > 1 else ""

        # Parse profile data
        profile_lines = [l for l in profile_section.strip().split("\n") if l and not l.startswith("param,")]
        profiles = {}  # param_idx -> (taus, thetas_per_param, opt)
        for line in profile_lines:
            parts = line.split(",")
            param_idx = int(parts[0])
            tau = float(parts[1])
            thetas = [float(x) for x in parts[2:-1]]
            opt = float(parts[-1])
            if param_idx not in profiles:
                profiles[param_idx] = {"taus": [], "thetas": [[] for _ in range(len(thetas))], "opt": opt}
            profiles[param_idx]["taus"].append(tau)
            for c, v in enumerate(thetas):
                profiles[param_idx]["thetas"][c].append(v)

        # Parse contour data
        contour_lines = [l for l in contour_section.strip().split("\n") if l and not l.startswith("i,j,")]
        contours = {}  # (i,j) -> (theta_i, theta_j)
        for line in contour_lines:
            parts = line.split(",")
            i, j = int(parts[0]), int(parts[1])
            ti, tj = float(parts[2]), float(parts[3])
            if (i, j) not in contours:
                contours[(i, j)] = ([], [])
            contours[(i, j)][0].append(ti)
            contours[(i, j)][1].append(tj)

        k = len(profiles)
        if k == 0:
            raise ValueError("No profile data returned. Check that the expression has >= 2 parameters.")

        # Create subplots: row 1 = tau vs theta profiles, row 2 = contour (full width)
        n_contour_pairs = k * (k - 1) // 2
        fig = plt.figure(figsize=(5 * k, 4 * 2))
        gs_top = fig.add_gridspec(1, k, hspace=0.3, top=0.92, bottom=0.55)
        gs_bot = fig.add_gridspec(1, 1, hspace=0.3, top=0.45, bottom=0.12)

        axes_top = [fig.add_subplot(gs_top[0, i]) for i in range(k)]
        ax_contour = fig.add_subplot(gs_bot[0, 0]) if n_contour_pairs > 0 else None

        # Plot theta vs tau for each parameter (theta on x, tau on y)
        for p_idx in range(k):
            ax = axes_top[p_idx]
            prof = profiles[p_idx]
            taus = np.array(prof["taus"])
            thetas_p = np.array(prof["thetas"][p_idx])  # the profiled parameter
            opt_val = prof["opt"]

            # Sort by tau for clean plotting
            order = np.argsort(taus)
            ax.plot(thetas_p[order], taus[order], 'b-', linewidth=1.5, label=f'theta_{p_idx}')
            ax.axvline(x=opt_val, color='r', linestyle='--', alpha=0.5, label=f'MLE={opt_val:.4g}')
            ax.axhline(y=0, color='gray', linestyle=':', alpha=0.5)
            ax.set_xlabel(f'theta_{p_idx}')
            ax.set_ylabel('tau')
            ax.set_title(f'Profile: theta_{p_idx}')
            ax.legend(fontsize=8)
            ax.grid(True, alpha=0.3)

        # Plot pairwise contours
        if ax_contour is not None:
            for i in range(k):
                for j in range(i + 1, k):
                    if (i, j) in contours:
                        ti_arr = np.array(contours[(i, j)][0])
                        tj_arr = np.array(contours[(i, j)][1])
                        ax_contour.plot(ti_arr, tj_arr, 'b-', linewidth=1.5)
                        ax_contour.plot(profiles[i]["opt"], profiles[j]["opt"], 'r+', markersize=10, markeredgewidth=2, label='MLE')
                        ax_contour.legend(fontsize=8)
                    ax_contour.set_xlabel(f'theta_{i}')
                    ax_contour.set_ylabel(f'theta_{j}')
                    ax_contour.set_title(f'Contour: theta_{i} x theta_{j}')
                    ax_contour.grid(True, alpha=0.3)
            # Make contour square by using equal aspect with dataLim
            ax_contour.set_box_aspect(1)

        if saveTo:
            fig.savefig(saveTo, dpi=150, bbox_inches='tight')
        return fig

    def getNEclasses(self, eid, n=10):
        return self.runQuery(f"getNEclasses {self._dbSpec()} {self.dataset_name} {n} {eid}")
