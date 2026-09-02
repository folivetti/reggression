import atexit
from contextlib import contextmanager
from threading import Lock
from typing import Iterator, List
from io import StringIO
import tempfile
import csv
import os

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

VERSION: str = "2.2.0"


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
    def __init__(self, dataset, testData="", loss="MSE", loadFrom="", parseCSV="", parseParams=True, refit=False, simpleOutput=False, dataset_name=""):
        losses = ["MSE", "Gaussian", "Bernoulli", "Poisson"]
        if loss not in losses:
            raise ValueError('loss must be one of ', losses)
        #if len(loadFrom) == 0 and len(parseCSV) == 0:
        #    raise ValueError('you must provide either a "loadFrom" or "parseCSV" value')
        if len(dataset) == 0:
            raise ValueError('you must provide a dataset filename')
        if not os.path.isfile(dataset):
            raise ValueError('dataset does not exist')
        if (len(loadFrom) > 0 or len(parseCSV) > 0) and not os.path.isfile(loadFrom) and not os.path.isfile(parseCSV):
            raise ValueError('egraph or CSV file do not exist')
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

        df = pd.read_csv(dataset)
        self.varnames = ','.join(df.columns)

        self.temp_file = tempfile.NamedTemporaryFile(mode='w+', newline='', delete=False,  dir=os.getcwd(), suffix='.egraph')
        self.tempname = self.temp_file.name
        self.temp_file.close()
        print("Calculating DL...")
        reggression_run("top 10", self.dataset, self.testData, self.loss, self.loadFrom, self.tempname, self.parseCSV, self.parseParams, self.refit, self.refit, self.varnames)
        print("Welcome to r🥚ression")
    def __del__(self):
        ''' remove temporary e-graph file before ending the program '''
        os.remove(self.tempname)
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
        csv_data = reggression_run(query, self.dataset, self.testData, self.loss, self.tempname, self.tempname, self.parseCSV, self.parseParams, 0, 0, self.varnames)
        if df and len(csv_data) > 0:
            csv_io = StringIO(csv_data.strip())
            self.results = pd.read_csv(csv_io, header=0)
        else:
            self.results = pd.DataFrame() if df else csv_data
        cols = self.results.columns if df else []
        if self.simpleOutput and all(['Id' in cols, 'Latex' in cols, 'Fitness' in cols]):
            return self.results[['Id', 'Latex', 'Fitness']]
        return self.results

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

        filters_str = " ".join([f"with {f}" for f in filters])
        patmatch = f"{'not ' if negate else ''} matching {'root' if isRoot else ''} {pattern}" if len(pattern)>0 else ""
        cistr = " with ci" if ci else ""
        query = f"top {n} {filters_str} by {criteria} {patmatch}{cistr}"
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

        filters_str = " ".join([f"with {f}" for f in filters])
        query = f"distribution {filters_str} limited at {limitedAt} {'dsc' if dsc else 'asc'} {'by fitness' if byFitness else ''} with at least {atLeast} from top {fromTop}"
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
        if not isinstance(n, int):
            raise TypeError('n must be an int')
        if not isinstance(byFitness, bool):
            raise TypeError('byFitness must be a boolean')

        filters_str = " ".join([f"with size {f}" for f in filters])
        query = f"modularity {n} {filters_str} {'by fitness' if byFitness else ''}"
        return self.runQuery(query)
    def countPattern(self, pattern):
        ''' Count the frequency of a certain pattern

        Parameters
        ----------

        pattern : str
            Pattern that should be counted
        '''
        query = f"count-pattern {pattern}"
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
        return self.runQuery(f"report {n} {'with ci' if ci else ''}")
    def optimize(self, n, ci=False):
        ''' (re)optimize e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        ci : bool, default=False
            Whether to include profile-likelihood confidence intervals
        '''
        return self.runQuery(f"optimize {n} {'with ci' if ci else ''}")
    def eqsat(self, n=1):
        ''' run n steps of equality saturation
        sequentially for each rule (see https://github.com/folivetti/srtree/blob/main/src/Algorithm/EqSat/Simplify.hs)
        Note: if the e-graph is large, this will take some seconds. This will not ensure saturation as it will run each rule
        sequentially.
        '''
        return self.runQuery(f"eqsat {n}")
    def getNExpressions(self, eid, n=10):
        ''' return n expressions described by e-class id eid 
        '''
        return self.runQuery(f"getNExprs {n} {eid}")
    def subtrees(self, n):
        ''' Return the subtrees of e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        '''
        return self.runQuery(f"subtrees {n}")
    def insert(self, expr):
        ''' Insert a new expression

        Parameters
        ----------
        expr : str
            Expression to be inserted
        '''
        return self.runQuery(f"insert {expr}")
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
        front = self.runQuery(f"pareto {'by fitness' if byFitness else 'by dl'}{cistr}")
        col = 'Fitness' if byFitness else 'DL'

        return front[front[col] >= front[col].cummax()]

    def extractPattern(self, eid):
        ''' Returns the patterns and counts of matches for a single expression

        Parameters
        ----------
        eid : int
            e-class id of the expression.
        '''
        return self.runQuery(f"extract-pattern {eid}")
    def distributionOfTokens(self, top=-1):
        ''' Return the counts and average fitness of tokens.

        '''
        return self.runQuery(f"distribution-tokens {top}")
    def save(self, fname):
        ''' Save the e-graph file

        Parameters
        ----------
        fname : str
            Filename
        '''
        return self.runQuery(f"save {fname}", df=False)
    def load(self, fname):
        ''' Load an e-graph file

        Parameters
        ----------
        fname : str
            Filename
        '''
        return self.runQuery(f"load {fname}", df=False)
    def persist(self, fname, fitDb=""):
        ''' Save the current e-graph to the SQLite database file fname
        (srtree-db). A later db-top/db-distribution/db-count/db-pareto on the
        same file runs the query directly in SQLite.

        Parameters
        ----------
        fname : str
            SQLite database filename
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"persist {dbSpec} {self.dataset_name}", df=False)
    def loadDB(self, fname, fitDb=""):
        ''' Load an e-graph previously persisted with `persist` into memory.

        Parameters
        ----------
        fname : str
            SQLite database filename
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-load {dbSpec} {self.dataset_name}", df=False)
    def importDB(self, eqs, fname, extractParameters=True, fitDb=""):
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
        fname : str
            SQLite database filename to build.
        extractParameters : bool, default=True
            Whether to extract parameter values from the expressions.
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-import {dbSpec} {eqs} {self.dataset_name}", df=False)
    def dbTop(self, fname, n=5, ci=False, fitDb=""):
        ''' Top-n e-classes by fitness queried directly from the SQLite
        database fname (no in-memory pattern enumeration).

        Parameters
        ----------
        fname : str
            SQLite database filename
        n : int, default=5
            Number of e-classes
        ci : bool, default=False
            Whether to include profile-likelihood confidence intervals
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        cistr = " with ci" if ci else ""
        return self.runQuery(f"db-top {dbSpec} {self.dataset_name} {n}{cistr}")
    def dbDistribution(self, fname, maxSize=100, fitDb=""):
        ''' Number of evaluated e-classes per model size (size <= maxSize),
        queried from the SQLite database fname.

        Parameters
        ----------
        fname : str
            SQLite database filename
        maxSize : int, default=100
            Maximum model size included
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-distribution {dbSpec} {self.dataset_name} {maxSize}")
    def dbCount(self, fname, op, fitDb=""):
        ''' Number of e-classes containing an e-node with operator `op`
        (e.g. "EAdd", "EMul", "LogAbs"), queried from the SQLite database fname.

        Parameters
        ----------
        fname : str
            SQLite database filename
        op : str
            Operator detail string
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-count {dbSpec} {op}", df=False)
    def dbPareto(self, fname, ci=False, fitDb=""):
        ''' Pareto front over (max fitness, min size) queried from the SQLite
        database fname.

        Parameters
        ----------
        fname : str
            SQLite database filename
        ci : bool, default=False
            Whether to include profile-likelihood confidence intervals
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        cistr = " with ci" if ci else ""
        return self.runQuery(f"db-pareto {dbSpec} {self.dataset_name}{cistr}")
    def dbStream(self, fname, op, budget=1000, fitDb=""):
        ''' PoC: stream the enode table by operator through a SQLite cursor and
        report the total count of matching nodes and the first `budget` matched
        e-classes, to validate O(1)-memory streaming matching.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-stream {dbSpec} {op} {budget}", df=False)
    def dbPushFit(self, fname, fitDb=""):
        ''' Write the current e-graph's fitness/DL metrics into the @fit@ table
        of the SQLite database fname (the graph structure is left intact).

        Parameters
        ----------
        fname : str
            SQLite database filename
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-push-fit {dbSpec} {self.dataset_name}", df=False)
    def dbRefreshFitness(self, fname, fitDb=""):
        ''' Overwrite the in-memory fitness values with those stored in the
        @fit@ table of the SQLite database fname (per e-class, by canonical
        id).

        Parameters
        ----------
        fname : str
            SQLite database filename
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-refresh-fitness {dbSpec} {self.dataset_name}", df=False)
    def dbEqSat(self, fname, iterations=10, ruleset="default", fitDb=""):
        ''' Run equality saturation directly against a lazily loaded (out-of-core)
        e-graph stored in the SQLite database fname. The e-graph stays paged:
        only the structural indexes are resident and every e-class body is
        streamed through the store, so it never all lives in memory at once. The
        rewritten graph is written back to fname.

        Parameters
        ----------
        fname : str
            SQLite database filename
        iterations : int, default=10
            Maximum number of eqsat iterations
        ruleset : str, default="default"
            Rule set to apply: "default" (rewrites) or "params" (rewritesParams)
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-eqsat {dbSpec} {self.dataset_name} {iterations} {ruleset}", df=False)
    def dbEqSatFrontier(self, fname, iterations=10, ruleset="default", fitDb=""):
        ''' Re-saturate only the frontier of a lazily loaded (out-of-core) e-graph:
        the e-classes that were created or merged since the last re-saturation pass
        (tracked in the DB's `frontier` table). The matcher's candidate roots are
        restricted to the frontier, so unchanged parts of the graph are not re-worked.
        The frontier is cleared afterwards. O(1) memory (paged). The pure in-memory
        eggp loop and a full `dbEqSat` are unaffected.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-eqsat-frontier {dbSpec} {self.dataset_name} {iterations} {ruleset}", df=False)
    def dbInsert(self, fname, expr, fitDb=""):
        ''' Local eggp delta: insert a single expression into the DB-backed
        (out-of-core) e-graph in `fname`. Its subgraph is written through and
        content-addressed (existing subexpressions dedup against the live
        tables); every genuinely-new class is marked as part of the
        re-saturation frontier, so a later `dbEqSatFrontier` re-saturates only
        what changed. Returns the root e-class id (as an int), symmetric with
        the in-memory `insert`. O(subgraph) work, O(1) memory.

        Parameters
        ----------
        fname : str
            SQLite database filename
        expr : str
            Expression to insert
        fitDb : str, default=""
            Optional separate fit database filename. If provided, the command
            string embeds it as `fname:fit_path`. If empty, uses fname for both.

        Returns
        -------
        int : the root e-class id
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        v = self.runQuery(f"db-insert {dbSpec} {self.dataset_name} {expr}", df=False)
        return int(str(v).strip())
    def dbSetFit(self, fname, eid, fitness, fitDb=""):
        ''' Record the fitness of a single e-class in `dataset_fit`, so a
        newly-inserted DB expression (from `dbInsert`) can be ranked by the query
        layer once the eggp loop has evaluated it.
        '''
        dbSpec = f"{fname}:{fitDb}" if fitDb else fname
        return self.runQuery(f"db-set-fit {dbSpec} {self.dataset_name} {eid} {fitness}", df=False)
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
        return self.runQuery(f"import {fname} {extractParameters}", df=False)

    def getNEclasses(self, eid, n=10):
        return self.runQuery(f"getNEclasses {n} {eid}")
