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

from ._binding import (
    unsafe_hs_reggression_version,
    unsafe_hs_reggression_main,
    unsafe_hs_reggression_run,
    unsafe_hs_reggression_init,
    unsafe_hs_reggression_exit,
)

VERSION: str = "1.0.7"


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

def reggression_run(myCmd : str, dataset : str, testData : str, loss : str, loadFrom : str, dumpTo : str, parseCSV : str, parseParams : int, calcDL : int, calcFit : int) -> str:
    with hs_rts_init():
        return unsafe_hs_reggression_run(myCmd, dataset, testData, loss, loadFrom, dumpTo, parseCSV, parseParams, calcDL, calcFit)

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
    def __init__(self, dataset, testData="", loss="MSE", loadFrom="", parseCSV="", parseParams=True, refit=False):
        losses = ["MSE", "Gaussian", "Bernoulli", "Poisson"]
        if loss not in losses:
            raise ValueError('loss must be one of ', losses)
        if len(loadFrom) == 0 and len(parseCSV) == 0:
            raise ValueError('you must provide either a "loadFrom" or "parseCSV" value')
        if len(dataset) == 0:
            raise ValueError('you must provide a dataset filename')
        if not os.path.isfile(dataset):
            raise ValueError('dataset does not exist')
        if not os.path.isfile(loadFrom) and not os.path.isfile(parseCSV):
            raise ValueError('egraph or CSV file do not exist')
        if not isinstance(parseParams, bool):
            raise ValueError('parseParams must be a boolean')
        if not isinstance(refit, bool):
            raise ValueError('refit must be a boolean')
        self.dataset = dataset
        self.testData = testData
        self.loss = loss
        self.loadFrom = loadFrom
        self.parseCSV = parseCSV
        self.parseParams = int(parseParams)
        self.refit = refit

        self.temp_file = tempfile.NamedTemporaryFile(mode='w+', newline='', delete=False,  dir=os.getcwd(), suffix='.egraph')
        self.tempname = self.temp_file.name
        self.temp_file.close()
        print("Calculating DL...")
        reggression_run("top 10", self.dataset, self.testData, self.loss, self.loadFrom, self.tempname, self.parseCSV, self.parseParams, 1, self.refit)
        print("Welcome to r🥚ression")
    def __del__(self):
        ''' remove temporary e-graph file before ending the program '''
        os.remove(self.tempname)
    def runQuery(self, query, df=True):
        '''  Runs a query.

        Parameters
        ----------

        query : str
            A string with the query to send to r🥚ression.

        df : bool, default=True
            Whether the query returns a DataFrame.
        '''
        csv_data = reggression_run(query, self.dataset, self.testData, self.loss, self.tempname, self.tempname, self.parseCSV, self.parseParams, 0, 0)
        if df and len(csv_data) > 0:
            csv_io = StringIO(csv_data.strip())
            self.results = pd.read_csv(csv_io, header=0)
        else:
            self.results = pd.DataFrame() if df else csv_data
        return self.results

    def top(self, n=5, filters=[], criteria="fitness", pattern="", isRoot=False, negate=False):
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
        query = f"top {n} {filters_str} by {criteria} {patmatch}"
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
    def countPattern(self, pattern):
        ''' Count the frequency of a certain pattern

        Parameters
        ----------

        pattern : str
            Pattern that should be counted
        '''
        query = f"count-pattern {pattern}"
        return self.runQuery(query, df=False)
    def report(self, n):
        ''' Detailed report of e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        '''
        return self.runQuery(f"report {n}")
    def optimize(self, n):
        ''' (re)optimize e-class n

        Parameters
        ----------
        n : int
            E-class id of the e-class
        '''
        return self.runQuery(f"optimize {n}")
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
    def pareto(self, byFitness=True):
        ''' Return the Pareto front of accuracy x size

        Parameters
        ----------
        byFitness : bool, default=True
            Whether the first objective is fitness or description length
        '''
        return self.runQuery(f"pareto {'by fitness' if byFitness else 'by dl'}")
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
