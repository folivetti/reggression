"""Regression test for the GHC multi-RTS / late-dlopen crash.

GHC hosts a single RTS per process and does not safely support a second
Haskell shared library being dlopen'd after the RTS has been used.  Importing
``reggression`` must therefore proactively map the sibling ``eggp`` binding,
so that a later ``import eggp`` does not load a Haskell library into an
already-running RTS.
"""

import importlib.util
import subprocess
import sys

import pytest


@pytest.mark.skipif(
    not sys.platform.startswith("linux"),
    reason="uses /proc/self/maps to check the mapped shared libraries",
)
def test_import_reggression_preloads_eggp_binding():
    if importlib.util.find_spec("eggp") is None:
        pytest.skip("eggp is not installed")

    code = "import reggression; print(open('/proc/self/maps').read())"
    maps = subprocess.check_output([sys.executable, "-c", code], text=True)
    assert "eggp/_binding" in maps
    assert "reggression/_binding" in maps
