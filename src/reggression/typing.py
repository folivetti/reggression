import sys

from typing import List

if sys.version_info < (3, 8):
    from typing_extensions import Protocol
else:
    from typing import Protocol


class Session(Protocol):
    def version(self) -> str: ...
    def main(self, args: List[str] = []) -> int: ...
    def reggression_run(self, myCmd : str, dataset : str, testData : str, loss : str, loadFrom : str, dumpTo : str, parseCSV : str, parseParams : int, calcDL : int, calcFit : int) -> str: ...
