from reggression import Reggression
import pandas as pd

reg = Reggression(dataset="test/nikuradse_1.csv", loss="MSE", loadFrom="test/eqs.egraph", refit=True)
print(reg.top(10))

print(reg.top(10, filters=["size < 10"], pattern="v0 ** v1", negate=True))

print(reg.pareto())
