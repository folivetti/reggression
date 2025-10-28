from reggression import Reggression
from pyoperon.sklearn import SymbolicRegressor
import bingo.symbolic_regression.symbolic_regressor as bingo
import pandas as pd
import numpy as np

pd.set_option('display.max_colwidth', 500)
N = 5

df = pd.read_csv("datasets/nikuradse_1.csv")
X = df[['r_k','log_Re']].values
y = df['target'].values

print("Let's fit Operon SR model first and save the pareto front into a file:")
regOp = SymbolicRegressor()
regOp.fit(X, y)
f = open("equations.operon", "w")
for eq in regOp.pareto_front_:
  eqstr = regOp.get_model_string(eq['tree'])
  print(f"{eqstr},,1.0", file=f)
f.close()

print("Now, let's fit Bingo model and save the pareto front into a file:")
regBingo = bingo.SymbolicRegressor(max_time=60)
regBingo.fit(X, y)
f = open("equations.bingo", "w")
for eq in regBingo.best_pop:
  print(f"{eq},,1.0", file=f)
f.close()

print("Now, let's load it into the e-graph to further explore them, the flag True parse numeric values into fitting parameters:")
egg = Reggression(dataset="datasets/nikuradse_1.csv", loss="MSE")
egg.importFromCSV("equations.bingo", True)
egg.importFromCSV("equations.operon", True)

print("Let's save the e-graph and reload it to refit the expressions:")
egg.save("merged.egg")
egg = Reggression(dataset="datasets/nikuradse_1.csv", loadFrom="merged.egg", loss="MSE", refit=True)

print(f"""\nTop-{N} expressions in the e-graph after importing from CSV files:""")
print(egg.top(N)[["Expression", "Fitness", "Size"]])

print(f"""\nDistribution of the top {N} patterns in the e-graph:""")
print(egg.distribution(limitedAt=N, dsc=True, byFitness=True, atLeast=5, fromTop=5000))

print(f"""And, finally, analyse the distribution of tokens of the top-100 expressions:""")
print(egg.distributionOfTokens())

print(f"""We can also search for the top expressions that present a modularity with size greater than 2.
Modularity here means expressions with repeated sub-expressions. This is shown in LaTeX format:""")
print(egg.modularity(5, filters=[">1"]).Latex)
