from reggression import Reggression
import pandas as pd

pd.set_option('display.max_colwidth', 500)
N = 5

print("Let's start with an empty e-graph:")
egg = Reggression(dataset="datasets/nikuradse_1.csv", loss="MSE")

'''
print("\nWe can insert new expressions at will:")
print(egg.insert("x0^(t0 * x1)")[["Expression", "Fitness", "Size"]])
print(egg.insert("(t0 * x1)*log(x0)")[["Expression", "Fitness", "Size"]])
print(egg.insert("sqrt((x1+x0)*(x1+x0))")[["Expression", "Fitness", "Size"]])
print(egg.insert("log(exp(x0*t0))")[["Expression", "Fitness", "Size"]])


print("\nWe can run equality saturation to find equivalent expressions for the expressions of this e-graph\nBe mindful that it can lead to a memory leakage if there are many expressions or many eqsat iterations:")
egg.eqsat(15)

print("Let's see 15 alternative expressions from 'sqrt((t0+x0)*(t0+x0))':")
print(egg.getNExpressions(9, 15).Expression)
print("\nLet's see 5 alternative expressions from 'log(exp(x0+t0))':")
print(egg.getNExpressions(12, 5).Expression)
'''
print("\nTo check whether two expressions are equivalent, we can insert them and see if they belong to the same e-class after eqsat\nFor example, (x0 + 3)**2 - 9 and x0*(x0 + 6):")
eid1 = egg.insert("(x0 + 3)**2 - 9").Id.values[0]
eid2 = egg.insert("x0*(x0 + 6)").Id.values[0]
print("After inserting, their eclass ids are different:", eid1, eid2)
print("\nWe will run 5 iterations of equality saturation. Be mindful because some expressions can produce many alternative representations and lead to memory leakage.")
egg.eqsat(5)
print("A sample of ten equivalent expressions of (x0 + 3)**2 - 9:")
print("\n".join(sorted(egg.getNExpressions(eid1, 500).Expression.values, key=len)[:10]))
print("A sample of three equivalent expressions of x0*(x0 + 6):")
print("\n".join(sorted(egg.getNExpressions(eid2, 500).Expression.values, key=len)[:10]))
print("\nNow, let's see their eclass ids again:")
print("Id of the first equation: \n", egg.report(eid1).loc[0:1, ["Info", "Training"]])
print("Id of the second equation: \n", egg.report(eid2).loc[0:1, ["Info", "Training"]])
