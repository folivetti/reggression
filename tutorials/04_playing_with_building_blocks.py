from reggression import Reggression
import pandas as pd 

pd.set_option('display.max_colwidth', 200)
N = 5

# Load the dataset and create an empty e-graph
egg = Reggression(dataset="datasets/nikuradse_1.csv", loadFrom="regression_example.egg", loss="MSE") 

# Show the distribution of the top patterns
print(f"""\nDistribution of the top {N} patterns in the e-graph, considering
the average fitness, sorted from the best to the worst, and only those with at least 100 occurrences,
and extracted from the top 5000 expressions:""")
print(egg.distribution(limitedAt=N, dsc=True, byFitness=True, atLeast=100, fromTop=5000))


print(f"""\nDisabling byFitness will sort by frequency of occurrence:""")
print(egg.distribution(limitedAt=N, dsc=False, byFitness=False, atLeast=100, fromTop=5000))

print(f"""\nWe can also count the frequency of a certain pattern, for example 'x0 + v0 ^ v1':""")
print(egg.countPattern("x0 + v0 ^ v1"))

print(f"""\nWe can extract the patterns from one expression:""")
print(egg.extractPattern(110))

print(f"""And, finally, analyse the distribution of tokens of the top-100 expressions:""")
print(egg.distributionOfTokens(top=100))

print(f"""We can also search for the top expressions that present a modularity with size greater than 2.
Modularity here means expressions with repeated sub-expressions. This is shown in LaTeX format:""")
print(egg.modularity(5, filters=[">2"]).Latex)
