from reggression import Reggression
import pandas as pd 

pd.set_option('display.max_colwidth', 200)

# Load the dataset and create an empty e-graph
egg = Reggression(dataset="datasets/nikuradse_1.csv", loadFrom="regression_example.egg", loss="MSE") 

# Return the top 10 expressions
print("\nBest 10 expressions according to MSE:")
print(egg.top(10)[['Expression', 'Fitness', 'Size']])

# Return the top 10 expressions with a size smaller than 8
print("\nBest 10 expressions according to MSE smaller than 8 nodes:")
print(egg.top(10, filters=["size < 8"])[['Expression', 'Fitness', 'Size']])

print("\nBest 10 expressions according to MSE with a single parameter:")
print(egg.top(10, filters=["parameters = 1"])[['Expression', 'Fitness', 'Size']])

print("\nBest 10 expressions according to MSE with a cost equal or less than 10.")
print("""The cost is the sum of individual costs for each node type:
Variables has cost 1
Constants and Parameters have cost 3
Binary operators have cost 2
Unary operators have cost 3"""
)
print(egg.top(10, filters=["cost <= 10"])[['Expression', 'Fitness', 'Size']])

print("\nBest 10 expressions according to description length (DL). Notice that DL to be correct must be calculated with a likelihood loss function:")
print(egg.top(10, criteria="dl")[['Expression', 'Fitness', 'Size']])

