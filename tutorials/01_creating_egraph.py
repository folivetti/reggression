from eggp import EGGP
from reggression import Reggression
import pandas as pd 

pd.set_option('display.max_colwidth', 100)
df = pd.read_csv("datasets/nikuradse_1.csv")

# We will use eggp to fit a symbolic regression model to the data and save the search history
# as an e-graph file (here we are using .egg extension)
model = EGGP(gen=100, nPop=100, maxSize=15, nTournament=5, pc=0.8, pm=0.2, nonterminals='add,sub,mul,div,power,exp,log', loss='MSE', optIter=100, optRepeat=5, folds=2, max_time=120, simplify=False, dumpTo='regression_example.egg')
model.fit(df[['r_k', 'log_Re']], df['target'])

# Load the dataset and create an empty e-graph
egg = Reggression(dataset="datasets/nikuradse_1.csv", loadFrom="regression_example.egg", loss="MSE") 

# Return the top 10 expressions
print(egg.top(10)[['Expression', 'Fitness', 'Size']])
