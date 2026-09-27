from reggression import Reggression
import pandas as pd 

pd.set_option('display.max_colwidth', 100)
df = pd.read_csv("datasets/nikuradse_1.csv")
egg = Reggression(dataset="datasets/nikuradse_1.csv", loss="MSE") 

from collections import Counter

def get_list_difference(list_a, list_b):
    # Count occurrences of each element in both lists
    counts_a = Counter(list_a)
    counts_b = Counter(list_b)
    
    # Subtracting Counters keeps only the remaining counts
    # (e.g., if '1' appears twice in A and once in B, the result is one '1')
    diff = counts_a - counts_b
    diff2 = counts_b - counts_a

    l1 = list(diff.elements())
    l2 = list(diff2.elements())
    
    # Return the elements as a list
    if len(l1) > len(l2):
        return l1
    return l2
    
def jaccard(e1, e2, dbg=False, k=10):
    egg = Reggression(dataset="datasets/nikuradse_1.csv", loss="MSE") 
    id1 = egg.insert(e1).values[0][0]
    id2 = egg.insert(e2).values[0][0]
    egg.eqsat(k)
    # ids might change after eqsat
    top2 = egg.top(10)
    if dbg:
        print(top2)
        print(egg.subtrees(id1))
        print(egg.subtrees(id2))
    #id1 = top2.values[0][0] if id1 not in top2.values[:,0] else id1
    #id2 = top2.values[1][0] if top2.shape[0] > 1 else id1
    ecs1 = egg.subtrees(id1).values
    ecs2 = egg.subtrees(id2).values
    print(egg.getNEclasses(id1))

    counts1 = [(e[0], 2 if isinstance(e[1], str) and ' ' in e[1] else 1 if isinstance(e[1], str) else 0) for e in ecs1]
    counts2 = [(e[0], 2 if isinstance(e[1], str) and ' ' in e[1] else 1 if isinstance(e[1], str) else 0) for e in ecs2]
    ids1 = set(counts1)
    ids2 = set(counts2)
    excess = (ids1 | ids2) - (ids1 & ids2)
    # using set = node graph edit distance
    node_dist = len(excess)
    # using set but counting two for any node with space = edge edit distance
    edge_dist = sum([e[1] for e in excess])
    # no set = tree edit distance
    tree_dist = len(get_list_difference(counts1, counts2))
    
    #ecs1 = set(egg.subtrees(id1)['Id'].values)
    #ecs2 = set(egg.subtrees(id2)['Id'].values)
    jaccard = len(ids1 & ids2)/len(ids1 | ids2)#, ecs1, ecs2
    return jaccard, node_dist, edge_dist, tree_dist

jaccard("(x0 + (x1 + (sin(x0) + cos(x0))))", "(x0 + (x1 + (sin(x0) + cos(x1))))")
