from reggression import Reggression
import pandas as pd 

pd.set_option('display.max_colwidth', 200)
N = 5

# Load the dataset and create an empty e-graph
egg = Reggression(dataset="datasets/nikuradse_1.csv", loadFrom="regression_example.egg", loss="MSE") 

# Return the top N expressions
print(f"""\nIn rEGGression, pattern matching is writen as a mathematical expression where
x0 .. xn is one of the input variables
t0 .. tn is one of the numerical parameters
v0 .. vn is a match all pattern.
Examples:

x0 + t0 * x1 will match this exact expression, if it exists.
x0 + v0 * x1 will match expressions such as:
   - x0 + t0 * x1
   - x0 + x0 * x1
   - x0 + sin(x0 + t0 * x1) * x1
among others.

v0 ^ v0 will match:
    - x0 ^ x0
    - log(x0 + t0) ^ log(x0 + t0)
among others.

v0 ^ v1 will match:
    - x0 ^ x1
    - log(x0 + t0) ^ log(x0 + t0)
    - log(x0 + t0) ^ sin(t1 * x1)
among others.

    Best {N} expressions according to MSE that matches the pattern log(v0) ^ v1:""")
print(egg.top(N, pattern="log(v0) ^ v1")[['Expression', 'Fitness', 'Size']])

print(f"\nTop {N} expressions with pattern v0 ^ v0:")
print(egg.top(N, pattern="v0 ^ v0")[['Expression', 'Fitness', 'Size']])

print(f"""\nThe argument `isRoot` when True will match only those expressions with a root matching the pattern. For example:
x0 + v0 * x1 will match expressions such as:
    - x0 + t0 * x1
    - x0 + x0 * x1
    - x0 + sin(x0 + t0 * x1) * x1
But it will NOT match:
    - log(x0 + t0 * v1)
    - (x0 + t0 * v1) * t1
    - x0 + t0 * v1 + t1
Top {N} expressions with pattern log(v0) ^ v1 at the root:""")
print(egg.top(N, pattern="log(v0) ^ v1", isRoot=True)[['Expression', 'Fitness', 'Size']])

print(f"""\nThe argument `negate` when True will retrieve the expressions NOT matching the pattern.
Top {N} expressions not matching x0 + v0 * x1 (notice that the matching is in this exact\nother, so this can show the pattern x1 * v0 + x0, for example):""")
print(egg.top(N, pattern="x0 + v0 * x1", negate=True)[['Expression', 'Fitness', 'Size']])

print(f"\nTop {N} expressions not matching log(v0):")
print(egg.top(N, pattern="log(v0)", negate=True)[['Expression', 'Fitness', 'Size']])
