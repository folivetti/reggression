# A Powerful Database for Equations: Using e-graphs and Equality Saturation for Interactive Equation Discovery
by Fabricio Olivetti de França ([Scholar](https://scholar.google.com/citations?user=1FgxaZ0AAAAJ), [Linkedin](https://www.linkedin.com/in/olivetti/))

In the last post we introduced the idea of e-graphs and how it can play an important role with **equation discovery** (aka **symbolic regression**). We also introduced **[eggp](https://github.com/folivetti/eggp)** [[1]](#1), the first equation discovery algorithm that takes advantage of e-graphs by using it as a powerful database system and enforce novelty.

We also briefly introduced **[r🥚ression](https://github.com/folivetti/reggression)** [[2]](#2), a Python tool that allows us to explore the power of e-graphs in different scenario. In this post, we will play a bit more with this tool to show how powerful e-graphs can be as a go to tool for equation discovery.

For a gentle introduction to e-graphs and equality saturation, see the [previous part of this blog post](https://symreg.at/blog/2025/equality-saturation-and-symbolic-regression/).

## First things first

For this experiment, we will use a variation of one of the benchmarks proposed by E. J. Vladislavleva et al[[3]](#3):

$$
e^{-x/1.2}\, x^3  \left(\cos(x)\, \sin(x)^2 - 3.1415\right)
$$

Let's generate data points in the range $[0, 10]$ while adding a bit of Gaussian noise:

```python
x = np.arange(0, 10, 0.05)
y = np.exp(-x/1.2)*x**3*(np.cos(x) \
    * np.sin(x)**2 - 3.1415) \
    + np.random.normal(0, 0.05, x.shape)
```

To make things a bit more interesting for this post, we will use just the middle part for training and the rest as a test set:

```python
lb, ub = 2.1, 5
x_sel = x[(x>lb) & (x<ub)].reshape(-1,1)
y_sel = y[(x>lb) & (x<ub)]
x_ood = x[(x<=lb) | (x>=ub)].reshape(-1, 1)
y_ood = y[(x<=lb) | (x>=ub)]
```

Plotting the training set as red dots and the test set as green dots, we have:

![](figs/vlad.svg)

We are, of course, making things harder for symbolic regression:
1. The relationship is nonlinear.
2. The training set is insufficient to guarantee an unique global optima.

In any case, the purpose here is to show how we can use r🥚ression to explore alternative models.

## Laying the egg 🥚

We can create an initial e-graph for this dataset using `eggp`. As mentioned [in the previous post](https://symreg.at/blog/2025/equality-saturation-and-symbolic-regression/), this algorithm uses e-graphs to enforce the generation of new expressions, avoiding redundancy in the search.

```python
from eggp import EGGP
import pandas as pd

reg = EGGP(gen=200, nPop=200, maxSize=25, \
      nonterminals="add,sub,mul,div,log,power,sin,cos,abs,sqrt", \
      simplify=True, optRepeat=2, optIter=20, folds=2,  \
      dumpTo="vlad.egg")
reg.fit(x_sel, y_sel)
```

Some observations:
- The non-terminal set is large in order to generate many different alternative models.
- We are not running for a large number of iterations, so we could possibly find better models with proper settings.
- The maximum size is larger than the true equation.

We are saving the final e-graph into the file named `vlad.egg` so we can explore it after the search.
Looking at the results we can see the Pareto front with different trade-offs of accuracy and size.

| Math                                                                                                                                                                                                                                 |   size |   loss_train |
|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------:|-------------:|
| $$\theta_{0}$$                                                                                                                                                                                                                       |      1 |   0.360495   |
| $$\left(\theta_{0} + \operatorname{cos}(x_{0})\right)$$                                                                                                                                                                              |      4 |   0.319114   |
| $$\left(\theta_{0} + \frac{\theta_{1}}{x_{0}}\right)$$                                                                                                                                                                               |      5 |   0.318433   |
| $$\left(\theta_{0} + (\left(\theta_{1} - x_{0}\right))^2\right)$$                                                                                                                                                 |      6 |   0.0641624  |
| $$\left(\left(\operatorname{cos}(x_{0}) \cdot \left(x_{0} + \theta_{0}\right)\right) + \theta_{1}\right)$$                                                                                                                           |      8 |   0.0421559  |
| $$\left(\theta_{0} + \left(\operatorname{cos}(\left(\theta_{1} + \operatorname{cos}(x_{0})\right)) \cdot x_{0}\right)\right)$$                                                                                                       |      9 |   0.012507   |
| $$\left(\theta_{0} + \left(\operatorname{cos}(\left(\theta_{1} + \operatorname{cos}(x_{0})\right)) \cdot \left(\theta_{2} \cdot x_{0}\right)\right)\right)$$                                                                         |     11 |   0.00899634 |
| $$\left(\theta_{0} + \left(\operatorname{cos}(\operatorname{cos}(x_{0})) \cdot \left(\theta_{1} \cdot \operatorname{cos}(\left(x_{0} + \theta_{2}\right))\right)\right)\right)$$                                                     |     12 |   0.0046806  |
| $$\left(\theta_{0} - \left(\operatorname{cos}(\operatorname{cos}(x_{0})) \cdot \left(\theta_{1} \cdot \operatorname{cos}(\left (\left(x_{0} + \theta_{2}\right)\right ))\right)\right)\right)$$                                      |     13 |   0.00481255 |
| $$\left(\theta_{0} + \left(\operatorname{cos}(\operatorname{cos}(x_{0})) \cdot \left(\theta_{1} \cdot \operatorname{cos}(\left(\left(\theta_{2} - x_{0}\right) + \theta_{3}\right))\right)\right)\right)$$                           |     14 |   0.00586963 |
| $$\left(\left(\operatorname{cos}(\operatorname{cos}(x_{0})) \cdot \left(\theta_{0} \cdot \operatorname{cos}(\left(\left(\theta_{1} - \left(x_{0} + \theta_{2}\right)\right) + \theta_{3}\right))\right)\right) + \theta_{4}\right)$$ |     16 |   0.00547237 |



## Hatching the egg 🐣

Now, let's load the e-graph into `r🥚ression`:

```python
from reggression import Reggression
egg = Reggression(dataset="vlad.csv", loadFrom="vlad.egg")
```

If we look at the top-5 models, we can see small variations of the top performing with similar fitness (negative MSE) values.

```python
egg.top(5)[["Latex", "Fitness", "Size"]]
```

| Latex                                                                                                                                                                                                                                                                      |     Fitness |   Size |
|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------:|-------:|
| $$\left(\left(\operatorname{cos}(\operatorname{cos}(x)) \cdot \left(\theta_{0} \cdot \operatorname{cos}(\left(\left(\theta_{1} - \left(x + \theta_{2}\right)\right) + \theta_{3}\right))\right)\right) + \theta_{4}\right)$$                           | -0.00415306 |     16 |
| $$\left(\theta_{0} + \left(\operatorname{cos}(\operatorname{cos}(x)) \cdot \left(\operatorname{cos}(\left(\mid\mid\left(\left(x + \theta_{1}\right) + \theta_{2}\right)\mid\mid + \theta_{3}\right)) \cdot \theta_{4}\right)\right)\right)$$ | -0.00425244 |     18 |
| $$\left(\left(\left(\operatorname{cos}(\operatorname{cos}(x)) \cdot \left(\operatorname{cos}(\left(\left(\theta_{0} - \left(x + \theta_{1}\right)\right) + \theta_{2}\right)) \cdot \theta_{3}\right)\right) + \theta_{4}\right) + \theta_{5}\right)$$ | -0.00430326 |     18 |
| $$\left(\theta_{0} + \left(\left(\operatorname{cos}(\operatorname{cos}(x)) \cdot \left(\operatorname{cos}(\left(\mid\left(\sqrt{x}^2 + \theta_{1}\right)\mid + \theta_{2}\right)) \cdot \theta_{3}\right)\right) + \theta_{4}\right)\right)$$     | -0.00430774 |     19 |
| $$\left(\theta_{0} + \left(\operatorname{cos}(\operatorname{cos}(x)) \cdot \left(\theta_{1} \cdot \operatorname{cos}(\left(\left(\theta_{2} - x\right) + \theta_{3}\right))\right)\right)\right)$$                                                     | -0.0043503  |     14 |

Some of these functions behave similarly while others display a different behavior when looking outside of the training region:

![](figs/top5.svg)

We can also plot the best models while limiting the maximum size:

```python
model_top(egg.top(n=10, filters=["size <= 10"]), n, x, y)
```
![](figs/top10.svg)

We can see even more different behaviors compared to the previous plot but, sill, none of them are even close to the correct one  :-( 

Since we are still far from the true expression, let us investigate the distribution of the tokens of the top 1000 generated expressions. 

```python
egg.distributionOfTokens(top=1000)
```
This command returns a table with the number of times each token was used in the top expressions and the average fitness of the expressions that contains such token.  The table is ordered by average fitness (negative MSE).

| Pattern    |   Count |      AvgFit |
|:-----------|--------:|------------:|
| x0         |    2604 | -0.00359749 |
| t0         |    1006 | -0.009312   |
| t1         |     981 | -0.00941213 |
| t2         |     955 | -0.00937039 |
| t3         |     806 | -0.00893546 |
| t4         |     466 | -0.00910986 |
| t5         |     144 | -0.00786632 |
| t6         |       1 | -0.013187   |
| Abs(v0)    |     465 | -0.00810496 |
| Sin(v0)    |      74 | -0.0115615  |
| Cos(v0)    |    3029 | -0.00309273 |
| Sqrt(v0)   |      32 | -0.00845579 |
| Square(v0) |      27 | -0.00967352 |
| Log(v0)    |      10 | -0.00972384 |
| Exp(v0)    |      45 | -0.0118458  |
| Cube(v0)   |      38 | -0.00867039 |
| (v0 + v1)  |    3405 | -0.00275121 |
| (v0 - v1)  |     351 | -0.00848634 |
| (v0 * v1)  |    2139 | -0.0042415  |
| (v0 / v1)  |      68 | -0.00815694 |

Apart from the first rows that displays the terminals, we can see that the absolute value function is frequently used and often contributes to a lower fitness, even though it is not present in the ground-truth expression.

> When we have partial functions such as `log` and `sqrt`, the absolute value can help "fixing" invalid inputs.

Sine and cosine are ranked next, but with cosine being more often used. The exponential is rarely used and particularly with a worse average fitness than the other tokens. The reason for this could be that fitting parameters inside an exponential function can be tricky depending on the initial values.

We can verify that by plotting the top 5 expressions with the pattern $e^{\square_0}\square_1$ with the command:

```python
egg.top(n=n, pattern="exp(v0)*v1")
```

![](figs/top5pat.svg)

as we can see, still not a very good fit, as expected.

## With a little help from my friends 🐣🐤

We can try our luck with another SR method, such as Operon [[4]](#4), and insert the obtained expressions into the e-graph:

```python
from pyoperon.sklearn import SymbolicRegressor
regOp = SymbolicRegressor(objectives=['mse','length'], max_length=20, allowed_symbols='add,sub,mul,div,square,sin,cos,exp,log,sqrt,abs,constant,variable')
regOp.fit(x_sel, y_sel)
f = open("equations.operon", "w")
for eq in regOp.pareto_front_:
  eqstr = regOp.get_model_string(eq['tree'])
  fitness = -eq['mean_squared_error']
  print(f"{eqstr},{fitness},{fitness}", file=f)
f.close()
egg.importFromCSV("equations.operon")
```
Plotting the top-5 expressions we get:

![](figs/top5operon.svg)

Still no luck! But we didn't make things easy for SR anyway!

We can insert the ground-truth expression to see whether the parameter optimization is capable of converging to the true parameters and if the fitness is better than what we have.

```python
egg.insert("exp(x0/t0)*(x0^3)*(cos(x0)*(sin(x0)^2)-t1)")
```

| Latex                                                                                                                                                                                                        |     Fitness | Parameters                                |
|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------:|:------------------------------------------|
| $$\left(\left({x^{3.0}} \cdot \left(\left(\operatorname{cos}(x) \cdot {\operatorname{sin}(x)^{2.0}}\right) + \theta_{0}\right)\right) \cdot e^{\left(x \cdot \theta_{1}\right)}\right)$$ | -0.00256414 | [-3.15, -0.83] |

The answer is YES! We can get the ground-truth expression with enough iterations and a larger amount of luck :-)
Or, we can even resort to adding some constraints [[5]](#5)...

### It's all the same, no matter where you are 🐥🐥🐥

We can also use r🥚ression to check whether two or more expressions are equivalent. Let's say we want to see whether $(x+3)^2 - 9$ and $x(x + 6)$ are the same. 

First, we create an empty e-graph:

```python
newegg = Reggression(dataset="vlad.csv", loss="MSE")
```

Next, we add both expressions while storing their e-class ids:

```python
eid1 = egg.insert("(x0 + 3)**2 - 9").Id.values[0]
eid2 = egg.insert("x0*(x0 + 6)").Id.values[0]
print(eid1, eid2)
> 6, 9
```

Initially, their ids are going to be different, since until now they are distinct to each other as far as the e-graph is concerned.

Now, the main idea is that we run equality saturation to produce all the equivalent forms of each one of these expressions following a set of rules, such as:

$$
(x + y)^2 \rightarrow x^2 + y^2 + 2xy
$$

> If the set of rules are sufficient to produce at least one common expression departing from the first and from the second expressions, they will eventually be merged, and their e-class id will become the same.

We can run some iterations of equality saturation using the command:

```python
egg.eqsat(5)
```

And, now, their ids should be the same!

```python
print("Id of the first equation: \n", egg.report(eid1).loc[0:1, ["Info", "Training"]])
print("Id of the second equation: \n", egg.report(eid2).loc[0:1, ["Info", "Training"]])
> Id of the first equation: 16
> Id of the second equation: 16
```

After running equality saturation, we can also retrieve a sample of the equivalent expressions for that e-class id:

```python
egg.getNExpressions(eid1, 10)
```

Leading to:

$$
((6.0 + x) * x)   \\
((x + 6.0) * x)  \\
((x * 6.0) + (x ^ 2))  \\
((x * 6.0) + (x ^ 2))  \\
(0.0 + ((6.0 + x) * x))  \\
(0.0 + ((x + 6.0) * x))  \\
((2.0 * (x * 3.0)) + (x ^ 2))  \\
((2.0 * (3.0 * x)) + (x ^ 2))  \\
(((x * 3.0) * 2.0) + (x ^ 2))  \\
(((3.0 * x) * 2.0) + (x ^ 2)) \\
$$

This can potentially be used to exploit e-graphs with reward based algorithms such as Monte Carlo Tree Search [[6]](#6) [[7]](#7) and Deep Reinforcement Learning [[8]](#8). Whenever a new expression is produced, we can reinforce the building blocks or producing rules for a subset of the equivalent expressions, increasing the probability of alternative structures in parallel. Another possibility is to input alternative expressions to LLMs [[9]](#9).

## Stay tuned!

As we can see, there is still a vast ground to be explored with the combination of e-graphs and symbolic regression! Stay tuned for our next exciting work on this topic.

## Try it yourself

The full Jupyter Notebook is available [here](https://github.com/folivetti/reggression/tree/main/tutorials/blog)
And rEGGression repository also host some [tutorials](https://github.com/folivetti/reggression/tree/main/tutorials)

## References
<a id="1">[1]</a> de França, Fabrício Olivetti, and Gabriel Kronberger. "Improving Genetic Programming for Symbolic Regression with Equality Graphs." _Proceedings of the Genetic and Evolutionary Computation Conference_. 2025.

<a id="2">[2]</a> de França, Fabrício Olivetti, and Gabriel Kronberger. "rEGGression: an Interactive and Agnostic Tool for the Exploration of Symbolic Regression Models." _Proceedings of the Genetic and Evolutionary Computation Conference_. 2025.

<a id="3">[3]</a> Vladislavleva, Ekaterina J., Guido F. Smits, and Dick Den Hertog. "Order of nonlinearity as a complexity measure for models generated by symbolic regression via pareto genetic programming." _IEEE Transactions on Evolutionary Computation_ 13.2 (2008): 333-349.

<a id="4">[4]</a> Burlacu, Bogdan, Gabriel Kronberger, and Michael Kommenda. "Operon C++ an efficient genetic programming framework for symbolic regression." _Proceedings of the 2020 genetic and evolutionary computation conference companion_. 2020.

<a id="5">[5]</a> Kronberger, Gabriel, et al. "Shape-constrained symbolic regression—improving extrapolation with prior knowledge." _Evolutionary computation_ 30.1 (2022): 75-98.

<a id="6">[6]</a> Kamienny, Pierre-Alexandre, et al. "Deep generative symbolic regression with monte-carlo-tree-search." _International Conference on Machine Learning_. PMLR, 2023.

<a id="7">[7]</a> Sun, Fangzheng, et al. "Symbolic Physics Learner: Discovering governing equations via Monte Carlo tree search." _The Eleventh International Conference on Learning Representations_.

<a id="8">[8]</a> Mundhenk, Terrell, et al. "Symbolic regression via deep reinforcement learning enhanced genetic programming seeding." _Advances in Neural Information Processing Systems_ 34 (2021): 24912-24923.

<a id="9">[9]</a> Shojaee, Parshin, et al. "LLM-SR: Scientific Equation Discovery via Programming with Large Language Models." _The Thirteenth International Conference on Learning Representations_.
