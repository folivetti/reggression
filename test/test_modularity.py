from reggression import Reggression

egfinal = Reggression(dataset="nikuradse_1.csv", loadFrom="final.egraph", simpleOutput=True)
df = egfinal.top(5)
print(df)
modulars = egfinal.modularity(5, filters=["> 5"])
print(modulars)
