## this code will activate results_sub, this will load the code and print it in the output file ##

require ( BDgraph )
require (pROC)

source("results_sub.R")

graph = "random"
n = 10
p = 5
round = 2

print_results(p=p,graph=graph,n=n,report="auc",round=round)
