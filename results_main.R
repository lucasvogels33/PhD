## this code will activate results_sub, this will load the code and print it in the output file ##

require ( BDgraph )
require (pROC)

source("results_sub.R")

graph = "random"
n = 10
p = 5
iter_rep = 3
round = 2

load_results(p=p,graph=graph,n=n,report="auc",round=round,iter_rep=iter_rep)

filename = paste0("output.Rdata")
load(file=filename)
cat(mean(result $ auc_mpl_bd), file = "output_2", append = TRUE )

