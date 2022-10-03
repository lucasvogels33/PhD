## this code will activate results_sub, this will load the code and print it in the output file ##

require ( BDgraph )
require (pROC)

source("results_sub.R")

args = commandArgs(trailingOnly = TRUE)
p = as.numeric(args[1])
n = as.numeric(args[2])
graph = args[3]
iter_rep = as.numeric(args[4])

round = 2

result = load_results(p=p,graph=graph,n=n,round=round,iter_rep=iter_rep)

filename = paste0(graph,"_output_mean")

cat("auc ", mean(result $ auc_mpl_bd),"(",sd(result $ auc_mpl_bd),")", file = filename,"\n", append = TRUE )
cat("time ", mean(result $ time_mpl_bd),"(",sd(result $ time_mpl_bd),")", file = filename,"\n", append = TRUE )

