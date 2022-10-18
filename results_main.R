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

filename = paste0(graph,"_n=",n,"_output_meansd:")

cat("auc values", file = filename,"\n", append = TRUE )
cat("mpl (jump=1) & mpl (jump = 10) & bd (jump=1) & bd (jump=10) & rj (jump=1) & rj (jump=10) & ss ",file = filename,"\n", append = TRUE )
cat(mean(result $ auc_mpl_bd1),"(",sd(result $ auc_mpl_bd1),") & ",
    mean(result $ auc_mpl_bd2),"(",sd(result $ auc_mpl_bd2),") & ",
    mean(result $ auc_bd_app1),"(",sd(result $ auc_bd_app1),") & ",
    mean(result $ auc_bd_app2),"(",sd(result $ auc_bd_app2),") & ",
    mean(result $ auc_rj_app1),"(",sd(result $ auc_rj_app1),") & ",
    mean(result $ auc_rj_app2),"(",sd(result $ auc_rj_app2),") & ",
    mean(result $ auc_ss),"(",sd(result $ auc_ss),")",
    file = filename,"\n", append = TRUE )

cat("time values", file = filename,"\n", append = TRUE )
cat("mpl (jump=1) & mpl (jump = 10) & bd (jump=1) & bd (jump=10) & rj (jump=1) & rj (jump=10) & ss ",file = filename,"\n", append = TRUE )
cat(mean(result $ time_mpl_bd1),"(",sd(result $ time_mpl_bd1),") & ",
    mean(result $ time_mpl_bd2),"(",sd(result $ time_mpl_bd2),") & ",
    mean(result $ time_bd_app1),"(",sd(result $ time_bd_app1),") & ",
    mean(result $ time_bd_app2),"(",sd(result $ time_bd_app2),") & ",
    mean(result $ time_rj_app1),"(",sd(result $ time_rj_app1),") & ",
    mean(result $ time_rj_app2),"(",sd(result $ time_rj_app2),") & ",
    mean(result $ time_ss),"(",sd(result $ auc_ss),")",
    file = filename,"\n", append = TRUE )
