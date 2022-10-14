## this code will activate results_sub, this will load the code and print it in the output file ##

require ( BDgraph )
require (pROC)

source("results_sub.R")

args = commandArgs(trailingOnly = TRUE)
p = as.numeric(args[1])
n = as.numeric(args[2])
graph = args[3]
iter_rep = as.numeric(args[4])
jump = as.numeric(args[5])

round = 2

result = load_results(p=p,graph=graph,n=n,round=round,jump=jump,iter_rep=iter_rep)

filename = paste0(graph,"_output_mean")

cat("auc of mpl_bd ", mean(result $ auc_mpl_bd),"(",sd(result $ auc_mpl_bd),")", file = filename,"\n", append = TRUE )
cat("time of mpl_bd ", mean(result $ time_mpl_bd),"(",sd(result $ time_mpl_bd),")", file = filename,"\n", append = TRUE )
cat("auc of bd_app ", mean(result $ auc_bd_app),"(",sd(result $ auc_bd_app),")", file = filename,"\n", append = TRUE )
cat("time of bd_app ", mean(result $ time_bd_app),"(",sd(result $ time_bd_app),")", file = filename,"\n", append = TRUE )
#cat("auc of bd_ex ", mean(result $ auc_bd_ex),"(",sd(result $ auc_bd_ex),")", file = filename,"\n", append = TRUE )
#cat("time of bd_ex ", mean(result $ time_bd_ex),"(",sd(result $ time_bd_ex),")", file = filename,"\n", append = TRUE )
cat("auc of rj_app ", mean(result $ auc_rj_app),"(",sd(result $ auc_rj_app),")", file = filename,"\n", append = TRUE )
cat("time of rj_app ", mean(result $ time_rj_app),"(",sd(result $ time_rj_app),")", file = filename,"\n", append = TRUE )
#cat("auc of rj_ex ", mean(result $ auc_rj_ex),"(",sd(result $ auc_rj_ex),")", file = filename,"\n", append = TRUE )
#cat("time of rj_ex ", mean(result $ time_rj_ex),"(",sd(result $ time_rj_ex),")", file = filename,"\n", append = TRUE )
cat("auc of ss ", mean(result $ auc_ss),"(",sd(result $ auc_ss),")", file = filename,"\n", append = TRUE )
cat("time of ss ", mean(result $ time_ss),"(",sd(result $ time_ss),")", file = filename,"\n", append = TRUE )

