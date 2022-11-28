
library(BDgraph)
library(ssgraph)
source("run_core_experiments.R")

#--set parameters to simulate data --#
prob = 0.2
type = "Gaussian"
size = NULL
vis = FALSE

#--set parameters to solve data --#
iter = 1000
burnin= 500
save = FALSE
verbose = FALSE
g.start = "empty"
g.prior = 0.2
cores = 1 
jump = 1 
var1 = 0.02 #ss parameter
var2 = 2 #ss parameter
lambda = 1 #ss parameter

##--
core_vec = c(1,8,15)
p_vec = c(10,50,100)
graph_vec = c("random","cluster","scale-free")
rep_vec = c(1,2,3)

for (p in p_vec){
  n = 10*p
  for (cores in core_vec){
    for (graph in graph_vec){
      for (i in rep_vec){
        result = run_experiments( p = p, n = n, graph = graph, type = type, vis = vis, 
            jump = jump, iter = iter, burnin = burnin, save = save, cores = cores,
            verbose = verbose,g.start =g.start,var1=var1,var2=var2,lambda=lambda,g.prior=g.prior,seed=i) 
            
        filename = paste0("result_p",p,"_n",n,"_",graph,"_cores",cores,"_rep",i,".Rdata")
        save( result, file = filename)
        }
      }
  }
}