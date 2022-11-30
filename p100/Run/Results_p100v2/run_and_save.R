#--read the arguments --- #
args = commandArgs(trailingOnly = TRUE)
p = as.numeric(args[1])
n = as.numeric(args[2])
graph = args[3]
iter_rep = as.numeric(args[4])

#--download the necessary libraries and load the run_experiments file --
library( BDgraph )
library (pROC)
library (ssgraph)
source("run_experiments.R")

#--set parameters to simulate data --#
prob = 0.2
type = "Gaussian"
size = NULL
vis = FALSE

#--set parameters to solve data --#
iter = 100000
burnin= 30000
save = TRUE
verbose = FALSE
g.start = "empty"
g.prior = 0.2
cores = 1 
jump = 1 
var1 = 0.02 #ss parameter
var2 = 2 #ss parameter
lambda = 1 #ss parameter

#the results for p=100 did not finish within 5 days. That is why a second run is needed. This second run will run the replications that the first run failed to run.
if ((graph=="cluster")*(n==100)){
    iter_vec = (43:50)
}
if ((graph=="random")*(n==100)){
    iter_vec = (43:50)
}
if ((graph=="scale-free")*(n==100)){
    iter_vec = (48:50)
}
if ((graph=="cluster")*(n==200)){
    iter_vec = (43:50)
}
if ((graph=="random")*(n==200)){
    iter_vec = (44:50)
}
if ((graph=="scale-free")*(n==200)){
    iter_vec = (48:50)
}
if ((graph=="cluster")*(n==1000)){
    iter_vec = (39:50)
}
if ((graph=="random")*(n==1000)){
    iter_vec = (44:50)
}
if ((graph=="scale-free")*(n==1000)){
    iter_vec = (48:50)
}

#-- run experiments
for (i in iter_vec) 
{
    result = run_experiments( p = p, n = n, graph = graph, type = type, vis = vis, 
                      jump = jump, iter = iter, burnin = burnin, save = save, cores = cores,
                      verbose = verbose,g.start =g.start,var1=var1,var2=var2,lambda=lambda,g.prior=g.prior,seed=i) 
    
    #--print data to a Rdata file
    filename = paste0("result_p",p,"_n",n,"_",graph,"_rep",i,".Rdata")
    save( result, file = filename )
}
