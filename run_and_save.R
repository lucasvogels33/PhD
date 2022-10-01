#--read the arguments --- #
args = commandArgs(trailingOnly = TRUE)
p = as.numeric(args[1])
n = as.numeric(args[2])
graph = args[3]
iter_rep = as.numeric(args[4])

#--print the size of the problem in the command prompt 
cat("the size of the problems is: ",n,"\n")

#--download the necessary libraries and load the run_experiments file --
library( BDgraph )
library (pROC)
source("run_experiments.R")

#--set parameters to simulate data --#
prob = 0.2
type = "Gaussian"
size = NULL
vis = FALSE

#--set parameters to solve data --#
iter = 500
burnin= iter/2
jump = 1
save = FALSE
verbose = TRUE

#-- run experiments
for (i in 1:iter_rep) 
{
    result = run_experiments( p = p, n = n, graph = graph, type = type, vis = vis, 
                      jump = jump, iter = iter, burnin = burnin, save = save, 
                      verbose = verbose )

    #--print data to a Rdata file
    filename = paste0("result_p",p,"_n",n,"_",graph,"_rep",i,".Rdata")
    save( result, file = filename )
}
