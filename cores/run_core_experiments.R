run_experiments = function( p = 10, n = 100, graph = "random", type = "Gaussian", 
                        vis = FALSE, jump = 1, iter = 10000, burnin = 7000, 
                        save = TRUE, cores = 1, verbose = TRUE,g.start="empty",var1=var1,var2=var2,lambda=lambda,g.prior=0.2,seed=1)
{

##-----simulate data-------------##
seed = 10*seed

set.seed(seed)
data.sim = bdgraph.sim( p = p, n = n, graph = graph, prob = prob, size = size, type = type, vis = vis )

#----solve data using mpl_bd method --##
set.seed(seed+1)
t1_mpl_bd      = proc.time()	#start time
sample_mpl_bd  = bdgraph.mpl( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_mpl_bd    = as.numeric( ( proc.time() - t1_mpl_bd)[ 3 ] ) #end and save time


#----solve data using mpl_rj method --##
set.seed(seed+2)
t1_mpl_rj      = proc.time()	#start time
sample_mpl_rj  = bdgraph.mpl( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_mpl_rj    = as.numeric( ( proc.time() - t1_mpl_rj)[ 3 ] ) #end and save time

#----solve data using bdmcmc method with app ratio of norm constants---#
set.seed(seed+3)
t1_bd      = proc.time()	
sample_bd  = bdgraph( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_bd   = as.numeric( ( proc.time() - t1_bd )[ 3 ] )

#----solve data using rj method with app ratio of norm constants with first jump value--------#
set.seed(seed+4)
t1_rj     = proc.time()	
sample_rj  = bdgraph( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_rj    = as.numeric( ( proc.time() - t1_rj )[ 3 ] )
	
#----solve data using ss method --------#
set.seed(seed+5)
t1_ss      = proc.time()	
sample_ss  = ssgraph( data = data.sim, iter = iter, burnin = burnin, var1 = var1, var2=var2,lambda=lambda,g.start=g.start,save=save,cores=cores,g.prior=g.prior)
time_ss    = as.numeric( ( proc.time() - t1_ss )[ 3 ] )
	
# save solutions results in a list
return(list(time_mpl_bd = time_mpl_bd, 
            time_mpl_rj = time_mpl_rj, 
            time_bd = time_bd, 
            time_rj = time_rj, 
            time_ss = time_ss)
}