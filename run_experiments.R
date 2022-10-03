run_experiments = function( p = 10, n = 100, graph = "random", type = "Gaussian", 
                        vis = FALSE, jump = 1, iter = 100000, burnin = 70000, 
                        save = FALSE, verbose = TRUE )
{


##-----simulate data-------------##

data.sim = bdgraph.sim( p = p, n = n, graph = graph, prob = prob, size = size, type = type, vis = vis )

#----solve data---------#
t1_mpl_bd      = proc.time()	
sample_mpl_bd  = bdgraph.mpl( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save)
time_mpl_bd    = as.numeric( ( proc.time() - t1_mpl_bd )[ 3 ] )

compare_mpl_bd = compare( data.sim, sample_mpl_bd )[,2]
plinks_mpl_bd  = as.matrix( sample_mpl_bd $ p_links, round = 10 )
roc_mpl_bd     = BDgraph::roc( pred = sample_mpl_bd, actual = data.sim )
auc_mpl_bd     = auc( roc_mpl_bd )[ 1 ]

# save solutions results in a list
return(list( time_mpl_bd = time_mpl_bd, compare_mpl_bd = compare_mpl_bd, plinks_mpl_bd = plinks_mpl_bd, roc_mpl_bd = roc_mpl_bd, auc_mpl_bd = auc_mpl_bd,true_g = as.matrix( data.sim $ G ) ) )







}