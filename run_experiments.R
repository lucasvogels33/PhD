run_experiments = function( p = 10, n = 100, graph = "random", type = "Gaussian", 
                        vis = FALSE, jump = 1, iter = 10000, burnin = 7000, 
                        save = FALSE, verbose = TRUE )
{


##-----simulate data-------------##

data.sim = bdgraph.sim( p = p, n = n, graph = graph, prob = prob, size = size, type = type, vis = vis )

#----solve data using mpl_bd method--------#
t1_mpl_bd      = proc.time()	
sample_mpl_bd  = bdgraph.mpl( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save)
time_mpl_bd    = as.numeric( ( proc.time() - t1_mpl_bd )[ 3 ] )

compare_mpl_bd = compare( data.sim, sample_mpl_bd )[,2]
plinks_mpl_bd  = as.matrix( sample_mpl_bd $ p_links, round = 10 )
roc_mpl_bd     = BDgraph::roc( pred = sample_mpl_bd, actual = data.sim )
auc_mpl_bd     = auc( roc_mpl_bd )[ 1 ]

#----solve data using bdmcmc method with app ratio of norm constants--------#
t1_bd_app      = proc.time()	
sample_bd_app  = bdgraph( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save)
time_bd_app    = as.numeric( ( proc.time() - t1_bd_app )[ 3 ] )
	
compare_bd_app = compare( data.sim, sample_bd_app )[ , 2 ]
plinks_bd_app  = as.matrix( sample_bd_app $ p_links, round = 10 )
roc_bd_app     = BDgraph::roc( pred = sample_bd_app, actual = data.sim )
auc_bd_app     = auc( roc_bd_app )[ 1 ]

#----solve data using bdmcmc method with ex ratio of norm constants--------#
# t1_bd_ex      = proc.time()	
# sample_bd_ex  = bdgraph( data = data.sim, algorithm = "bd-dmh", iter = iter, burnin = burnin, jump = jump, save = save)
# time_bd_ex    = as.numeric( ( proc.time() - t1_bd_ex )[ 3 ] )
	
# compare_bd_ex = compare( data.sim, sample_bd_ex )[ , 2 ]
# plinks_bd_ex  = as.matrix( sample_bd_ex $ p_links, round = 10 )
# roc_bd_ex     = BDgraph::roc( pred = sample_bd_ex, actual = data.sim )
# auc_bd_ex     = auc( roc_bd_ex )[ 1 ]

#----solve data using rj method with app ratio of norm constants--------#
t1_rj_app      = proc.time()	
sample_rj_app  = bdgraph( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jump, save = save)
time_rj_app    = as.numeric( ( proc.time() - t1_rj_app )[ 3 ] )
	
compare_rj_app = compare( data.sim, sample_rj_app )[ , 2 ]
plinks_rj_app  = as.matrix( sample_rj_app $ p_links, round = 10 )
roc_rj_app     = BDgraph::roc( pred = sample_rj_app, actual = data.sim )
auc_rj_app     = auc( roc_rj_app )[ 1 ]

#----solve data using rj method with ex ratio of norm constants--------#
#t1_rj_ex     = proc.time()	
#sample_rj_ex  = bdgraph( data = data.sim, algorithm = "rj-dmh", iter = iter, burnin = burnin, jump = jump, save = save)
#time_rj_ex    = as.numeric( ( proc.time() - t1_rj_ex )[ 3 ] )
	
#compare_rj_ex = compare( data.sim, sample_rj_ex )[ , 2 ]
#plinks_rj_ex  = as.matrix( sample_rj_ex $ p_links, round = 10 )
#roc_rj_ex     = BDgraph::roc( pred = sample_rj_ex, actual = data.sim )
#auc_rj_ex     = auc( roc_rj_ex )[ 1 ]


# save solutions results in a list
return(list( time_mpl_bd = time_mpl_bd, compare_mpl_bd = compare_mpl_bd, plinks_mpl_bd = plinks_mpl_bd, roc_mpl_bd = roc_mpl_bd, auc_mpl_bd = auc_mpl_bd,
            time_bd_app = time_bd_app, compare_bd_app = compare_bd_app, plinks_bd_app = plinks_bd_app, roc_bd_app = roc_bd_app, auc_bd_app = auc_bd_app,
            #time_bd_ex = time_bd_ex, compare_bd_ex = compare_bd_ex, plinks_bd_ex = plinks_bd_ex, roc_bd_ex = roc_bd_ex, auc_bd_ex = auc_bd_ex,
            time_rj_app = time_rj_app, compare_rj_app = compare_rj_app, plinks_rj_app = plinks_rj_app, roc_rj_app = roc_rj_app, auc_rj_app = auc_rj_app,
            #time_rj_ex = time_rj_ex, compare_rj_ex = compare_rj_ex, plinks_rj_ex = plinks_rj_ex, roc_rj_ex = roc_rj_ex, auc_rj_ex = auc_rj_ex, 
            true_g = as.matrix( data.sim $ G ) ) )







}