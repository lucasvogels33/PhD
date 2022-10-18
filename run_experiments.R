run_experiments = function( p = 10, n = 100, graph = "random", type = "Gaussian", 
                        vis = FALSE, jump = c(1,10), iter = 10000, burnin = 7000, 
                        save = FALSE, cores = 1, verbose = TRUE,g.start="empty")
{

jumpone = jump[1]
jumptwo = jump[2]
##-----simulate data-------------##

data.sim = bdgraph.sim( p = p, n = n, graph = graph, prob = prob, size = size, type = type, vis = vis )

#----solve data using mpl_bd method using first jump value--------#
t1_mpl_bd1      = proc.time()	
sample_mpl_bd1  = bdgraph.mpl( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jumpone, save = save,cores=cores,g.start=g.start)
time_mpl_bd1    = as.numeric( ( proc.time() - t1_mpl_bd1 )[ 3 ] )

compare_mpl_bd1 = compare( data.sim, sample_mpl_bd1 )[,2]
plinks_mpl_bd1  = as.matrix( sample_mpl_bd1 $ p_links, round = 10 )
roc_mpl_bd1     = BDgraph::roc( pred = sample_mpl_bd1, actual = data.sim )
auc_mpl_bd1     = auc( roc_mpl_bd1 )[ 1 ]

#----solve data using mpl_bd method using second jump value--------#

t1_mpl_bd2      = proc.time()	
sample_mpl_bd2  = bdgraph.mpl( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jumptwo, save = save,cores=cores,g.start=g.start)
time_mpl_bd2    = as.numeric( ( proc.time() - t1_mpl_bd2 )[ 3 ] )

compare_mpl_bd2 = compare( data.sim, sample_mpl_bd2 )[,2]
plinks_mpl_bd2  = as.matrix( sample_mpl_bd2 $ p_links, round = 10 )
roc_mpl_bd2     = BDgraph::roc( pred = sample_mpl_bd2, actual = data.sim )
auc_mpl_bd2     = auc( roc_mpl_bd2 )[ 1 ]

#----solve data using bdmcmc method with app ratio of norm constants and first jump value--------#
t1_bd_app1      = proc.time()	
sample_bd_app1  = bdgraph( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jumpone, save = save,cores=cores,g.start=g.start)
time_bd_app1   = as.numeric( ( proc.time() - t1_bd_app1 )[ 3 ] )
	
compare_bd_app1 = compare( data.sim, sample_bd_app1 )[ , 2 ]
plinks_bd_app1  = as.matrix( sample_bd_app1 $ p_links, round = 10 )
roc_bd_app1     = BDgraph::roc( pred = sample_bd_app1, actual = data.sim )
auc_bd_app1     = auc( roc_bd_app1 )[ 1 ]

#----solve data using bdmcmc method with app ratio of norm constants and second jump value--------#
t1_bd_app2      = proc.time()	
sample_bd_app2  = bdgraph( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jumptwo, save = save,cores=cores,g.start=g.start)
time_bd_app2    = as.numeric( ( proc.time() - t1_bd_app2 )[ 3 ] )
	
compare_bd_app2 = compare( data.sim, sample_bd_app2 )[ , 2 ]
plinks_bd_app2  = as.matrix( sample_bd_app2 $ p_links, round = 10 )
roc_bd_app2     = BDgraph::roc( pred = sample_bd_app2, actual = data.sim )
auc_bd_app2     = auc( roc_bd_app2 )[ 1 ]

#----solve data using bdmcmc method with ex ratio of norm constants--------#
 #t1_bd_ex      = proc.time()	
 #sample_bd_ex  = bdgraph( data = data.sim, algorithm = "bd-dmh", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start)
 #time_bd_ex    = as.numeric( ( proc.time() - t1_bd_ex )[ 3 ] )

 #compare_bd_ex = compare( data.sim, sample_bd_ex )[ , 2 ]
 #plinks_bd_ex  = as.matrix( sample_bd_ex $ p_links, round = 10 )
 #roc_bd_ex     = BDgraph::roc( pred = sample_bd_ex, actual = data.sim )
 #auc_bd_ex     = auc( roc_bd_ex )[ 1 ]

#----solve data using rj method with app ratio of norm constants with first jump value--------#
t1_rj_app1      = proc.time()	
sample_rj_app1  = bdgraph( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jumpone, save = save,cores=cores,g.start=g.start)
time_rj_app1    = as.numeric( ( proc.time() - t1_rj_app1 )[ 3 ] )
	
compare_rj_app1 = compare( data.sim, sample_rj_app1 )[ , 2 ]
plinks_rj_app1  = as.matrix( sample_rj_app1 $ p_links, round = 10 )
roc_rj_app1     = BDgraph::roc( pred = sample_rj_app1, actual = data.sim )
auc_rj_app1     = auc( roc_rj_app1 )[ 1 ]

#----solve data using rj method with app ratio of norm constants with second jump value--------#
t1_rj_app2      = proc.time()	
sample_rj_app2  = bdgraph( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jumptwo, save = save,cores=cores,g.start=g.start)
time_rj_app2    = as.numeric( ( proc.time() - t1_rj_app2 )[ 3 ] )
	
compare_rj_app2 = compare( data.sim, sample_rj_app2 )[ , 2 ]
plinks_rj_app2  = as.matrix( sample_rj_app2 $ p_links, round = 10 )
roc_rj_app2     = BDgraph::roc( pred = sample_rj_app2, actual = data.sim )
auc_rj_app2     = auc( roc_rj_app2 )[ 1 ]

#----solve data using rj method with ex ratio of norm constants--------#
#t1_rj_ex     = proc.time()	
#sample_rj_ex  = bdgraph( data = data.sim, algorithm = "rj-dmh", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start)
#time_rj_ex    = as.numeric( ( proc.time() - t1_rj_ex )[ 3 ] )
	
#compare_rj_ex = compare( data.sim, sample_rj_ex )[ , 2 ]
#plinks_rj_ex  = as.matrix( sample_rj_ex $ p_links, round = 10 )
#roc_rj_ex     = BDgraph::roc( pred = sample_rj_ex, actual = data.sim )
#auc_rj_ex     = auc( roc_rj_ex )[ 1 ]

#----solve data using ss method --------#
t1_ss      = proc.time()	
sample_ss  = ssgraph( data = data.sim, iter = iter, burnin = burnin, var1 = 4e-04, var2=1,lambda=1,g.start=g.start,save=save,cores=cores)
time_ss    = as.numeric( ( proc.time() - t1_ss )[ 3 ] )
	
compare_ss = compare( data.sim, sample_ss )[ , 2 ]
plinks_ss  = as.matrix( sample_ss $ p_links, round = 10 )
roc_ss     = BDgraph::roc( pred = sample_ss, actual = data.sim )
auc_ss     = auc( roc_ss)[ 1 ]


# save solutions results in a list
return(list( 
            time_mpl_bd1 = time_mpl_bd1, compare_mpl_bd1 = compare_mpl_bd1, plinks_mpl_bd1 = plinks_mpl_bd1, roc_mpl_bd1 = roc_mpl_bd1, auc_mpl_bd1 = auc_mpl_bd1,
            time_mpl_bd2 = time_mpl_bd2, compare_mpl_bd2 = compare_mpl_bd2, plinks_mpl_bd2 = plinks_mpl_bd2, roc_mpl_bd2 = roc_mpl_bd2, auc_mpl_bd2 = auc_mpl_bd2,
            time_bd_app1 = time_bd_app1, compare_bd_app1 = compare_bd_app1, plinks_bd_app1 = plinks_bd_app1, roc_bd_app1 = roc_bd_app1, auc_bd_app1 = auc_bd_app1,
            time_bd_app2 = time_bd_app2, compare_bd_app2 = compare_bd_app2, plinks_bd_app2 = plinks_bd_app2, roc_bd_app2 = roc_bd_app2, auc_bd_app2 = auc_bd_app2,
            #time_bd_ex = time_bd_ex, compare_bd_ex = compare_bd_ex, plinks_bd_ex = plinks_bd_ex, roc_bd_ex = roc_bd_ex, auc_bd_ex = auc_bd_ex,
            time_rj_app1 = time_rj_app1, compare_rj_app1 = compare_rj_app1, plinks_rj_app1 = plinks_rj_app1, roc_rj_app1 = roc_rj_app1, auc_rj_app1 = auc_rj_app1,
            time_rj_app2 = time_rj_app2, compare_rj_app2 = compare_rj_app2, plinks_rj_app2 = plinks_rj_app2, roc_rj_app2 = roc_rj_app2, auc_rj_app2 = auc_rj_app2,
            #time_rj_ex = time_rj_ex, compare_rj_ex = compare_rj_ex, plinks_rj_ex = plinks_rj_ex, roc_rj_ex = roc_rj_ex, auc_rj_ex = auc_rj_ex, 
            time_ss = time_ss, compare_ss = compare_ss, plinks_ss = plinks_ss, roc_ss = roc_ss, auc_ss = auc_ss,
            true_g = as.matrix( data.sim $ G ) ) )







}