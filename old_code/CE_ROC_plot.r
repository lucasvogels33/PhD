
avg_plot = function(CE=TRUE,ROC=TRUE,p_vec=c(10),n_vec=c(100),graph_vec=c("random"),cut=200,path,rep=50){

    alpha_vector = (0:cut)/cut

    CE_mpl_bd = c(rep(0,cut+1))
    CE_mpl_rj = c(rep(0,cut+1))
    CE_bd = c(rep(0,cut+1))
    CE_rj = c(rep(0,cut+1))
    CE_ss = c(rep(0,cut+1))

    ROC_mpl_bd = c(rep(0,cut+1))
    ROC_mpl_rj = c(rep(0,cut+1))
    ROC_bd = c(rep(0,cut+1))
    ROC_rj = c(rep(0,cut+1))
    ROC_ss = c(rep(0,cut+1))


    count = 0
    for (p in p_vec){
        for (n in n_vec){
            for (graph in graph_vec){
                for (i in 1:rep){
                    count = count + 1
                    load( file = paste0( "result_p", p, "_n", n,"_",graph,"_rep",i,".Rdata", sep = "" ) )
                    trueG = result $true_G
                    if (CE){
                        predictor =  result $ plinks_mpl_bd
                        CE_obj  = CE_plot_cut(response=trueG,predictor=predictor,plot=FALSE,area=FALSE,cut=cut)
                        CE_mpl_bd = CE_mpl_bd + CE_obj $ CE_vec

                        predictor =  result $ plinks_mpl_rj
                        CE_obj  = CE_plot_cut(response=trueG,predictor=predictor,plot=FALSE,area=FALSE,cut=cut)
                        CE_mpl_rj = CE_mpl_rj + CE_obj $ CE_vec

                        predictor =  result $ plinks_bd
                        CE_obj  = CE_plot_cut(response=trueG,predictor=predictor,plot=FALSE,area=FALSE,cut=cut)
                        CE_bd = CE_bd + CE_obj $ CE_vec

                        predictor =  result $ plinks_rj
                        CE_obj  = CE_plot_cut(response=trueG,predictor=predictor,plot=FALSE,area=FALSE,cut=cut)
                        CE_rj = CE_rj + CE_obj $ CE_vec

                        predictor =  result $ plinks_ss
                        CE_obj  = CE_plot_cut(response=trueG,predictor=predictor,plot=FALSE,area=FALSE,cut=cut)
                        CE_ss = CE_ss + CE_obj $ CE_vec
                    }
                    if (ROC){


                    }

                }
            }
        }
    }


    return(list(CE_mpl_bd.....))
}