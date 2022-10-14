## this file contains the function print_results. This function gets the data from the Rdata files and prints it ##

load_results = function(p=5,graph="random",n=10,jump=1,round=2,iter_rep=10)
{
    auc_mpl_bd = c(rep(0,iter_rep))
    time_mpl_bd = c(rep(0,iter_rep))
    auc_bd_app= c(rep(0,iter_rep))
    time_bd_app = c(rep(0,iter_rep))
    auc_bd_ex = c(rep(0,iter_rep))
    time_bd_ex = c(rep(0,iter_rep))
    auc_rj_app = c(rep(0,iter_rep))
    time_rj_app = c(rep(0,iter_rep))
    auc_rj_ex = c(rep(0,iter_rep))
    time_rj_ex = c(rep(0,iter_rep))

    
    filename_paste = paste0(graph,"_output_all")
    
    for (i in 1:iter_rep){
        filename_copy = paste0("result_p",p,"_n",n,"_",graph,"_jump",jump,"rep",i,".Rdata")
        load(file = filename_copy)

        auc_mpl_bd[i] = result $ auc_mpl_bd
        time_mpl_bd[i] = result $ time_mpl_bd
        auc_bd_app[i]= result $ auc_bd_app
        time_bd_app[i] = result $ time_bd_app
        #auc_bd_ex[i] = result $ auc_bd_ex
        #time_bd_ex[i] = result $ time_bd_ex
        auc_rj_app[i] = result $ auc_rj_app
        time_rj_app[i] = result $ time_rj_app
        #auc_rj_ex[i] = result $ auc_rj_ex
        #time_rj_ex[i] = result $ time_rj_ex
        auc_ss[i] = result $ auc_ss
        time_ss[i] = result $ time_ss

        cat("auc of rep ",i," of the mpl_bd method is ",auc_mpl_bd[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the mpl_bd method is ",time_mpl_bd[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the bd_app method is ",auc_bd_app[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the bd_app method is ",time_bd_app[i]," ", file = filename_paste,"\n", append = TRUE)
        #cat("auc of rep ",i," of the bd_ex method is ",auc_bd_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        #cat("time of rep ",i," of the bd_ex method is ",time_bd_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the rj_app method is ",auc_rj_app[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the rj_app method is ",time_rj_app[i]," ", file = filename_paste,"\n", append = TRUE)
        #cat("auc of rep ",i," of the rj_ex method is ",auc_rj_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        #cat("time of rep ",i," of the rj_ex method is ",time_rj_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the ss method is ",auc_ss[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the ss method is ",time_ss[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("\n",append=TRUE,file=filename_paste)

        
    }
    
    return(list(auc_mpl_bd=auc_mpl_bd, time_mpl_bd = time_mpl_bd,
                auc_bd_app=auc_bd_app, time_bd_app = time_bd_app,
                #auc_bd_ex=auc_bd_ex, time_bd_ex = time_bd_ex,
                auc_rj_app=auc_rj_app, time_rj_app = time_rj_app,
                #auc_rj_ex=auc_rj_ex, time_rj_ex = time_rj_ex,
                auc_ss=auc_cc, time_ss=time_ss
    ))
    

}