## this file contains the function load_results. This function gets the data from the Rdata files and prints it ##



load_results = function(p=5,graph="random",n=10,round=2,iter_rep=10)
{
    auc_mpl_bd1 = c(rep(0,iter_rep))
    time_mpl_bd1 = c(rep(0,iter_rep))
    auc_mpl_bd2 = c(rep(0,iter_rep))
    time_mpl_bd2 = c(rep(0,iter_rep))
    auc_bd_app1= c(rep(0,iter_rep))
    time_bd_app1 = c(rep(0,iter_rep))
    auc_bd_app2= c(rep(0,iter_rep))
    time_bd_app2 = c(rep(0,iter_rep))
    auc_rj_app1 = c(rep(0,iter_rep))
    time_rj_app1 = c(rep(0,iter_rep))
    auc_rj_app2 = c(rep(0,iter_rep))
    time_rj_app2 = c(rep(0,iter_rep))
    auc_ss = c(rep(0,iter_rep))
    time_ss = c(rep(0,iter_rep))
    

    filename_paste = paste0(graph,"_n=",n,"_output_all")
    
    for (i in 1:iter_rep){
        filename_copy = paste0("result_p",p,"_n",n,"_",graph,"_rep",i,".Rdata")
        load(file = filename_copy)

        auc_mpl_bd1[i] = result $ auc_mpl_bd1
        time_mpl_bd1[i] = result $ time_mpl_bd1
        auc_mpl_bd2[i] = result $ auc_mpl_bd2
        time_mpl_bd2[i] = result $ time_mpl_bd2
        auc_bd_app1[i]= result $ auc_bd_app1
        time_bd_app1[i] = result $ time_bd_app1
        auc_bd_app2[i]= result $ auc_bd_app2
        time_bd_app2[i] = result $ time_bd_app2
        #auc_bd_ex[i] = result $ auc_bd_ex
        #time_bd_ex[i] = result $ time_bd_ex
        auc_rj_app1[i] = result $ auc_rj_app1
        time_rj_app1[i] = result $ time_rj_app1
        auc_rj_app2[i] = result $ auc_rj_app2
        time_rj_app2[i] = result $ time_rj_app2
        #auc_rj_ex[i] = result $ auc_rj_ex
        #time_rj_ex[i] = result $ time_rj_ex
        auc_ss[i] = result $ auc_ss
        time_ss[i] = result $ time_ss

        cat("auc of rep ",i," of the mpl_bd1 method is ",auc_mpl_bd1[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the mpl_bd1 method is ",time_mpl_bd1[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the mpl_bd2 method is ",auc_mpl_bd2[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the mpl_bd2 method is ",time_mpl_bd2[i]," ", file = filename_paste,"\n", append = TRUE)
       
        cat("auc of rep ",i," of the bd_app1 method is ",auc_bd_app1[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the bd_app1 method is ",time_bd_app1[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the bd_app2 method is ",auc_bd_app2[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the bd_app2 method is ",time_bd_app2[i]," ", file = filename_paste,"\n", append = TRUE)

        #cat("auc of rep ",i," of the bd_ex method is ",auc_bd_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        #cat("time of rep ",i," of the bd_ex method is ",time_bd_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the rj_app1 method is ",auc_rj_app1[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the rj_app1 method is ",time_rj_app1[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the rj_app2 method is ",auc_rj_app2[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the rj_app2 method is ",time_rj_app2[i]," ", file = filename_paste,"\n", append = TRUE)

        #cat("auc of rep ",i," of the rj_ex method is ",auc_rj_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        #cat("time of rep ",i," of the rj_ex method is ",time_rj_ex[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("auc of rep ",i," of the ss method is ",auc_ss[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("time of rep ",i," of the ss method is ",time_ss[i]," ", file = filename_paste,"\n", append = TRUE)
        cat("\n",append=TRUE,file=filename_paste)

        
    }
    
    return(list(auc_mpl_bd1=auc_mpl_bd1, time_mpl_bd1 = time_mpl_bd1,
                auc_mpl_bd2=auc_mpl_bd2, time_mpl_bd2 = time_mpl_bd2,
                auc_bd_app1=auc_bd_app1, time_bd_app1 = time_bd_app1,
                auc_bd_app2=auc_bd_app2, time_bd_app2 = time_bd_app2,
                #auc_bd_ex=auc_bd_ex, time_bd_ex = time_bd_ex,
                auc_rj_app1=auc_rj_app1, time_rj_app1 = time_rj_app1,
                auc_rj_app2=auc_rj_app2, time_rj_app2 = time_rj_app2,
                #,auc_rj_ex=auc_rj_ex, time_rj_ex = time_rj_ex,
                auc_ss=auc_cc, time_ss=time_ss
    ))
    
    
}