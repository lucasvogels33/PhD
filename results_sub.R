## this file contains the function print_results. This function gets the data from the Rdata files and prints it ##

load_results = function(p=5,graph="random",n=10,report="auc",round=2,iter_rep=10)
{
    auc_mpl_bd = c(rep(0,iter_rep))
    time_mpl_bd = c(rep(0,iter_rep))

    for (i in 1:iter_rep){
        filename = paste0("result_p",p,"_n",n,"_",graph,"_rep",i,".Rdata")
        load(file = filename)

        auc_mpl_bd[i] = result $ auc_mpl_bd
        time_mpl_bd[i] = result $ time_mpl_bd
        cat("auc of rep ",i," of the mpl_bd method is ",auc_mpl_bd[i]," ", file = "output_all", append = TRUE)
        cat("time of rep ",i," of the mpl_bd method is ",time_mpl_bd[i]," ", file = "output_all", append = TRUE)

        
    }
    
    return(list(auc_mpl_bd=auc_mpl_bd, time_mpl_bd = time_mpl_bd))
    

}