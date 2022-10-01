## this file contains the function print_results. This function gets the data from the Rdata files and prints it ##

load_results = function(p=5,graph="random",n=10,report="auc",round=2,iter_rep=10)
{
    auc_mpl_bd = c(rep(0,iter_rep))

    for (i in 1:iter_rep){
        filename = paste0("result_p",p,"_n",n,"_",graph,"_rep",i,".Rdata")
        load(file = filename)

        if ( report == "auc")
        {
            auc_mpl_bd[i] = result $ auc_mpl_bd
        }

        result = list(auc_mpl_bd)
    }
        


}