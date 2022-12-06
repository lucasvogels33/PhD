#--download the necessary libraries and load the read_data_functions file --
library(pROC)
source("read_data_functions.R")

#--set parameters
graph_list = c("random","cluster","scale-free") 
n_list = c(10,20,100)
p_list = c(10)
rep_list = c(1:50)
thin_CE = 100
thin_conv = 1000
epsilon = 0.005
cut = 200

##---------------------------------------------------------------------------|

for (graph in graph_list){
    for (n in n_list){
        for (p in p_list){
            
            #average over all replications and save    
            avg_obj = avg_metrics(n_vec=c(n),p_vec=c(p),graph_vec=c(graph),rep_vec=rep_list,thin_CE=thin_CE,
            thin_conv=thin_conv,epsilon=epsilon,plot=FALSE,cut=cut)
            filename = paste0("AVERAGE_p",p,"_n",n,"_",graph,"v3.Rdata")
            save(avg_obj, file = filename )


        }
    }
}


  
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
