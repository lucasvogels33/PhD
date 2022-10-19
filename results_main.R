## this code will activate results_sub, this will load the code and print it in the output file ##

require ( BDgraph )
require (pROC)

source("results_sub.R")

graph_list = c("random","cluster","scale-free")
n_list = c(50,100)
p_list = c(10)
round_nr = 2
iter_rep = 2

filename_auc = "latex_auc"
filename_time = "latex_time"

cat("mpl (jump=1) & mpl (jump = 10) & bd (jump=1) & bd (jump=10) & rj (jump=1) & rj (jump=10) & ss  \\ ",file = filename_auc,"\n", append = TRUE )
cat("\\hline \\","\\",sep="",file = filename_auc,"\n", append = TRUE )

cat("mpl (jump=1) & mpl (jump = 10) & bd (jump=1) & bd (jump=10) & rj (jump=1) & rj (jump=10) & ss  \\ ",file = filename_time,"\n", append = TRUE )
cat("\\hline \\","\\",sep="",file = filename_time,"\n", append = TRUE )

for (p in p_list)
{
    for( n in n_list )
    {
        cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = filename_auc,"\n", append = TRUE )
        cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = filename_time,"\n", append = TRUE )

        for( graph in graph_list )
	    {
        result = load_results(p=p,graph=graph,n=n,iter_rep=iter_rep)
		
        cat(graph," & ",
        round(mean(result $ auc_mpl_bd1),round_nr),"(",round(sd(result $ auc_mpl_bd1),round_nr),") & ",
        round(mean(result $ auc_mpl_bd2),round_nr),"(",round(sd(result $ auc_mpl_bd2),round_nr),") & ",
        round(mean(result $ auc_bd_app1),round_nr),"(",round(sd(result $ auc_bd_app1),round_nr),") & ",
        round(mean(result $ auc_bd_app2),round_nr),"(",round(sd(result $ auc_bd_app2),round_nr),") & ",
        round(mean(result $ auc_rj_app1),round_nr),"(",round(sd(result $ auc_rj_app1),round_nr),") & ",
        round(mean(result $ auc_rj_app2),round_nr),"(",round(sd(result $ auc_rj_app2),round_nr),") & ",
        round(mean(result $ auc_ss),round_nr),"(",round(sd(result $ auc_ss),round_nr),")",
        file = filename_auc,"\n", append = TRUE )
	    
        cat(graph," & ",
        round(mean(result $ time_mpl_bd1),round_nr),"(",round(sd(result $ time_mpl_bd1),round_nr),") & ",
        round(mean(result $ time_mpl_bd2),round_nr),"(",round(sd(result $ time_mpl_bd2),round_nr),") & ",
        round(mean(result $ time_bd_app1),round_nr),"(",round(sd(result $ time_bd_app1),round_nr),") & ",
        round(mean(result $ time_bd_app2),round_nr),"(",round(sd(result $ time_bd_app2),round_nr),") & ",
        round(mean(result $ time_rj_app1),round_nr),"(",round(sd(result $ time_rj_app1),round_nr),") & ",
        round(mean(result $ time_rj_app2),round_nr),"(",round(sd(result $ time_rj_app2),round_nr),") & ",
        round(mean(result $ time_ss),round_nr),"(",round(sd(result $ auc_ss),round_nr),")",
        file = filename_time,"\n", append = TRUE )   
                
        }
    }    
}    

