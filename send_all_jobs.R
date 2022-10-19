

create_script = function( n = 50, graph = "random", iter = 1, p = 10, constraint ="gold_6130",
                               dir = "$HOME/temp", walltime = "00-23:59:00", partition = "nodes", nodes = 1, ntasks = 1 )
{
	script_name = paste(graph,"_p",p,"n_",n,sep = "" )
	
    cat( "#!/bin/bash", "\n", file = script_name)
    cat( "#SBATCH --job-name=", graph,"_p",p,"n_",n, "\n", file = script_name, append = TRUE, sep = "" )
    cat( "#SBATCH --output=",   graph,"_p",p,"n_",n,"_output","\n", file = script_name, append = TRUE, sep = "" )
    cat( "#SBATCH --nodes=",     nodes,     "\n", file = script_name, append = TRUE, sep = "" )
	cat( "#SBATCH --ntasks=",    ntasks,    "\n", file = script_name, append = TRUE, sep = "" )
	cat( "#SBATCH --time=",      walltime,  "\n", file = script_name, append = TRUE, sep = "" )
	cat( "#SBATCH --partition=", partition, "\n", file = script_name, append = TRUE, sep = "" )
    cat("#SBATCH --constraint=",constraint, "\n", file = script_name, append = TRUE, sep = "" )
	cat( "cd ", dir,"\n", file = script_name, append = TRUE, sep = "" )
	
    cat("module load 2021","\n",file = script_name, append = TRUE, sep = "" )
    cat("module load R/4.1.0-foss-2021a","\n",file = script_name, append = TRUE, sep = "" )

	cat( "Rscript --no-save --slave run_and_save.R ",p," ",n," ",graph," ",iter,"\n",file=script_name,append = TRUE, sep = "" )
       
	return( script_name )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
graph_list = c("random","cluster","scale-free")
n_list = c(50,100,500)
p_list = c(50)
iter = 10
walltime = "00-23:59:00"
partition = "normal"
nodes = 1
ntasks = 1
constraint = "gold_6130"
dir = "$HOME/temp3"
##---------------------------------------------------------------------------|


for (p in p_list)
{
    for( n in n_list )
    {
	    for( graph in graph_list )
	    {
			system( paste( "sbatch ", create_script( n = n, graph = graph, iter = iter,p=p,dir=dir,walltime=walltime,partition=partition,nodes=nodes,ntasks=ntasks,constraint=constraint), sep = "" ) )   
	    }
    }    
}     
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
