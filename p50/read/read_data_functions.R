## this R document contains the function CE_area, CE_plot_cut, area_ROC_CE_per_iter and ROC_plot_cut
##CE_area takes as input an actual matrix and a predictor matrix. It calculates the area under the CE curve. This area is equal to the calibration error
##CE_lot_cut takes as input an actual matrix and a predictor matrix. It outputs a vector of alpha values with corresponding CE(alpha) values. Optionally it plots these vectors and calculates the area under thsi curve
##area_ROC_CE_per_iter takes an bdgraph.obj and an actual matrix as input. It calculates the area under the ROC curve and the area under the CE curve at every x iterations, where x = thin. 

avg_metrics = function(n_vec=c(100),p_vec=c(100),graph_vec=c("random"),rep_vec=c(1:50),thin_CE=100,thin_conv=1000,
plot=FALSE,cut=200,epsilon=0.005){
  #determine number of objects to average over
  length = length(n_vec)*length(p_vec)*length(graph_vec)*length(rep_vec)
 
  #determine number of MCMC iterations
  p =p_vec[1]
  n= n_vec[1]
  graph = graph_vec[1]
  rep = rep_vec[1]
  filename = paste0("result_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
  load(file = filename)
  iter = length(result$all_graphs_mpl_bd)
  
  #set thinned vector of iterations (x-axis for iteration graphs)
  iter_vec_thin = c(thin_CE * (1:floor(iter/thin_CE)))
  iter_vec_thin_conv = c(thin_conv * (1:floor(iter/thin_conv)))[-1]
  iter_length = length(iter_vec_thin)
  iter_conv_length = length(iter_vec_thin_conv)
  
  #set cut points for the average CE plots and the average ROC curves
  cut_points = (1:(cut-1))/cut
  cut_points = c(0,cut_points,1)
  cut_length = length(cut_points)

  #initiate all output vectors mpl bd method
  AUC_mpl_bd = c(rep(0,length))
  CE_mpl_bd = c(rep(0,length))
  SVAUC_mpl_bd = c(rep(0,length))
  time_mpl_bd = c(rep(0,length))
  AUC_iter_mpl_bd = c(rep(0,iter_length))
  CE_iter_mpl_bd = c(rep(0,iter_length))
  SVAUC_iter_mpl_bd = c(rep(0,iter_length))
  tp_cut_mpl_bd = c(rep(0,cut_length))
  fp_cut_mpl_bd = c(rep(0,cut_length))
  CE_cut_mpl_bd = c(rep(0,cut_length))
  all_graphs_max_mpl_bd = c(rep(0,length))
  iteration_conv_mpl_bd = c(rep(0,length))
  time_conv_mpl_bd = c(rep(0,length))
  max_diff_mpl_bd = c(rep(0,iter_conv_length))
  
  #initiate all output vectors mpl rj method
  AUC_mpl_rj = c(rep(0,length))
  CE_mpl_rj = c(rep(0,length))
  SVAUC_mpl_rj = c(rep(0,length))
  time_mpl_rj = c(rep(0,length))
  AUC_iter_mpl_rj = c(rep(0,iter_length))
  CE_iter_mpl_rj = c(rep(0,iter_length))
  SVAUC_iter_mpl_rj = c(rep(0,iter_length))
  tp_cut_mpl_rj = c(rep(0,cut_length))
  fp_cut_mpl_rj = c(rep(0,cut_length))
  CE_cut_mpl_rj = c(rep(0,cut_length))
  all_graphs_max_mpl_rj = c(rep(0,length))
  iteration_conv_mpl_rj = c(rep(0,length))
  time_conv_mpl_rj = c(rep(0,length))
  max_diff_mpl_rj = c(rep(0,iter_conv_length))

  #initiate all output vectors bd method
  AUC_bd = c(rep(0,length))
  CE_bd = c(rep(0,length))
  SVAUC_bd = c(rep(0,length))
  time_bd = c(rep(0,length))
  AUC_iter_bd = c(rep(0,iter_length))
  CE_iter_bd = c(rep(0,iter_length))
  SVAUC_iter_bd = c(rep(0,iter_length))
  tp_cut_bd = c(rep(0,cut_length))
  fp_cut_bd = c(rep(0,cut_length))
  CE_cut_bd = c(rep(0,cut_length))
  all_graphs_max_bd = c(rep(0,length))
  iteration_conv_bd = c(rep(0,length))
  time_conv_bd = c(rep(0,length))
  max_diff_bd = c(rep(0,iter_conv_length))

  #initiate all output vectors rj method
  AUC_rj = c(rep(0,length))
  CE_rj = c(rep(0,length))
  SVAUC_rj = c(rep(0,length))
  time_rj = c(rep(0,length))
  AUC_iter_rj = c(rep(0,iter_length))
  CE_iter_rj = c(rep(0,iter_length))
  SVAUC_iter_rj = c(rep(0,iter_length))
  tp_cut_rj = c(rep(0,cut_length))
  fp_cut_rj = c(rep(0,cut_length))
  CE_cut_rj = c(rep(0,cut_length))
  all_graphs_rj = c(rep(0,length))
  iteration_conv_rj = c(rep(0,length))
  time_conv_rj = c(rep(0,length))
  max_diff_rj = c(rep(0,iter_conv_length))

  #initiate all output vectors ss method
  AUC_ss = c(rep(0,length))
  CE_ss = c(rep(0,length))
  SVAUC_ss = c(rep(0,length))
  time_ss = c(rep(0,length))
  AUC_iter_ss = c(rep(0,iter_length))
  CE_iter_ss = c(rep(0,iter_length))
  SVAUC_iter_ss = c(rep(0,iter_length))
  tp_cut_ss = c(rep(0,cut_length))
  fp_cut_ss = c(rep(0,cut_length))
  CE_cut_ss = c(rep(0,cut_length))
  all_graphs_ss = c(rep(0,length))
  iteration_conv_ss = c(rep(0,length))
  time_conv_ss = c(rep(0,length))
  max_diff_mpl_ss = c(rep(0,iter_conv_length))

  i = 0

  for (p in p_vec){  
    for (n in n_vec){
      for (graph in graph_vec){
        for (rep in rep_vec){
            i = i+1
            
            filename = paste0("result_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
            load(file = filename)
            actual = result$true_g
            response = actual[upper.tri(actual)]
            
            ### ---mpl bd method---###
            #--collect data and calculate AUC,CE,SVAUC and time
            all_graphs_mpl_bd = result$all_graphs_mpl_bd
            sample_graphs_mpl_bd = result$sample_graphs_mpl_bd
            all_weights_mpl_bd = result$all_weights_mpl_bd
            time = result$time_mpl_bd

            obj_mpl_bd = area_ROC_CE_per_iter(all_graphs=all_graphs_mpl_bd,sample_graphs=sample_graphs_mpl_bd,all_weights=all_weights_mpl_bd, actual=actual, thin_CE = thin_CE,thin_conv=thin_conv, verbose = TRUE) 
            
            #--final AUC, CE, SVAUC and time values
            AUC_mpl_bd[i] = obj_mpl_bd$AUC
            CE_mpl_bd[i] = obj_mpl_bd$CE
            SVAUC_mpl_bd[i] = obj_mpl_bd$SVAUC
            time_mpl_bd[i] = time            
            all_graphs_max_mpl_bd[i] = max(all_graphs_mpl_bd)
            
            #find the iteration and time of convergence 
            iteration_conv_mpl_bd[i] = iter_vec_thin_conv[match(TRUE,(obj_mpl_bd$max_diff<epsilon))]
            time_per_iter_mpl_bd = time/iter
            time_conv_mpl_bd[i] = iteration_conv_mpl_bd[i]*time_per_iter_mpl_bd

            #--obtain AUC,CE,SVAUC,diff per iter vectors
            AUC_iter_mpl_bd = AUC_iter_mpl_bd + obj_mpl_bd$AUC_vec
            CE_iter_mpl_bd = CE_iter_mpl_bd + obj_mpl_bd$CE_vec
            SVAUC_iter_mpl_bd = SVAUC_iter_mpl_bd + obj_mpl_bd$SVAUC_vec
            max_diff_mpl_bd = max_diff_mpl_bd + obj_mpl_bd$max_diff

            #--obtain fp,tp points and CE points for thresholds
            plinks = obj_mpl_bd$plinks
            CE_ROC_obj_mpl_bd = CE_ROC_plot_cut(response=response,predictor=plinks,cut=cut)
            tp_cut_mpl_bd = tp_cut_mpl_bd + CE_ROC_obj_mpl_bd$tp
            fp_cut_mpl_bd = fp_cut_mpl_bd + CE_ROC_obj_mpl_bd$fp
            CE_cut_mpl_bd = CE_cut_mpl_bd + CE_ROC_obj_mpl_bd$CE_vec        
        
            ### ---mpl rj method--- ###
            #- collect data and calculate AUC,CE,SVAUC and time
            all_graphs_mpl_rj = result$all_graphs_mpl_rj
            sample_graphs_mpl_rj = result$sample_graphs_mpl_rj
            all_weights_mpl_rj = result$all_weights_mpl_rj
            time = result$time_mpl_rj

            obj_mpl_rj = area_ROC_CE_per_iter(all_graphs=all_graphs_mpl_rj,sample_graphs=sample_graphs_mpl_rj,all_weights=all_weights_mpl_rj, actual=actual, thin_CE = thin_CE,thin_conv=thin_conv, verbose = TRUE) 

            #--obtain AUC,CE,SVAUC per iter vectors
            AUC_mpl_rj[i] = obj_mpl_rj$AUC
            CE_mpl_rj[i] = obj_mpl_rj$CE
            SVAUC_mpl_rj[i] = obj_mpl_rj$SVAUC
            time_mpl_rj[i] = time
            all_graphs_max_mpl_rj[i] = max(all_graphs_mpl_rj)

            #find the iteration and time of convergence 
            iteration_conv_mpl_rj[i] = iter_vec_thin_conv[match(TRUE,(obj_mpl_rj$max_diff<epsilon))]
            time_per_iter_mpl_rj = time/iter
            time_conv_mpl_rj[i] = iteration_conv_mpl_rj[i]*time_per_iter_mpl_rj

            #--obtain AUC,CE,SVAUC,diff per iter vectors
            AUC_iter_mpl_rj = AUC_iter_mpl_rj + obj_mpl_rj$AUC_vec
            CE_iter_mpl_rj = CE_iter_mpl_rj + obj_mpl_rj$CE_vec
            SVAUC_iter_mpl_rj = SVAUC_iter_mpl_rj + obj_mpl_rj$SVAUC_vec
            max_diff_mpl_rj = max_diff_mpl_rj + obj_mpl_rj$max_diff

            #--obtain fp,tp points and CE points for thresholds
            plinks = obj_mpl_rj$plinks
            CE_ROC_obj_mpl_rj = CE_ROC_plot_cut(response=response,predictor=plinks,cut=cut)
            tp_cut_mpl_rj = tp_cut_mpl_rj + CE_ROC_obj_mpl_rj$tp
            fp_cut_mpl_rj = fp_cut_mpl_rj + CE_ROC_obj_mpl_rj$fp
            CE_cut_mpl_rj = CE_cut_mpl_rj + CE_ROC_obj_mpl_rj$CE_vec  

            ##--bd method --##
            #-collect data and calculate AUC,CE,SVAUC and time
            all_graphs_bd = result$all_graphs_bd
            sample_graphs_bd = result$sample_graphs_bd
            all_weights_bd = result$all_weights_bd
            time = result$time_bd

            obj_bd = area_ROC_CE_per_iter(all_graphs=all_graphs_bd,sample_graphs=sample_graphs_bd,all_weights=all_weights_bd, actual=actual, thin_CE = thin_CE,thin_conv=thin_conv, verbose = TRUE) 
            AUC_bd[i] = obj_bd$AUC
            CE_bd[i] = obj_bd$CE
            SVAUC_bd[i] = obj_bd$SVAUC
            time_bd[i] = time
            all_graphs_max_bd[i] = max(all_graphs_bd)

            #find the iteration and time of convergence 
            iteration_conv_bd[i] = iter_vec_thin_conv[match(TRUE,(obj_bd$max_diff<epsilon))]
            time_per_iter_bd = time/iter
            time_conv_bd[i] = iteration_conv_bd[i]*time_per_iter_bd

            #--obtain AUC,CE,SVAUC,diff per iter vectors
            AUC_iter_bd = AUC_iter_bd + obj_bd$AUC_vec
            CE_iter_bd = CE_iter_bd + obj_bd$CE_vec
            SVAUC_iter_bd = SVAUC_iter_bd + obj_bd$SVAUC_vec
            max_diff_bd = max_diff_bd + obj_bd$max_diff
            
            #--obtain fp,tp points and CE points for thresholds
            plinks = obj_bd$plinks
            CE_ROC_obj_bd = CE_ROC_plot_cut(response=response,predictor=plinks,cut=cut)
            tp_cut_bd = tp_cut_bd + CE_ROC_obj_bd$tp
            fp_cut_bd = fp_cut_bd + CE_ROC_obj_bd$fp
            CE_cut_bd = CE_cut_bd + CE_ROC_obj_bd$CE_vec  

            ##--rj method ##
            #- collect data and calculate AUC,CE,SVAUC and time
            all_graphs_rj = result$all_graphs_rj
            sample_graphs_rj = result$sample_graphs_rj
            all_weights_rj = result$all_weights_rj
            time = result$time_rj
            
            obj_rj = area_ROC_CE_per_iter(all_graphs=all_graphs_rj,sample_graphs=sample_graphs_rj,all_weights=all_weights_rj, actual=actual, thin_CE = thin_CE,thin_conv=thin_conv, verbose = TRUE) 
            
            #--obtain AUC,CE,SVAUC per iter vectors
            AUC_rj[i] = obj_rj$AUC
            CE_rj[i] = obj_rj$CE
            SVAUC_rj[i] = obj_rj$SVAUC
            time_rj[i] = time
            all_graphs_max_rj[i] = max(all_graphs_rj)

            #find the iteration and time of convergence 
            iteration_conv_rj[i] = iter_vec_thin_conv[match(TRUE,(obj_rj$max_diff<epsilon))]
            time_per_iter_rj = time/iter
            time_conv_rj[i] = iteration_conv_rj[i]*time_per_iter_rj

            #--obtain AUC,CE,SVAUC,diff per iter vectors
            AUC_iter_rj = AUC_iter_rj + obj_rj$AUC_vec
            CE_iter_rj = CE_iter_rj + obj_rj$CE_vec
            SVAUC_iter_rj = SVAUC_iter_rj + obj_rj$SVAUC_vec
            max_diff_rj = max_diff_rj + obj_rj$max_diff

            #--obtain fp,tp points and CE points for thresholds
            plinks = obj_rj$plinks
            CE_ROC_obj_rj = CE_ROC_plot_cut(response=response,predictor=plinks,cut=cut)
            tp_cut_rj = tp_cut_rj + CE_ROC_obj_rj$tp
            fp_cut_rj = fp_cut_rj + CE_ROC_obj_rj$fp
            CE_cut_rj = CE_cut_rj + CE_ROC_obj_rj$CE_vec  

            ### ---ss method -##
            # collect data and calculate AUC,CE,SVAUC and time
            all_graphs_ss = result$all_graphs_ss
            sample_graphs_ss = result$sample_graphs_ss
            all_weights_ss = result$all_weights_ss
            time = result$time_ss

            obj_ss = area_ROC_CE_per_iter(all_graphs=all_graphs_ss,sample_graphs=sample_graphs_ss,all_weights=all_weights_ss, actual=actual, thin_CE = thin_CE,thin_conv=thin_conv, verbose = TRUE) 

            #--obtain AUC,CE,SVAUC per iter vectors
            AUC_ss[i] = obj_ss$AUC
            CE_ss[i] = obj_ss$CE
            SVAUC_ss[i] = obj_ss$SVAUC
            time_ss[i] = time
            all_graphs_max_ss[i] = max(all_graphs_ss)

            #find the iteration and time of convergence 
            iteration_conv_ss[i] = iter_vec_thin_conv[match(TRUE,(obj_ss$max_diff<epsilon))]
            time_per_iter_ss = time/iter
            time_conv_ss[i] = iteration_conv_ss[i]*time_per_iter_ss

            #--obtain AUC,CE,SVAUC,diff per iter vectors
            AUC_iter_ss = AUC_iter_ss + obj_ss$AUC_vec
            CE_iter_ss = CE_iter_ss + obj_ss$CE_vec
            SVAUC_iter_ss = SVAUC_iter_ss + obj_ss$SVAUC_vec
            max_diff_ss = max_diff_ss + obj_ss$max_diff
            
            #--obtain fp,tp points and CE points for thresholds
            plinks = obj_ss$plinks
            CE_ROC_obj_ss = CE_ROC_plot_cut(response=response,predictor=plinks,cut=cut)
            tp_cut_ss = tp_cut_ss + CE_ROC_obj_ss$tp
            fp_cut_ss = fp_cut_ss + CE_ROC_obj_ss$fp
            CE_cut_ss = CE_cut_ss + CE_ROC_obj_ss$CE_vec  
            
        }
      }
    }
  }

  AUC_iter_mpl_bd = AUC_iter_mpl_bd/length
  CE_iter_mpl_bd = CE_iter_mpl_bd/length
  SVAUC_iter_mpl_bd = SVAUC_iter_mpl_bd/length
  tp_cut_mpl_bd = tp_cut_mpl_bd/length
  fp_cut_mpl_bd = fp_cut_mpl_bd/length
  CE_cut_mpl_bd = CE_cut_mpl_bd/length
  max_diff_mpl_bd = max_diff_mpl_bd/length

  AUC_iter_mpl_rj = AUC_iter_mpl_rj/length
  CE_iter_mpl_rj = CE_iter_mpl_rj/length
  SVAUC_iter_mpl_rj = SVAUC_iter_mpl_rj/length
  tp_cut_mpl_rj = tp_cut_mpl_rj/length
  fp_cut_mpl_rj = fp_cut_mpl_rj/length
  CE_cut_mpl_rj = CE_cut_mpl_rj/length
  max_diff_mpl_rj = max_diff_mpl_rj/length

  AUC_iter_bd = AUC_iter_bd/length
  CE_iter_bd = CE_iter_bd/length
  SVAUC_iter_bd = SVAUC_iter_bd/length
  tp_cut_bd = tp_cut_bd/length
  fp_cut_bd = fp_cut_bd/length
  CE_cut_bd = CE_cut_bd/length
  max_diff_bd = max_diff_bd/length

  AUC_iter_rj = AUC_iter_rj/length
  CE_iter_rj = CE_iter_rj/length
  SVAUC_iter_rj = SVAUC_iter_rj/length
  tp_cut_rj = tp_cut_rj/length
  fp_cut_rj = fp_cut_rj/length
  CE_cut_rj = CE_cut_rj/length
  max_diff_rj = max_diff_rj/length

  AUC_iter_ss = AUC_iter_ss/length
  CE_iter_ss = CE_iter_ss/length
  SVAUC_iter_ss = SVAUC_iter_ss/length
  tp_cut_ss = tp_cut_ss/length
  fp_cut_ss = fp_cut_ss/length
  CE_cut_ss = CE_cut_ss/length
  max_diff_ss = max_diff_ss/length

  return(list(AUC_mpl_bd = AUC_mpl_bd, CE_mpl_bd = CE_mpl_bd, SVAUC_mpl_bd = SVAUC_mpl_bd, time_mpl_bd = time_mpl_bd,
            AUC_iter_mpl_bd = AUC_iter_mpl_bd, CE_iter_mpl_bd = CE_iter_mpl_bd,
            SVAUC_iter_mpl_bd = SVAUC_iter_mpl_bd,
            tp_cut_mpl_bd = tp_cut_mpl_bd, fp_cut_mpl_bd = fp_cut_mpl_bd, CE_cut_mpl_bd = CE_cut_mpl_bd, 
            all_graphs_max_mpl_bd = all_graphs_max_mpl_bd, iteration_conv_mpl_bd = iteration_conv_mpl_bd,
            time_conv_mpl_bd = time_conv_mpl_bd, max_diff_mpl_bd = max_diff_mpl_bd, 

            AUC_mpl_rj = AUC_mpl_rj, CE_mpl_rj = CE_mpl_rj, SVAUC_mpl_rj = SVAUC_mpl_rj, time_mpl_rj = time_mpl_rj,
            AUC_iter_mpl_rj = AUC_iter_mpl_rj, CE_iter_mpl_rj = CE_iter_mpl_rj,
            SVAUC_iter_mpl_rj = SVAUC_iter_mpl_rj,
            tp_cut_mpl_rj = tp_cut_mpl_rj, fp_cut_mpl_rj = fp_cut_mpl_rj, CE_cut_mpl_rj = CE_cut_mpl_rj,
            all_graphs_max_mpl_rj = all_graphs_max_mpl_rj, iteration_conv_mpl_rj = iteration_conv_mpl_rj,
            time_conv_mpl_rj = time_conv_mpl_rj, max_diff_mpl_rj = max_diff_mpl_rj, 

            AUC_bd = AUC_bd, CE_bd = CE_bd, SVAUC_bd = SVAUC_bd, time_bd = time_bd,
            AUC_iter_bd = AUC_iter_bd, CE_iter_bd = CE_iter_bd,
            SVAUC_iter_bd = SVAUC_iter_bd,
            tp_cut_bd = tp_cut_bd, fp_cut_bd = fp_cut_bd, CE_cut_bd = CE_cut_bd,
            all_graphs_max_bd = all_graphs_max_bd, iteration_conv_bd = iteration_conv_bd,
            time_conv_bd = time_conv_bd, max_diff_bd = max_diff_bd, 

            AUC_rj = AUC_rj, CE_rj = CE_rj, SVAUC_rj = SVAUC_rj, time_rj = time_rj,
            AUC_iter_rj = AUC_iter_rj, CE_iter_rj = CE_iter_rj,
            SVAUC_iter_rj = SVAUC_iter_rj,
            tp_cut_rj = tp_cut_rj, fp_cut_rj = fp_cut_rj, CE_cut_rj = CE_cut_rj,
            all_graphs_max_rj = all_graphs_max_rj, iteration_conv_rj = iteration_conv_rj,
            time_conv_rj = time_conv_rj, max_diff_rj = max_diff_rj, 

            AUC_ss = AUC_ss, CE_ss = CE_ss, SVAUC_ss = SVAUC_ss, time_ss = time_ss,
            AUC_iter_ss = AUC_iter_ss, CE_iter_ss = CE_iter_ss,
            SVAUC_iter_ss = SVAUC_iter_ss,
            tp_cut_ss = tp_cut_ss, fp_cut_ss = fp_cut_ss, CE_cut_ss = CE_cut_ss,
            all_graphs_max_ss = all_graphs_max_ss, iteration_conv_ss = iteration_conv_ss,
            time_conv_ss = time_conv_ss, max_diff_ss = max_diff_ss, 

            iter_vec_thin= iter_vec_thin,
            cut_points = cut_points))

}

calc_SVAUC = function(response=response,predictor=predictor){
  
  #make sure predictor and response have the same length
  if (length(response) != length(predictor)) {
    stop("response and predictor vector must be of same length")
  }

  predictor.order = order(predictor,decreasing=TRUE)
  predictor.sorted = predictor[predictor.order]
  response.sorted = response[predictor.order]
  ones = sum(response)
  zeroes = length(response)-ones
  
  #obtain the true positive vector
  tp = cumsum(response.sorted==1)
  tp = c(0,tp)
  tpr = tp/ones
  #obtain the false postive vector
  fp = cumsum(response.sorted==0)
  fp = c(0,fp)
  fpr = fp/zeroes

  #calculate threshold ranges
  top = c(1,predictor.sorted)
  bottom = c(predictor.sorted,0)
  ranges = top-bottom
  
  SVAUC = sum(ranges*(1-fpr)*tpr)
  return(SVAUC)
}

calc_AUC = function(predictor,response){

  if (length(response) != length(predictor)) {
    stop("response and predictor vector must be of same length")
  }

  #order the vectors so that the predictor is increasing
  predictor.order = order(predictor,decreasing=FALSE)
  predictor.sorted = predictor[predictor.order]
  response.sorted = response[predictor.order]

  #determine amount of zeroes and ones
  ones = sum(response)
  zeroes = length(response)-ones

  #if there are duplicates
  if (sum(duplicated(predictor.sorted))>0){
    #create a vector with one index for every group of duplicates
    dup_index = cumsum(duplicated(predictor.sorted)==0)

    #create a vector sum_vec that sums the true positives in each group of duplicates   
    df <- data.frame(duplicates=dup_index,response.sorted=response.sorted)
    sum_vec = aggregate(response.sorted ~ duplicates, data=df, sum)[,2]

    #create a vector that averages the maximum amount of false positives of the current group with the previous group
    fp = cumsum(response.sorted==0)
    df <- data.frame(duplicates=cumsum(dup==0),fp=fp)
    max_vec = aggregate(fp ~ duplicates, data=df, max)[,2]
    top = c(0,max_vec)
    bottom = c(max_vec,0)
    average_vec = head((top+bottom)/2,-1)

    #AUC is the dot product of the two vectors divided by the normalizing constant
    AUC = (sum_vec%*%average_vec)/(ones*zeroes)

  }

  #if there are no duplicates
  if (sum(duplicated(predictor.sorted))==0){
    fp = cumsum(response.sorted==0)
    AUC = sum(fp * response.sorted)
    AUC = AUC/(zeroes*ones)  
    
  }

  return(AUC)

  
}

CE_ROC_plot_cut = function(response,predictor,plot=FALSE,area=FALSE,cut=200){
  if (length(response) != length(predictor)) {
    stop("response and predictor vector must be of same length")
  }
  
  cut_points = (1:(cut-1))/cut
  CE_vec = c(rep(0,length(cut_points)))
  tp = c(rep(0,length(cut_points)))
  fp = c(rep(0,length(cut_points)))
  ones = sum(response)
  zeroes = length(response)-ones
  j = 0
  
  for (i in cut_points){
    j = j + 1 #count 

    #calculate graph corresponding to alpha value
    predictor_cut = c(rep(0,length(predictor)))
    predictor_cut[ predictor > i ] = 1

    #calculate CE(alpha)
    CE_alpha = sum(predictor_cut-response)
    CE_vec[j] = CE_alpha

    #calculate ROC vectors
    tp [j] = sum((predictor_cut==1)*(response==1))
    fp [j] = sum((predictor_cut==1)*(response==0))
  }
  
  if (area) {
    #calculate area under CE curve
    top = c(zeroes,CE_vec)
    bottom = c(CE_vec,ones)
    CE_vec_diff = (top + bottom)/2
    area_under = sum(CE_vec_diff)/(length(response)*cut)
    area_above = 1 - area_under
  }
  else{
    area_under = NULL
    area_above= NULL
  }
  
  cut_points = c(0,cut_points,1)
  CE_vec = c(zeroes,CE_vec,ones)
  CE_vec = CE_vec/length(response) #normalize
  tp = c(ones,tp,0)
  fp = c(zeroes,fp,0)
  tp = tp/ones #normalize
  fp = fp/zeroes #normalize

  if (plot) {
    #plot CE curve
    plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="alpha",ylab="CE")
    points(x=cut_points,y=CE_vec,type="l")

    #plot ROC curve
    plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="FPR",ylab="TPR")
    points(x=fp,y=tp,type="l")

  }
  
  return(list(cut_points=cut_points,CE_vec=CE_vec,tp=tp,fp=fp,area_under=area_under,area_above=area_above,plot=plot))
  
}

area_ROC_CE_per_iter = function (all_graphs=NULL,sample_graphs=NULL,all_weights=NULL, actual, thin_CE = NULL,thin_conv = NULL, verbose = TRUE) 
{
  #check if actual matrix is in correct format
  if (is.matrix(actual)) 
    if ((sum(actual == 0) + sum(actual == 1)) != (nrow(actual)^2)) 
      stop("Elements of matrix 'actual' must be 0 or 1")
  if (inherits(actual, "sim")) 
    actual = actual$G
  
  if (is.null(all_graphs))
    stop("please include the argument all_graphs")
  if (is.null(sample_graphs))
      stop("please include the argument sample_graphs")
  if (is.null(all_weights))
      stop("please include the argument all_weights")
  
  #check if the thin value is in the right format
  if (is.null(thin_CE)) 
    stop("'thin' must be a number")
  if (!is.numeric(thin_CE)) 
    stop("'thin' must be a number")
  if (is.matrix(thin_CE)) 
    stop("'thin' must be a number")
  
  #check if the thin value is in the right format
  if (is.null(thin_conv)) 
    stop("'thin' must be a number")
  if (!is.numeric(thin_conv)) 
    stop("'thin' must be a number")
  if (is.matrix(thin_conv)) 
    stop("'thin' must be a number")

  #obtain input
  p = nrow(actual)
  qp = p * (p - 1)/2
  response = actual[upper.tri(actual)]
  
  #create output vectors
  iter = length(all_graphs)
  iter_vec= c(1:iter)
  result_plinks = matrix(0,qp,iter)
  totaltime_per_edge = c(rep(0,qp))
  
  #create thinned output vectors
  iter_vec_thin = c(thin_CE * (1:floor(iter/thin_CE)))
  iter_vec_thin_conv = c(thin_conv * (1:floor(iter/thin_conv)))[-1]
  
  AUC_vec = c(rep(0,length(iter_vec_thin)))
  CE_vec = c(rep(0,length(iter_vec_thin)))
  SVAUC_vec = c(rep(0,length(iter_vec_thin)))
  
  max_diff = c(rep(0,length(iter_vec_thin_conv)))

  #we compute the edge inclusion matrix at every single iteration
  for (g in 1:iter) {
    if (verbose == TRUE) {
      mes = paste(c("Calculating plinks ... in progress : ", floor(100 * 
                                                              g/iter), "%"), collapse = "")
      cat(mes, "\r")
      utils::flush.console()
    }
    which_edge = which(unlist(strsplit(as.character(sample_graphs[all_graphs[g]]), 
                                       "")) == 1)
    
    #compute and save posterior inclusion probability for every edge
    totaltime_per_edge[which_edge] = totaltime_per_edge[which_edge] + all_weights[g]
    result_plinks[,g] = totaltime_per_edge/sum(all_weights[c(1:g)])
    
  }
  
  #we calculate the AUC, CE and SVAUC values every x=thin iterations
  i = 0
  for (g in iter_vec_thin){
    
    if (verbose == TRUE) {
      mes = paste(c("Calculating AUC and CE values ... in progress : ", floor(100 * 
                                                                    g/iter), "%"), collapse = "")
      cat(mes, "\r")
      utils::flush.console()
    }
    
    i = i + 1
    #compute and save area under the curve
    predictor = result_plinks[,g]
    AUC_vec[i] = calc_AUC(predictor=predictor,response=response)   
    CE_vec[i] = sum(abs(predictor-response))/qp
    SVAUC_vec[i] = calc_SVAUC(predictor=predictor,response=response)
  }

  #we calculate the amount of iterations until convergence, 
  #Convergence is when max|P_e(t)-P_e(t+1000)|<epsilon, 
  i = 0
  for (g in iter_vec_thin_conv){   
    i = i + 1
    
    #compute and save the maxixmum difference
    diff = abs(result_plinks[,g]-result_plinks[,g-thin_conv])
    max_diff[i] = max(diff)

    
  }
  
  #calculate final values AUC, CE, SVAUC and p_links
  plinks = result_plinks[,iter] 
  AUC = calc_AUC(predictor=plinks,response=response)
  CE = sum(abs(plinks-response))/qp
  SVAUC = calc_SVAUC(predictor=plinks,response=response)

  if (verbose == TRUE) {
    mes = paste(c("Calculation ... done.                        "), 
                collapse = "")
    cat(mes, "\r")
    cat("\n")
    utils::flush.console()
  }
  

  return(list(plinks=plinks,AUC=AUC,CE=CE,SVAUC=SVAUC,AUC_vec = AUC_vec,iter_vec_thin=iter_vec_thin,result_plinks=result_plinks,
  CE_vec=CE_vec,SVAUC_vec=SVAUC_vec,max_diff=max_diff))
}