## this R document contains the function CE_area, CE_plot_cut, area_ROC_CE_per_iter and ROC_plot_cut
##CE_area takes as input an actual matrix and a predictor matrix. It calculates the area under the CE curve. This area is equal to the calibration error
##CE_lot_cut takes as input an actual matrix and a predictor matrix. It outputs a vector of alpha values with corresponding CE(alpha) values. Optionally it plots these vectors and calculates the area under thsi curve
##area_ROC_CE_per_iter takes an bdgraph.obj and an actual matrix as input. It calculates the area under the ROC curve and the area under the CE curve at every x iterations, where x = thin. 

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

area_ROC_CE_per_iter = function (all_graphs=NULL,sample_graphs=NULL,all_weights=NULL, actual, thin = NULL, verbose = TRUE) 
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
  if (is.null(thin)) 
    stop("'thin' must be a number")
  if (!is.numeric(thin)) 
    stop("'thin' must be a number")
  if (is.matrix(thin)) 
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
  iter_vec_thin = c(thin * (1:floor(iter/thin)))
  auROC_vec = c(rep(0,length(iter_vec_thin)))
  auCE_vec = c(rep(0,length(iter_vec_thin)))
  SVAUC_vec = c(rep(0,length(iter_vec_thin)))
  
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
    auROC_vec[i] = auc(pROC::roc(response=response,predictor=predictor,quiet=TRUE))[1]
    auCE_vec[i] = sum(abs(predictor-response))/qp
    SVAUC_vec[i] = calc_SVAUC(predictor=predictor,response=response)
  }
  
  #calculate final values AUC, CE, SVAUC and p_links
  plinks = result_plinks[,iter] 
  AUC = auc(pROC::roc(response=response,predictor=plinks,quiet=TRUE))[1]
  CE = sum(abs(plinks-response))/qp
  SVAUC = calc_SVAUC(predictor=plinks,response=response)

  if (verbose == TRUE) {
    mes = paste(c("Calculation ... done.                        "), 
                collapse = "")
    cat(mes, "\r")
    cat("\n")
    utils::flush.console()
  }
  
  return(list(plinks=plinks,AUC=AUC,CE=CE,SVAUC=SVAUC,auROC_vec = auROC_vec,iter_vec_thin=iter_vec_thin,result_plinks=result_plinks,auCE_vec=auCE_vec,SVAUC_vec=SVAUC_vec))
}