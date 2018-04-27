bff = function(clusters,features, nfeatures = 10){  # best features function
  # features is n x d matrix; 
  #    colnames(features) should be "meaningful" (e.g. the dictionary in bag of words)
  # clusters is an n vector of cluster labels 1:K.
  # this function returns an nfeatures x K matrix where the i,j th element is 
  #   the ith "most important" feature for cluster j. 
  
  clusterLabels = unique(clusters) %>% sort
  nclust = length(clusterLabels)
  
  bestFeatures = matrix("", ncol = nclust, nrow = nfeatures)
  colnames(bestFeatures) = clusterLabels
  binary = max(features)==1
  for(i in 1:nclust){
    clust_i = which(clusters == clusterLabels[i])
    if(!binary) vals =  (sqrt(colMeans(features[clust_i,])))- (sqrt(colMeans(features[-clust_i,])))
    if(binary) vals =  asin(sqrt(colMeans(features[clust_i,])))- asin(sqrt(colMeans(features[-clust_i,])))
    bestFeatures[,i] = (names(vals)[order(-vals)[1:nfeatures]])
    #wordcloud( words= names(vals),freq = vals, min.freq  = sort(vals,decreasing = T)[20], random.order = F, rot.per = 0, scale = c(2,0.5))
  }
  return(bestFeatures)
}