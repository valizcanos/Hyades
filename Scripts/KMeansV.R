library(factoextra)

#####################################################################################################
TheBestKMeans = function(Dataset, Metodo){
  # wss and silhouette are the methods
  Dataset_Scaled = scale(Dataset)
  if(Metodo == "wss"){
    fviz_nbclust(Dataset_Scaled, kmeans, method = paste(Metodo))
  } else{
    fviz_nbclust(Dataset_Scaled, kmeans, method = paste(Metodo))
  }
}


MakeKMeans = function(Dataset, NumberCenters, NumIterations, NumStart){
  Dataset_Scaled = scale(Dataset)
  K_M = kmeans(Dataset_Scaled, centers =  NumberCenters,iter.max = NumIterations, nstart = NumStart) # Datos K-means
  Add_K_M = cbind(Dataset, cluster = K_M$cluster) #Datos con valores K
  Grap = fviz_cluster(K_M, geom="point",data=Dataset, labelsize = 16) #Grafica de k-means
  return(list(K_M, Add_K_M, Grap))
}


#####################################################################################################


