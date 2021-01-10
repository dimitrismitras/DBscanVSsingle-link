library(tidyverse)

#performs single link hierarchical clustering and plots the clusters
#works for 2D datasets
#df is the dataframe and NUM the number of clusters
single_link <- function(df, NUM){
  
  #seperating data and real class
  real_class <- df[,3]
  df <- df[-3]
  
  #calulating all distances
  distance <- as.matrix(dist(df))
  
  #getting rid of zeros of the diag
  diag(distance) <- Inf
  
  #initializing clusters lists to store cluster points (named 1:number of points)
  clus.names <- c(rownames(df))
  clus <- vector("list", length(clus.names))
  names(clus) <- clus.names
  #initially, each point is its own cluster
  for(x in names(clus)){clus[[x]] <- append(clus[[x]],as.numeric(x))}
  
  #condition to break the loop
  done <- FALSE
  
  #repeat until we have the wanted number of clusters (param NUM)
  while(done == FALSE){
    
    #if wanted clusters equal the number of points, exit with one point per cluster
    if(NUM == nrow(df)) {break}
    
    #if wanted clusters more than points, exit
    if(NUM > nrow(df)) {
      print((paste0("Error: Exceeded max number of clusters (", nrow(df), ")")))
      break
    }
    
    #find the indexes of the minimum distance in distance matrix
    min_d <- which(distance == min(distance), arr.ind = T)[1,]
    
    
    #find in which cluster the points with min distance belong
    cluster1 <- 0
    cluster2 <- 0
    
    for(i in names(clus)){
      if(min_d[1] %in% clus[[i]]){
        cluster1 <- i
      }
      if(min_d[2] %in% clus[[i]]){
        cluster2 <- i
      }
    }
    
    
    #IF they don't already belong in the same cluster, put them in the same cluster
    #of first point, clear the other one
    if(cluster1 != cluster2){
      clus[[cluster1]] <- append(clus[[cluster1]], clus[[cluster2]])
      clus[[cluster2]] <- NA
    }
    
    #remove the current min from the distance matrix so it won't be used again
    distance[as.numeric(min_d[1]), as.numeric(min_d[2])] <- Inf
    distance[as.numeric(min_d[2]), as.numeric(min_d[1])] <- Inf
    
    
    #termination condition: reaching the desired number of clusters
    clust_num <- 0 #number of clusters created
    
    for(j in names(clus)){ #for each cluster
      if(is.numeric(clus[[j]])) { #has elements inside (numerics)
        clust_num <- clust_num+1 #increase cluster counter
      }
    }
    
    #if desired cluster number met, condition = TRUE
    if (clust_num == NUM) {done <- T}
    
  } #exit loop
  
  
  #once the clustering is complete, we must assign each point of the dataset a class
  
  #remove NA values from clus
  clus <- clus[!is.na(clus)]
  
  #initialize class column 
  df$class <- NA
  
  for(x in as.numeric(names(clus))){ #for each cluster in clus
    for(y in clus[[as.character(x)]]){ #look for the elements in each cluster
      df[y,"class"] <- as.character(x) #assign corresponding df row the cluster name
    }
  }
  
  
  #plottin the data
  #color is the clusters assigned by the algorithm and shape is the true class if the data
  ggplot(df) + 
    geom_point(aes(x=df[,1],y=df[,2], color=class, shape=real_class))
  
}


#reading the datasets
sl_data <- read.csv(file = 'single_link_better.csv')
dbs_data <- read.csv(file= 'dbscan_better.csv')

#running the algorithms
single_link(sl_data[-1], 3)
single_link(dbs_data[-1], 3)
