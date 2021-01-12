library(tidyverse)

#loading the data
sl_data <- read.csv(file = 'single_link_better.csv')
dbs_data <- read.csv(file= 'dbscan_better.csv')
sl_data <- sl_data[-1]
dbs_data <- dbs_data[-1]

#running the functions

#where DBScan is better 
single_link(dbs_data, 4)
DBSCAN(dbs_data , eps=1, minpts=5)

#where Single Link is better
single_link(sl_data, 3)
DBSCAN(sl_data, eps=2, minpts=2)

#plotting the dendogram
dend <- hclust(dist(sl_data), method="single")
plot(dend)


### THE FUNCTIONS ###


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








#choose Eps and minpts
#Eps = maximum distance beetwen two points if they are in the same cluster
#minpts = minimum digits of points which are shaped a cluster 
DBSCAN<-function(data , eps , minpts){
  
  #seperating data and real class
  real_class <- data[,3]
  data <- data[-3]
  
  length<-as.numeric(lengths(data[1],use.names = TRUE)) #total length of the points
  
  visited<-array(FALSE, length) #every point which is not noise must been visited after the loop
  isNoise<-array(FALSE, length) #no need visit them
  
  cluster<-c() #create the basic array, contains all points which are include in a cluster , actually all points except noises, it is filled with positive integers which are refered to a data row
  k=1
  nofclusters <- 0
  distance <- as.matrix(dist(data)) #array[length,length]
  
  
  #put noises away
  for (i in 1:(length -1) ){  
    itspnts <-0
    for(j in 1 :length){
      if(i!=j){
        if (!is.null(distance[i,j]) && (distance[i,j]<eps)){
          itspnts=itspnts + 1
        }
      }
      if(itspnts>minpts)
        break;
    }
    
    if (itspnts<minpts)
      isNoise[i]=TRUE  
    
  }
  
  #for every point
  for (i in 1:(length-1)){
    if (isNoise[i]==FALSE) #ignore it
    {
      #finding neighbors of every point
      neighbours<-c(i)
      for(j in 1 :length){
        if (!is.null(distance[i,j]) && (distance[i,j]<eps))
        {
          neighbours<-c(neighbours ,  j)
        }
      }
      
      
      
      case<-FALSE 
      #if at least one point of the neighborhood is in array clusters[], it means that all points int the neighborhhod are in the cluster
      for (k in 1:length(neighbours)){
        if(visited[neighbours[k]]==TRUE){ #At least one point is in a neighborhood => all current neighborhood in this neighborhood except noises
          #must put them in correct neighborhood
          inClusterCage = -1
          for(kk in 1:length(cluster)){
            if (cluster[kk]== neighbours[k])
            {
              inClusterCage = kk;
              break;
            }
          }
          cluster<- c (cluster[1:inClusterCage], neighbours, cluster[inClusterCage: length(cluster)]) #so all neighbors are put in the correct cluster 
          case<-TRUE #we want it to delete points which exist already in the array cluster()
          break;
        }
      }
      #it means that it is a new neighborhood/cluster
      if (case==FALSE){ 
        cluster<-c(cluster, nofclusters ,neighbours) #i use a negative int variable: nofclusters, to know when we have a new cluster 
        nofclusters<-nofclusters-1
      }
      #must be unique elements
      else{
        cluster<-unique(cluster, incomparables = FALSE, MARGIN = 1, fromLast = FALSE)
        cluster <- cluster[!is.na(cluster)]
      }
      for (k in 1:length(neighbours)){
        visited[neighbours[k]]=TRUE
      }
    }# end of if(isNoise==FALSE) statement
  }#end of for(i in 1:(length-1)) loop
  
  
  nofclusters<-abs(nofclusters) #real number of clusters
  neighborhoods<-matrix(nrow=lengths(data , use.names=TRUE), ncol=nofclusters) #create a matrix in which every row haw a cluster
  i=1
  number<-0
  j<-0
  for(i in 1:length(cluster)){
    if (cluster[i]>0){
      j<-j+1
      neighborhoods[j,number] <- cluster[i]
    }
    else{
      number<-number+1
      j<-0
      
    }
  }
  
  
  #making it possible to plot with ggplot
  plot_data <- data
  plot_data$class <- NA #creates class column 
  #get number of clusters
  clust_num <- ncol(neighborhoods)
  #for each neighbourhood, add a class name to the dataset
  for(x in 1:clust_num){
    plot_data$class[neighborhoods[, x]] <- as.character(x)
  }
  
  ggplot(plot_data) + 
    geom_point(aes(x=data[,1],y=data[,2], color=class, shape=real_class))
  
} #end of function



#The code with which the data was created

# #single link data
# #dataset of random generated numbers that creates three clusters 
# #of varying density but that are also distinctable by distance
# 
# x <- c(runif(30, min=0, max=2), runif(3, min=0, max=4), runif(3, min=3, max=4), #class a
#        runif(5, min=6, max=10),                                                 #class b
#        runif(5, min=0, max=5))                                                  #class c
# y <- c(runif(30, min=8, max=10), runif(3, min=5, max=7), runif(3, min=5, max=10), 
#        runif(5, min=0, max=5), 
#        runif(5, min=-5, max=3))
# real_class <- c(rep('a',36), rep('b',5), rep('c',5))
# sl_data <- data.frame(x, y ,real_class )
# 
# write.csv(as.matrix(sl_data), "single_link_better.csv")
# 
# 
# #dbs_data 
# #dataset of random generated numbers that creates three clusters of similar density
# #and sparse noise between them
# x <- c(runif(20, min=0, max=2), runif(20, min=5, max=7), runif(20, min=2, max=4), runif(10, min=0, max=7))
# y <- c(runif(20, min=9, max=11), runif(20, min=-1, max=1), runif(20, min=-5, max=-3), runif(10, min=-5, max=10))
# real_class <- c(rep('a', 20), rep('b', 20), rep('c', 20), rep('noise', 10))
# dbs_data <- data.frame(x,y, real_class)
# 
# write.csv(as.matrix(dbs_data), "dbscan_better.csv")
