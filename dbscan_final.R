library(tidyverse)

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

#reading the datasets
sl_data <- read.csv(file = 'single_link_better.csv')
dbs_data <- read.csv(file= 'dbscan_better.csv')

#running the algorithm
DBSCAN(dbs_data[-1] , eps=1.5, minpts=2) #better for dbscan 
DBSCAN(sl_data[-1], eps=2, minpts=1) #better for single link
