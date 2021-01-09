#Dimitrios Mitras 3084
#DBScan algorithm 

 

#choose Eps and minpts
#Eps = maximum distance beetwen two points if they are in the same cluster
#minpts = minimum digits of points which are shaped a cluster 

#function DBscan
DBSCAN<-function(data , eps , minpts){
  

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

  #plot
  #manual plotting , depends on number of clusters
  plot(data)
  points(data[neighborhoods[,1],] , col="purple")
  points(data[neighborhoods[,2],], col="red")
  points(data[neighborhoods[,3],] , col="green")
  points(data[neighborhoods[,4],] , col ="yellow")



  #if its not clear the isVoice points
  #lines(data[neighborhoods[,1],] , col="purple")
  #lines(data[neighborhoods[,2],], col="red")

} #end of function


eps=0.5
minpts=8

?iris
data<-iris[-5]
data<-data[-4]
data<-data[-1]
DBSCAN(data , eps , minpts)


 
 
 

