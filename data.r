
#single link data
#dataset of random generated numbers that creates three clusters 
#of varying density but that are also distinctable by distance

x <- c(runif(30, min=0, max=2), runif(3, min=0, max=4), runif(3, min=3, max=4), runif(5, min=6, max=10), runif(5, min=0, max=5))
y <- c(runif(30, min=8, max=10), runif(3, min=5, max=7), runif(3, min=5, max=10), runif(5, min=0, max=5), runif(5, min=-5, max=3))
real_class <- c(rep('a',36), rep('b',5), rep('c',5))
sl_data <- data.frame(x, y ,real_class )

write.csv(as.matrix(sl_data), "single_link_better.csv")


#dbs_data 
#dataset of random generated numbers that creates three clusters of similar density
#and sparse noise between them
x <- c(runif(20, min=0, max=2), runif(20, min=5, max=7), runif(20, min=2, max=4), runif(10, min=0, max=7))
y <- c(runif(20, min=9, max=11), runif(20, min=-1, max=1), runif(20, min=-5, max=-3), runif(10, min=-5, max=10))
real_class <- c(rep('a', 20), rep('b', 20), rep('c', 20), rep('noise', 10))
dbs_data <- data.frame(x,y, real_class)

write.csv(as.matrix(dbs_data), "dbscan_better.csv")
