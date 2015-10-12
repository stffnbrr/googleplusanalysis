rm(list = ls())

path <- "ego-fb/facebook/"

cat("Loading list of vertices...\n")
# List files with ".circles" extension

t1 <- proc.time()
files <- list.files(path = path, pattern="\\.circles$")
t2 <- proc.time()
print(t2-t1)
# cut ".cirlces" from file list

t1 <- proc.time()
a <- lapply(files,strsplit,split="\\.")
start.vertices <- sapply(a,function(x)x[[1]][1])
t2 <- proc.time()
print(t2-t1)

#load.circles <- function(start.vertices,path){
#  # Der erstellende Kreis ist zurzeit nicht im Kreis enthalten.
#  cat("Loading circles...\n")
#  t1 <- proc.time()
#  circles <- list()
#  # Read ".circles" file for a vertex
#  for(vertex in start.vertices){
#    temp.file <- file(paste(path,"/",vertex,".circles",sep=""), 'r') 
#    while(length(input <- readLines(temp.file, n=-1) > 0){ 
#      for (i in seq_along(input)){ 
#        test <- strsplit(input[i],split="\t")[[1]]
#        circles[[test[1]]] <- test[-1]
#      } 
#    }
#    close(temp.file)
#  }
#  t2 <- proc.time()
#  print(t2-t1)
#  return(circles)
#}

#circles <- load.circles(start.vertices,path)

circles <- list()
circle.edge.list <- data.frame(from=character(0), to=character(0))
user.circle <- data.frame(user=character(0),circle=character(0))
# Read ".circles" file for a vertex
for(vertex in start.vertices){
  temp.file <- file(paste(path,vertex,".circles",sep=""), 'r') 
  while(length(input <- readLines(temp.file, n=-1)) > 0){ 
    for (i in seq_along(input)){ 
      test <- strsplit(input[i],split="\t")[[1]]
      circles[[test[1]]] <- test[-1]
      user.circle <- rbind(user.circle,data.frame(user=vertex,circle=test[1]))
      b2 <- matrix(c(rep(vertex,length(test[-1])),test[-1]),ncol=2,nrow=length(test[-1]))
      circle.edge.list <- rbind(circle.edge.list,b2)
    } 
  }
  close(temp.file)
}
circle.edge.list <- unique(circle.edge.list)
save(circles,file="~/code/R/googleplus/facebook/data/circles.RDa")
cat("Cricles loaded.\n")

load.edge.list <- function(start.vertices, path){
  cat("Load edge lists...\n")
  edge.list <- data.frame(from=character(0), to=character(0))
  #edge.list <- matrix(character(0),ncol=2,nrow=0)
  for(vertex in start.vertices){
    cat("Loading edges for vertex ", which(start.vertices == vertex), "/", length(start.vertices),"...\n")
    #vertex <- start.vertices[86]  
    t1 <- proc.time()
    temp.file <- file(paste(path, "/", vertex, ".edges", sep=""), 'r')
    input <- readLines(temp.file, n=-1) 
    # Splitting rows from text file in two column matrix
    a <- lapply(input,strsplit,split=" ")
    b <- t(sapply(a,function(x)x[[1]]))  
    # Adding relation between ego node and friend nodes
    #print(length(b[]))
    #if(length(b)>0){
      #friends <- union(b[,1],b[,2])
      #b2 <- matrix(c(rep(vertex,length(friends)),friends),ncol=2,nrow=length(friends))
      #edge.list <- rbind(edge.list,b,b2)
    #}
    edge.list <- rbind(edge.list,b)
    close(temp.file)
    t2 <- proc.time()
    print(t2-t1)
    print(length(edge.list[,1]))
  }
  return(unique(edge.list))
}

load.edge.list.par <- function(start.vertices, path){
  cat("Load edge lists...\n")
  library(foreach)
  library(doParallel)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  edge.list <- foreach(vertex = start.vertices,.combine=rbind)%dopar%{
    #cat("Loading edges for vertex ", which(start.vertices == vertex), "/", length(start.vertices),"...\n")
    #vertex <- start.vertices[2]  
    #t1 <- proc.time()
    temp.file <- file(paste(path, "/", vertex, ".edges", sep=""), 'r')
    input <- readLines(temp.file, n=-1)
    # Splitting rows from text file in two column matrix
    a <- lapply(input,strsplit,split=" ")
    b <- t(sapply(a,function(x)x[[1]])) 
    # Adding relation between ego node and friend nodes
    #if(length(b)>0){
    #  friends <- union(b[,1],b[,2])
    #  b2 <- matrix(c(rep(vertex,length(friends)),friends),ncol=2,nrow=length(friends))
    #  result <- rbind(b,b2)
    #}
    result <- b
    close(temp.file)
    #t2 <- proc.time()
    #print(t2-t1)
    #print(length(edge.list[,1]))
    if(length(result) == 0){
      result <- data.frame(from=character(0), to=character(0))
    }
    result
  }
  stopCluster(cl)
  return(unique(edge.list))
}

load.followers <- function(start.vertices,path){
  edge.list <- data.frame(from=character(0), to=character(0))
  for(vertex in start.vertices){
    cat("Loading followers for vertex ", vertex, "...\n")
    #vertex <- start.vertices[2]  
    t1 <- proc.time()
    temp.file <- file(paste(path, "/", vertex, ".followers", sep=""), 'r')
    while(length(input <- readLines(temp.file, n=-1)) > 0){
      b2 <- matrix(c(rep(vertex, length(input)),input),ncol=2,nrow=length(input))
      edge.list <- rbind(edge.list,b2)
    }
    close(temp.file)
    t2 <- proc.time()
    print(t2-t1)
    print(length(edge.list[,1]))
  }
  return(unique(edge.list))
}

load.follow.par <- function(start.vertices, path){
  cat("Load vertices from feat file...\n")
  library(foreach)
  library(doParallel)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  edge.list <- foreach(vertex = start.vertices,.combine=rbind)%dopar%{
    #cat("Loading edges for vertex ", which(start.vertices == vertex), "/", length(start.vertices),"...\n")
    #vertex <- start.vertices[2]  
    #t1 <- proc.time()
    temp.file <- file(paste(path, "/", vertex, ".feat", sep=""), 'r')
    input <- readLines(temp.file, n=-1)
    # Splitting rows from text file in two column matrix
    a <- lapply(input,strsplit,split=" ")
    b <- sapply(a,function(x)x[[1]][1])
    
    # Adding relation between ego node and friend nodes
    b2 <- matrix(c(rep(vertex,length(b)),b),ncol=2,nrow=length(b))
    
    close(temp.file)
    b2
  }
  stopCluster(cl)
  return(unique(edge.list))
}

system.time(edges.edge.list <- load.edge.list.par(start.vertices, path))
cat("Edges loaded.\n")

#system.time(followers.edge.list <- load.followers(start.vertices,path))
cat("Followers loaded.\n")

system.time(follow.edge.list <- load.follow.par(start.vertices,path))
cat("Followed vertices loaded.\n")

system.time(edge.list <- rbind(edges.edge.list,follow.edge.list))
system.time(edge.list <- unique(edge.list))
cat("Edge list is unique.\n")

save.image(file="~/code/R/googleplus/twedgelist.RData")

library(igraph)
graph <- graph.data.frame(edge.list,directed=TRUE)
save(graph,file="~/code/R/googleplus/twgraph.RDa")


#export
write.table(edge.list,file="edge_list.csv",sep=",",row.names=F,col.names=F)
save(circles,file="~/code/R/googleplus/circles.RDa")
write.table(user.circle,file="~/code/R/googleplus/facebook/data/user_circle.csv",sep=",",row.names=F,col.names=F)