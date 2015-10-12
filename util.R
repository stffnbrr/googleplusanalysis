library(igraph)

create.matrix <- function(vertex, friends,out=TRUE){
  if(out){
    result <- matrix(c(rep(vertex, length(friends)),friends),ncol=2,nrow=length(friends))  
  }else{
    result <- matrix(c(friends, rep(vertex, length(friends))),ncol=2,nrow=length(friends))
  }
  return(result)
}

relation.reciprocity <- function(graph, vertex){
  os <- neighbors(graph,V(graph)[vertex],mode="out")
  is <- neighbors(graph,V(graph)[vertex],mode="in")
  length(intersect(os,is))/length(os)
}

get.path <- function(network,file){
  paste("~/code/R/googleplus/",network, '/data/',file, sep="")
}

create.ggplot.data.frame <- function(datasets, column){
  graph.names <- names(datasets)
  networks <- unlist(lapply(graph.names,function(x){rep(x,length(datasets[[x]][,1]))}))
  values <- c()
  for(dataset in datasets){
    values <- c(values,dataset[order(dataset[column],decreasing=T),][[column]])
  }
  data.frame(network=as.factor(networks),value=values)
}


