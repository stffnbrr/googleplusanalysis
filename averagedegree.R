rm(list = ls())
library(igraph)
require(ggplot2)
setwd("~/code/R/googleplus")
source("util.R")

# Select network:
# google
# facebook
# twitter
cat("Loading graph...\n")
network <- "google" 
cat("Network:",network)
load(get.path(network,"graph.RDa"))
google <- graph
load(get.path(network,"circles.RDa"))
google.c <- circles

load(get.path("facebook","graph.RDa"))
facebook <- graph
load(get.path("facebook","circles.RDa"))
facebook.c <- circles


load(get.path("twitter","graph.RDa"))
twitter <- graph
load(get.path("twitter","circles.RDa"))
twitter.c <- circles

load("livejournal/graph.RDa")
livejournal <- graph
load("livejournal/communities.RDa")
livejournal.c <- communities

load("/nfs/projects/mindstone/orkut/graph.RDa")
orkut <- graph
load("/nfs/projects/mindstone/orkut/communities.RDa")
orkut.c <- communities


average.degree <- function(sub.graph){
  (2*ecount(sub.graph))/vcount(sub.graph)
}

average.degree.2 <- function(sub.graph){
  g <- as.undirected(sub.graph,mode="collapse")
  (2*ecount(g))/vcount(g)
}

library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)


circles <- orkut.c
graph <- orkut

data <- foreach(circle = circles,.combine=rbind)%dopar%{
#data <- data.frame(avgd=numeric(0),avgd2=numeric(0))
#for(circle in circles){  
  require(igraph)
  sub.graph <- induced.subgraph(graph,V(graph)[circle])  
  ad <- average.degree(sub.graph)
  ad2 <- average.degree.2(sub.graph)
  data.frame(avgd=ad,
             avgd2=ad2)
  #print(a)
  #data <- rbind(data,a)
}

orkut.data <- data
stopCluster(cl)

datasets <- list("Google"=google.data,
               "Twitter"=twitter.data,
               "LiveJournal"=livejournal.data,
               #    "Facebook"=facebook,
               "Orkut"=orkut.data)

  graph.names <- names(datasets)
  networks <- unlist(lapply(graph.names,function(x){rep(x,length(datasets[[x]][,1]))}))
  value1 <- c()
  for(dataset in datasets){
    value1 <- c(value1,dataset[order(dataset["avgd"],decreasing=T),][["avgd"]])
  }
  value2 <- c()
  for(dataset in datasets){
    value2 <- c(value2,dataset[order(dataset["avgd2"],decreasing=T),][["avgd2"]])
  }
  data <- data.frame(network=as.factor(networks),value=value1)
  networks2 <- unlist(lapply(graph.names,function(x){rep(paste(x,"2",sep=""),length(datasets[[x]][,1]))}))
  data <- rbind(data,data.frame(network=as.factor(networks2),value=value2))

mydf_m <- ddply(data,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network))
