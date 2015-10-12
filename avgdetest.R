rm(list = ls())
library(igraph)
require(ggplot2)
setwd("~/code/R/googleplus")
source("util.R")

# Select network:
# google
# facebook
# twitter
network <- "google" 
cat("Network:",network)

cat("Loading graph...\n")
load(get.path(network,"graph.RDa"))


load(get.path(network,"circles.RDa"))



load(get.path(network,"groupmetrics.RDa"))




user.circle <- read.table(get.path(network,"user_circle.csv"), 
                          header=F,sep=",", 
                          col.names=c("user","circle"),
                          colClasses=c("character","character"))

######################################################################################
# Internal Connectivity / Average Degree
# f(C) = 2m_C/n_C
# n_C = # vertices in circle
# m_C = # edges in circle
######################################################################################
average.degree <- function(sub.graph){
  g <- as.undirected(sub.graph,mode="collapse")
  (2*ecount(g))/vcount(g)
}


average.degree.2 <- function(sub.graph){
  (2*ecount(sub.graph))/vcount(sub.graph)
}


#sub.graph <- induced.subgraph(graph,V(graph)[circle])  
#reciprocity <- sapply(V(sub.graph),relation.reciprocity, graph=sub.graph)


#data <- data.frame(circle=character(0),
#                   avgdegree=numeric(0),
#                   cutratio=numeric(0),
#                   conductance=numeric(0),
#                   modularity=numeric(0))


library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)


data.2 <- foreach(circle = circles,.combine=rbind)%dopar%{
  #data <- data.frame(circle=character(0),avgdegree=numeric(0),cutratio=numeric(0),conductance=numeric(0))
  #for(circle in circles){  
  require(igraph)
  cC <- length(E(graph)[xor(from(circle),to(circle))])
  sub.graph <- induced.subgraph(graph,V(graph)[circle])  
  ad <- average.degree(sub.graph)
  ad2 <- average.degree.2(sub.graph)
  data.frame(avgdegree=ad,conductance=ad2)
  #print(a)
  #data <- rbind(data,a)
}

stopCluster(cl)

#save(data,file=get.path(network,"groupmetrics.RDa"))
#load(get.path(network,"groupmetrics.RDa"))

library(ggplot2)
require(grid)
rancircle <- data.frame(type=as.factor(c(
  rep("undirected",length(data[,1])),
  rep("directed",length(data.2[,1]))
)),
value=c(data[order(data$avgdegree,decreasing=T),]$avgdegreee,
        data.2[order(data.2$avgdegree,decreasing=T),]$avgdegree))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type), size=1.5) +
  xlab("Average Degree") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position=c(0.8,0.4),legend.key.width=unit(1,"in"))

