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
ungraph <- as.undirected(graph, mode = "collapse")

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

######################################################################################
# External Connectivity / Cut Ratio
# f(C) = c_C/n_C(n-n_C)
# c_C = # edges on the boundary
# n_C = # vertices in circle
# n = # vertices
######################################################################################
cut.ratio <- function(graph, circle,cC){
  cC/(length(circle)*(vcount(graph)-length(circle)))
}

######################################################################################
# Combined Internal and External Connectivity / Conductance
# f(C) = c_C/2m_C+c_C
# c_C = # edges on the boundary
# m_C = # edges in circle
######################################################################################
conductance <- function(sub.graph,cC){
  cC/(2*ecount(sub.graph)+cC)
}

######################################################################################
# Network Model / Modularity
# f(C) = 1/4(m_C-E(m_C))
# m_C = # edges in circle
# E
######################################################################################
modu <- function(sub.graph,random.sub){
  1/(2*ecount(graph)) * (ecount(sub.graph)-ecount(random.sub))
}


#sub.graph <- induced.subgraph(graph,V(graph)[circle])  
#reciprocity <- sapply(V(sub.graph),relation.reciprocity, graph=sub.graph)


#data <- data.frame(circle=character(0),
#                   avgdegree=numeric(0),
#                   cutratio=numeric(0),
#                   conductance=numeric(0),
#                   modularity=numeric(0))

load(get.path(network,"circles.RDa"))

library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)


data <- foreach(circle = circles,.combine=rbind)%dopar%{
  #data <- data.frame(circle=character(0),avgdegree=numeric(0),cutratio=numeric(0),conductance=numeric(0))
  #for(circle in circles){  
  require(igraph)
  cC <- length(E(graph)[xor(from(circle),to(circle))])
  sub.graph <- induced.subgraph(graph,V(graph)[circle])  
  g1 <- degree.sequence.game(degree(graph),method="vl")
  V(g1)$name <- V(graph)$name
  g1.sub <-  induced.subgraph(g1,V(g1)[circle])  
  ad <- average.degree(sub.graph)
  cr <- cut.ratio(graph,circle,cC)
  cd <- conductance(sub.graph,cC)
  
  m <- modu(sub.graph,g1.sub)
  data.frame(avgdegree=ad,
             cutratio=cr,
             conductance=cd,
             modularity=m)
  #print(a)
  #data <- rbind(data,a)
}

stopCluster(cl)
save(data,file=get.path(network,"undirectedgroupmetrics.RDa"))
dataun <- data
load(get.path("google","groupmetrics.RDa"))
google <- data



graphs <- list("directed"=google,
               "undirected"=dataun)
#"Facebook"=facebook,

columns <- c("avgdegree","conductance","cutratio","modularity")

create.ggplot.data.frame <- function(datasets, column){
  graph.names <- names(datasets)
  networks <- unlist(lapply(graph.names,function(x){rep(x,length(datasets[[x]][,1]))}))
  values <- c()
  for(dataset in datasets){
    values <- c(values,dataset[order(dataset[column],decreasing=T),][[column]])
  }
  data.frame(network=as.factor(networks),value=values)
}

require(plyr)

avgd <- create.ggplot.data.frame(graphs,"avgdegree")
mydf_m <- ddply(avgd,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network,linetype = network),size=2) +
  scale_linetype_manual(breaks=c("directed","undirected"), values=c(5,3))+
  xlab("Average Degree") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position=c(0.7,0.4),legend.key.width=unit(1,"in"))
# legend.position = "none"


ratioc <- create.ggplot.data.frame(graphs,"cutratio")
mydf_m <- ddply(ratioc,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network, linetype = network),size=2) +
  scale_linetype_manual(breaks=c("directed","undirected"), values=c(5,3))+
  xlab("Ratio Cut") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

cond <- create.ggplot.data.frame(graphs,"conductance")
mydf_m <- ddply(cond,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network, linetype = network),size=2) +
  scale_linetype_manual(breaks=c("directed","undirected"), values=c(5,3))+
  xlab("Conductance") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

mod <- create.ggplot.data.frame(graphs,"modularity")
mydf_m <- ddply(mod,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network,linetype = network),size=2) +
  scale_linetype_manual(breaks=c("directed","undirected"), values=c(5,3))+
  xlab("Modularity") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")
