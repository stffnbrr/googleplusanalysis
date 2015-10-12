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


all.raw <- lapply(circles,function(x){mean(degree(graph,x,mode="all"))})
all <- sapply(seq(1,length(all.raw)),function(i){all.raw[[i]]})
all <- sort(all, decreasing = TRUE)

in.raw <- lapply(circles,function(x){mean(degree(graph,x,mode="in"))})
ind <- sapply(seq(1,length(in.raw)),function(i){in.raw[[i]]})
ind <- sort(ind, decreasing = TRUE)

out.raw <- lapply(circles,function(x){mean(degree(graph,x,mode="out"))})
out <- sapply(seq(1,length(out.raw)),function(i){out.raw[[i]]})
out <- sort(out, decreasing = TRUE)

par(mfrow = c(1,3))
plot(all, ylab= "Degree (log)",log="y")
plot(ind, ylab="In-Degree (log)", log="y")
plot(out, ylab="Out-Degree (log)", log="y")

load(get.path(network,"groupmetrics.RDa"))


plot(all,data$avgdegree)

plot(all,data$cutratio)

plot(all,data$conductance)

plot(all,data$modularity)



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
  g1 <- degree.sequence.game(degree(graph,mode='out'),degree(graph,mode='in'),method="vl")
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

#save(data,file=get.path(network,"groupmetrics.RDa"))
load(get.path(network,"groupmetrics.RDa"))

load("~/code/R/googleplus/twitter/egonetworks.RDa")
load(get.path(network,"circles.RDa"))

ego.random.circles <- function(ego.networks){
  random.circles <- list()
  for(i in 1:length(circles)){
    ego.network <- sample(ego.networks,1)[[1]] 
    length <- length(circles[[i]])
    if(length > length(ego.network)){
      length <- length(ego.network)
    }
    random.circles[[i]] <- sample(ego.network,length)
  }
  random.cricles
}

walk.random.circles <- function(graph){
  random.circles <- list()
  lengths <- as.vector(sapply(circles,function(c){length(c)}))
  count <- length(circles)
  
  for(i in 1:count){
    current <- sample(V(graph),1)
    start <- current
    circle <- c()
    a <- sample(lengths,1)
    
    for(x in 1:a){
      print(current)
      circle <- c(circle,current)
      neighbors <- neighbors(graph,current,mode="all")
      b <- length(neighbors)
      neighbors <- setdiff(neighbors,circle)
      cat("Neighbors: ", length(neighbors), " Circle: ", length(circle), "Diff:", b, "\n" )
      while(is.null(neighbors) || length(neighbors) == 0){
        cat("bäm")
        current <- start
        neighbors <- neighbors(graph,current,mode="all")
        neighbors <- setdiff(neighbors,circle)
        cat("Neighbors: ", length(neighbors), " Circle: ", length(circle), "Diff:", b, "\n" )
      }
      current <- sample(neighbors,1)
    }
    
    random.circles[[i]] <- circle
  }
  
  random.circles
}

random.circles <- walk.random.circles(graph)

#random.circles <- ego.random.circles(ego.networks)



library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)


walk.data.random <- foreach(circle = random.circles,.combine=rbind)%dopar%{
  #data <- data.frame(circle=character(0),avgdegree=numeric(0),cutratio=numeric(0),conductance=numeric(0))
  #for(circle in circles){  
  require(igraph)
  cC <- length(E(graph)[xor(from(circle),to(circle))])
  sub.graph <- induced.subgraph(graph,V(graph)[circle])  
  g1 <- degree.sequence.game(degree(graph,mode='out'),degree(graph,mode='in'))
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

save(data.random,file=get.path(network,"walknetworkgroupmetricsrandom.RDa"))
save(data.random,file=get.path(network,"egonetworkgroupmetricsrandom.RDa"))
load(get.path(network,"groupmetricsrandom.RDa"))
load(get.path("google","groupmetrics.RDa"))


walk.data.random <- data.random

rancircle <- data.frame(type=as.factor(c(
  rep("Circles",length(data[,1])),
  rep("Random Walk",length(walk.data.random[,1]))
)),
value=c(data[order(data$avgdegree,decreasing=T),]$avgdegree,
        walk.data.random[order(walk.data.random$avgdegree,decreasing=T),]$avgdegree))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
av <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type), size=1.5) +
  scale_x_continuous(limits = c(0, 331))+
  xlab("Average Degree") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position=c(0.8,0.4),legend.key.width=unit(1,"in"))


rancircle <- data.frame(type=as.factor(c(
  rep("Circles",length(data[,1])),
  rep("Random Walk",length(walk.data.random[,1]))
)),
value=c(data[order(data$cutratio,decreasing=T),]$cutratio, 
        walk.data.random[order(walk.data.random$cutratio,decreasing=T),]$cutratio))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
rc <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type, linetype = type), size=1.5) +
  xlab("Ratio Cut") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")


rancircle <- data.frame(type=as.factor(c(
  rep("Cricles",length(data[,1])),
  rep("Random Walk",length(walk.data.random[,1]))
)),
value=c(data[order(data$conductance,decreasing=T),]$conductance, 
        walk.data.random[order(walk.data.random$conductance,decreasing=T),]$conductance))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
co <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type), size=1.5) +
  xlab("Conductance") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

mod <- data.frame(type=as.factor(c(
  rep("Cricles",length(data[,1])),
  rep("Random Walk",length(walk.data.random[,1]))
)),
value=c(data[order(data$modularity,decreasing=T),]$modularity, 
        walk.data.random[order(walk.data.random$modularity,decreasing=T),]$modularity))
mydf_m <- ddply(mod,.(type),transform, ecd = ecdf(value)(value))
mo <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type), size=1.5) +
  xlab("Modularity") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

require(gridExtra)
grid.arrange(av, rc, co, mo, ncol=2)

