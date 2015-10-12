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


a <- sapply(circles,function(x){mean(degree(graph,x,mode="in"))})
df.vin <- data.frame(v=1:length(a),x=sort(a,decreasing=TRUE))

require(scales)
ggplot(df.vin, aes(v, x)) +
  geom_point() +
  scale_y_continuous(trans=log_trans(),breaks=c(8,150,3000)) +
  xlab("Circle (Rank)") +
  ylab("In-Degree (#)")+
  theme_bw()


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

save(data,file=get.path(network,"groupmetrics.RDa"))
#load(get.path(network,"groupmetrics.RDa"))

old.groupmetrics <- function(){

data.1 <- data[order(data$avgdegree,decreasing=T) , ]
data.1["avgrank"] <- 1:length(data.1[,1])

data.1 <- data.1[order(data.1$cutratio,decreasing=T) , ]
data.1["cutrank"] <- 1:length(data.1[,1])

data.1 <- data.1[order(data.1$conductance,decreasing=T) , ]
data.1["conrank"] <- 1:length(data.1[,1])

data.1 <- data.1[order(data.1$modularity,decreasing=T) , ]
data.1["modrank"] <- 1:length(data.1[,1])

max.avgdegree <- max(data.1$avgdegree)
max.cutratio <- max(data.1$cutratio)
max.conductance <- max(data.1$conductance)
max.modularity <- max(data.1$modularity)

data.1$avgdegree <- data.1$avgdegree / max.avgdegree
data.1$cutratio <- data.1$cutratio / max.cutratio
data.1$conductance <- data.1$conductance / max.conductance
data.1$modularity <- data.1$modularity / max.modularity

data.2 <- data.frame(metric= as.factor(c(rep("Average Degree",468),
                       rep("Cut Ratio",468),
                       rep("Conductance",468),
                       rep("Modularity",468))),
                     rank= c(1:468,1:468,1:468,1:468),
                     value= c(data.1[order(data.1$avgdegree,decreasing=T) , ]$avgdegree,
                              data.1[order(data.1$cutratio,decreasing=T) , ]$cutratio,
                              data.1[order(data.1$conductance,decreasing=T) , ]$conductance,
                              data.1[order(data.1$modularity,decreasing=T) , ]$modularity))

par(mfrow=c(2,2))
plot(data.1[order(data.1$avgdegree,decreasing=T),]$avgdegree,ylab="Metric value",xlab="Circles",main="Average Degree")
plot(data.1[order(data.1$cutratio,decreasing=T),]$cutratio,ylab="Metric value",xlab="Circles",main="Cut Ratio")
plot(data.1[order(data.1$conductance,decreasing=T),]$conductance,ylab="Metric value",xlab="Circles",main="Conductance")
plot(data.1$modularity,ylab="Metric value",xlab="Circles",main="Modularity")

ggplot(data.2, aes(x = rank, y = value)) + 
  geom_line(aes(group = metric,colour = metric)) +
  xlab("Circles") +
  ylab("Metric Value") +
  theme(legend.title=element_blank())
ggsave(filename="groupmetrics.pdf",width=8,height=6,dpi=100)


require(plyr)
mydf_m <- ddply(data.2,.(metric),transform, ecd = ecdf(value)(value))

ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = metric,colour = metric)) +
  xlab("Metric Value") +
  ylab("CDF") +
  theme(legend.title=element_blank())
ggsave(filename="groupmetrics_cdf.pdf",width=8,height=6,dpi=100)
}

random.circles <- list()
for(i in 1:length(user.circle[,1])){
  random.circles[[as.character(i)]] <- sample(V(graph)$name,sample(10:50))
}

library(foreach)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)


data.random <- foreach(circle = random.circles,.combine=rbind)%dopar%{
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

save(data.random,file=get.path(network,"groupmetricsrandom.RDa"))
load(get.path(network,"groupmetricsrandom.RDa"))
load(get.path("google","groupmetrics.RDa"))


rancircle <- data.frame(type=as.factor(c(
  rep("Cricles",length(data[,1])),
  rep("Random Sets",length(data.random[,1]))
)),
value=c(data[order(data$avgdegree,decreasing=T),]$avgdegree, 
        data.random[order(data.random$avgdegree,decreasing=T),]$avgdegree))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
av <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type)) +
  scale_x_continuous(limits = c(0, 331))+
  xlab("Average Degree") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")


rancircle <- data.frame(type=as.factor(c(
  rep("Cricles",length(data[,1])),
  rep("Random Sets",length(data.random[,1]))
)),
value=c(data[order(data$cutratio,decreasing=T),]$cutratio, 
        data.random[order(data.random$cutratio,decreasing=T),]$cutratio))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
rc <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type, linetype = type)) +
  xlab("Ratio Cut") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")


rancircle <- data.frame(type=as.factor(c(
  rep("Cricles",length(data[,1])),
  rep("Random Sets",length(data.random[,1]))
)),
value=c(data[order(data$conductance,decreasing=T),]$conductance, 
        data.random[order(data.random$conductance,decreasing=T),]$conductance))
require(plyr)
mydf_m <- ddply(rancircle,.(type),transform, ecd = ecdf(value)(value))
co <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type)) +
  xlab("Conductance") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

mod <- data.frame(type=as.factor(c(
  rep("Cricles",length(data[,1])),
  rep("Random Sets",length(data.random[,1]))
)),
value=c(data$modularity, 
        data.random$modularity))
mydf_m <- ddply(mod,.(type),transform, ecd = ecdf(value)(value))
mo <- ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = type,color = type,linetype = type)) +
  xlab("Modularity") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")



require(gridExtra)
grid.arrange(av, rc, co, mo, ncol=2)


max.random.avgdegree <- max(data.random$avgdegree)
max.random.cutratio <- max(data.random$cutratio)
max.random.conductance <- max(data.random$conductance)
max.random.modularity <- max(data.random$modularity)

data.random$avgdegree <- data.random$avgdegree / max.random.avgdegree
data.random$cutratio <- data.random$cutratio / max.random.cutratio
data.random$conductance <- data.random$conductance / max.random.conductance
data.random$modularity <- (data.random$modularity/ max.random.modularity +1)/2

data.random.2 <- data.frame(metric= as.factor(c(rep("Average Degree",468),
                                         rep("Cut Ratio",468),
                                         rep("Conductance",468),
                                         rep("Modularity",468))),
                     rank= c(1:468,1:468,1:468,1:468),
                     value= c(data.random[order(data.random$avgdegree,decreasing=T) , ]$avgdegree,
                              data.random[order(data.random$cutratio,decreasing=T) , ]$cutratio,
                              data.random[order(data.random$conductance,decreasing=T) , ]$conductance,
                              data.random[order(data.random$modularity,decreasing=T) , ]$modularity))

data.1 <- data
#data.1$conductance <- 1- data.1$conductance 
#data.random$conductance <- 1- data.random$conductance
par(mfrow=c(2,2))
plot(data.1[order(data.1$avgdegree,decreasing=T),]$avgdegree,ylab="Average Degree",xlab="Ranked Circles",main="Average Degree")
lines(data.random[order(data.random$avgdegree,decreasing=T),]$avgdegree,type="p",pch=23,col="red")

plot(data.1[order(data.1$cutratio,decreasing=T),]$cutratio,ylab="Ratio Cut",xlab="Ranked Circles",main="Ratio Cut")
lines(data.random[order(data.random$cutratio,decreasing=T),]$cutratio,type="p",pch=23,col="red")

plot(data.1[order(data.1$conductance,decreasing=T),]$conductance,ylab="Conductance",xlab="Ranked Circles",main="Conductance")
lines(data.random[order(data.random$conductance,decreasing=T),]$conductance,type="p",pch=23,col="red")

plot(data.1[order(data.1$modularity,decreasing=T),]$modularity,ylab="Modulartiy",xlab="Ranked Circles",main="Modularity")
lines(data.random[order(data.random$modularity,decreasing=T) , ]$modularity,,type="p",pch=23,col="red")




ggplot(data.random.2, aes(x = rank, y = value)) + 
  geom_line(aes(group = metric,colour = metric)) +
  xlab("Circles") +
  ylab("Metric Value") +
  theme(legend.title=element_blank())
ggsave(filename="groupmetrics_random.pdf",width=8,height=6,dpi=100)

######################################################################################

library(poweRlaw)
par(mfrow=c(2,2))
cat("Estimate power-law...\n")
data <- data.1$conductance
data <- data[data > 0]
data.pl <- conpl$new(data)
estimate_pars(data.pl)
(est.pl <- estimate_xmin(data.pl))
data.pl$setXmin(est.pl)
plot(data.pl,xlab="Circle",ylab="CDF",cex=2,cex.lab=1.3, cex.axis=1.3,main="Power-law")
lines(data.pl, col = 2)

cat("Estimate log-normal...\n")
data.lnormal <- conlnorm$new(data)
estimate_pars(data.lnormal)
(est.lnormal = estimate_xmin(data.lnormal))
data.lnormal$setXmin(est.lnormal)
plot(data.lnormal,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Log-normal")
lines(data.lnormal, col = 2)

cat("Estimate exponential...\n")
data.exp = conexp$new(data)
estimate_pars(data.exp)
(est.exp = estimate_xmin(data.exp))
data.exp$setXmin(est.exp)
plot(data.exp,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Exponential")
lines(data.exp, col = 2)
plot(c(1))