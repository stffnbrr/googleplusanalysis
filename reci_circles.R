rm(list = ls())


require('igraph')
require('ggplot2')
setwd("~/code/R/googleplus")

source("util.R")

cat("Loading graph...\n")
load("data/graph.RDa")
print(graph)

# Select network:
# google
# facebook
# twitter
network <- "google" 
cat("Network:",network)

cat("Loading graph...\n")
load(get.path(network,"graph.RDa"))



######################################################################################
# Reciprocity in Circles
######################################################################################
load(get.path(network,"circles.RDa"))
i <- 0
reci.dd <- data.frame(circle=numeric(0),reci=numeric(0))
for(circle in circles){
  i <- i + 1
  c.id <- names(circles)[i]
  cat(i,"/",length(circles), " ", c.id, "\n")
  #circle <- circles[[3]]
  sub.graph <- induced.subgraph(graph,V(graph)[circle])  
  reciprocity <- sapply(V(sub.graph),relation.reciprocity, graph=sub.graph)
  d <- data.frame(circle=rep(c.id,length(reciprocity)),reci=reciprocity)
  reci.dd <- rbind(reci.dd,d)
}
save(reci.dd,file=get.path(network,"cirlcereciprocity.RDa"))
### Reciprocity per Circles
reci.2 <- reci.dd[complete.cases(reci.dd), ]
result <- aggregate(reci.2,list(reci.2$circle),FUN="mean")
reci.mean <- data.frame(circle=result$Group.1,reci=result$reci)
ggplot(reci.mean,aes(x=reci)) + 
  stat_ecdf() +
  xlab("Reciprocity per Circle") +
  ylab("CDF")
ggsave(filename="plots/reciprocitypercircle.pdf", width=8,height=6,dpi=100)


######################################################################################
# Reciprocity from user to circle members
######################################################################################
user.circle <- read.table(get.path(network,
                                   "user_circle.csv"),
                                   header=F,
                                   sep=",",
                                   col.names=c("user","circle"),
                                   colClasses=c("character","character"))

i <- 0
reci.user.circle <- data.frame(circle=character(0),reci=numeric(0))
for(circle in user.circle$circle){
  i <- i + 1
  c.id <- names(circles)[i]
  cat(i,"/",length(circles), " ", c.id, "\n")
  #circle <- circles[[3]]
  reci <- relation.reciprocity(induced.subgraph(graph,V(graph)[circles[circle][[1]]]), V(graph)[name == user.circle[i,1]])  
  d <- data.frame(circle=circle,reci=reci)
  reci.user.circle <- rbind(reci.user.circle,d)
}
reci.user.circle <- reci.user.circle[complete.cases(reci.user.circle), ]

######################################################################################
# Reciprocity from user to ego-network
######################################################################################
path <- "/nfs/projects/mindstone/ego-gp/gplus/"
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

t1 <- proc.time()
temp <- list()
for(vertex in start.vertices){
  temp[[ vertex ]] <- c(which(V(graph)$name == vertex),neighbors(graph,V(graph)[vertex],mode="all"))
  cat(vertex,"\n")
}
t2 <- proc.time()
print(t2-t1)
print(length(V(graph)) == length(unique(unlist(temp))))
ego.networks <- temp

i <- 0
reci.ego.network <- data.frame(network=character(0),reci=numeric(0))
for(i in 1:length(ego.networks)){
  sub.graph <- induced.subgraph(graph,V(graph)[ego.networks[[i]]])
  reciprocity <- sapply(V(sub.graph),relation.reciprocity, graph=sub.graph)
  print(length(reciprocity))
  print(length(reciprocity[is.nan(reciprocity)]))
  reciprocity <- reciprocity[!is.nan(reciprocity)]
  d <- data.frame(network=rep(names(ego.networks)[i],length(reciprocity)),reci=reciprocity)
  reci.ego.network <- rbind(reci.ego.network,d)
}

reci.ego.network <- reci.ego.network[complete.cases(reci.ego.network), ]
reci.ego.network <- aggregate(reci.ego.network,list(reci.ego.network$circle),FUN="mean")
reci.ego.network <- data.frame(network=reci.ego.network$Group.1,reci=reci.ego.network$reci)
ggplot(reci.ego.network,aes(x=reci)) + 
  stat_ecdf() +
  xlab("Reciprocity in ego-networks") +
  ylab("CDF")
                          

######################################################################################
# Length of Circles
######################################################################################
x <- lapply(circles,length)
x2 <- x
names(x2) <- c()
x2 <- sapply(x2,function(x) x)
length.circles <- data.frame(circle=names(circles),length=x2)
length.circles <- length.circles[with(length.circles, order(length)), ]
ggplot(length.circles,aes(x=length)) + 
  geom_histogram(aes( fill= ..count..),binwidth=16) +
  xlab("# Members in Circles") +
  ylab("# Circles")  +
  scale_fill_continuous("")
ggsave(filename="plots/circleslength.pdf",width=8,height=6,dpi=100)



######################################################################################
# Plot CDF of reciprocity
######################################################################################
reci.dd.graph <- load("graphreciprocity.RDa")

reciprocity <- data.frame(
  type=as.factor(c(
    rep("Graph",length(reci.dd.graph[,1])),
    rep("Circle",length(reci.mean[,1])),
    #rep("User",length(reci.user.circle[,1])),
    rep("Ego",length(reci.ego.network[,1])))),
  reci=c(
    reci.dd.graph[,2],
    reci.mean[,2],
    #reci.user.circle[,2],
    reci.ego.network[,2]))

require(plyr)
mydf_m <- ddply(reciprocity,.(type),transform, ecd = ecdf(reci)(reci))

#save(mydf_m,file="data/reciprocitydataframe.RDa")

ggplot(mydf_m,aes(x = reci, y = ecd)) + 
  geom_line(aes(group = type,colour = type)) +
  xlab("Reciprocity") +
  ylab("CDF") +
  theme(legend.title=element_blank())
ggsave(filename="plots/reciprocity.pdf",width=8,height=6,dpi=100)


ggplot(mydf_m,aes(x = reci, y = ecd)) + 
  geom_line(aes(group = type,linetype= type)) +
  xlab("Reciprocity") +
  ylab("CDF") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(.85,.7), legend.title=element_text(""))
 
