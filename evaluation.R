cat("Evaluation...\n")
library(igraph)
library(ggplot2)
setwd("~/code/R/googleplus")

network <- "google" 
cat("Network:",network)

cat("Loading graph...\n")
load(get.path(network,"graph.RDa"))

######################################################################################
# Load Ego-networks
######################################################################################
path <- "/var/mindstone/gplus/"
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
save(ego.networks,file="~/code/R/googleplus/egonetworks.RDa")

######################################################################################
# creation of ego-network graph
######################################################################################

edge.list <- data.frame(from=character(0),to=character(0),weight=numeric(0))
for(i in 2:length(ego.networks)){
  for(j in 2:i-1){
    #cat(names(ego.networks)[i],to=names(ego.networks)[j],length(intersect(ego.networks[i],ego.networks[j])),"\n")
    x <- length(intersect(ego.networks[i][[1]],ego.networks[j][[1]]))
    if(x > 0){
      cat(names(ego.networks)[i],to=names(ego.networks)[j],x,"\n")
      temp <- data.frame(from=names(ego.networks)[i],
                         to=names(ego.networks)[j],
                         weight=x)
      edge.list <- rbind(edge.list,temp)
    }
  }
}
save(edge.list,file="~/code/R/googleplus/edgelist-egonetworks.RDa")



edge.list <- data.frame(from=numeric(0),to=numeric(0),weight=numeric(0))
for(i in 2:length(ego.networks)){
  for(j in 2:i-1){
    #cat(names(ego.networks)[i],to=names(ego.networks)[j],length(intersect(ego.networks[i],ego.networks[j])),"\n")
    x <- length(intersect(ego.networks[i][[1]],ego.networks[j][[1]]))
    if(x > 0){
      cat(names(ego.networks)[i],to=names(ego.networks)[j],x,"\n")
      temp <- data.frame(from=i,
                         to=j,
                         weight=x)
      edge.list <- rbind(edge.list,temp)
    }
  }
}
plot.edge.list <- data.frame(x=1:length(edge.list[,3]),y=edge.list[,3])

p <- ggplot(plot.edge.list, aes(x, y))
p + geom_point()

######################################################################################
# vertices in ego-networks
######################################################################################
vertex.in.network <- c()
for(vertex in V(graph)){
  count <- 0
  for(i in 1:length(ego.networks)){
    if(any(vertex == ego.networks[i][[1]])){
      count <- count + 1
    }
  }
  cat(vertex,"->",count,"\n")
  vertex.in.network <- c(vertex.in.network,count)
}
save(vertex.in.network,file="~/code/R/googleplus/vertexinnetwork.RDa")
load("~/code/R/googleplus/google/vertexinnetwork.RDa")


df.vin <- data.frame(v=1:length(V(graph)),x=vertex.in.network)

qplot(df.vin$x, binwidth=.5)

vertexgreater1 <- vertex.in.network[which(vertex.in.network > 1)]

count <- sapply(seq(1,94), function(x){length(which(vertex.in.network == x))} )

df.vin <- data.frame(v=1:94,x=count)
df.vin <- subset(df.vin,subset=c(x > 0))

#breaks=c(0,1,10,200,3000,60000)
require(scales)
p <- ggplot(df.vin, aes(v, x)) +
  geom_point() +
  scale_y_continuous(trans=log_trans(),breaks=c(1,10,200,3000,60000)) +
  xlab("Vertex (Rank)") +
  ylab("Absolute Frequency (#)")+
  theme_bw()

ggsave(filename="plots/vertexegonetwork.pdf", width=10,height=6,dpi=100)

