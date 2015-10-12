rm(list = ls())

path <- "orkut/"

temp.file <- file(paste(path,"com-orkut.ungraph.txt",sep=""), 'r') 
input <- readLines(temp.file, n=-1)
print(head(input))
edge.list.raw <- input[5:length(input)]
rm(input)

a <- lapply(edge.list.raw,strsplit,split="\t")
rm(edge.list.raw)
b <- sapply(a,function(x)x[[1]])
edge.list <- t(b)
rm(a)
rm(b)

library(igraph)
graph <- graph.data.frame(edge.list)
rm(edge.list)
save(graph,file="orkut/graph.RDa")

load("orkut/graph.RDa")

###############################################################################
# Graph features
###############################################################################

# Nodes  3.997.962
# Edges	34.681.189
print(graph)


###############################################################################
# Load communities
###############################################################################
temp.file <- file(paste(path,"com-orkut.all.cmty.txt",sep=""), 'r') 
input <- readLines(temp.file, n=-1)

communities <- list() 
for (i in seq_along(input)){ 
  test <- strsplit(input[i],split="\t")[[1]]
  communities[[test[1]]] <- test[-1]
}
head(communities)
save(communities,file="orkut/allcommunities.RDa")
load("orkut/communities.RDa")

#V(graph)[name %in% communities[[1]]]

rm(list = ls())
library(igraph)
######################################################################################
# Internal Connectivity / Average Degree
# f(C) = 2m_C/n_C
# n_C = # vertices in circle
# m_C = # edges in circle
######################################################################################
average.degree <- function(sub.graph){
  (2*ecount(sub.graph))/vcount(sub.graph)
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
load("/nfs/projects/mindstone/orkut/graph.RDa")
load("/nfs/projects/mindstone/orkut/communities.RDa")

library(foreach)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)

g1 <- degree.sequence.game(degree(graph,mode='out'),degree(graph,mode='in'))
V(g1)$name <- V(graph)$name

data <- foreach(community = communities,.export=c("graph","g1"),.combine=rbind)%dopar%{
#data <- data.frame(circle=character(0),avgdegree=numeric(0),cutratio=numeric(0),conductance=numeric(0))
#for(community in communities){  
  require(igraph)
  cC <- length(E(graph)[xor(from(community),to(community))])
  sub.graph <- induced.subgraph(graph,V(graph)[community])  
  g1.sub <-  induced.subgraph(g1,V(g1)[community])  
  ad <- average.degree(sub.graph)
  cr <- cut.ratio(graph,community,cC)
  cd <- conductance(sub.graph,cC)
  
  m <- modu(sub.graph,g1.sub)
  data.frame(avgdegree=ad,
             cutratio=cr,
             conductance=cd,
             modularity=m)
  
  #print(a)
  #data <- rbind(data,a)
}
data <- data[complete.cases(data),]

save(data,file="orkutgroupmetrics.RDa")
stopCluster(cl)

