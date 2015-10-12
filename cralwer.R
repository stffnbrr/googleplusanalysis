rm(list = ls())

library(RCurl)
library(igraph)
library(gsubfn)
setwd("~/code/R/googleplus")

source("util.R")

cat("Lade Graph...\n")
load("graph.RDa")

out.going <- c("out","https://plus.google.com/_/socialgraph/lookup/visible/?o=%5Bnull%2Cnull%2C%22","%22%5D&rt=j")
in.coming <- c("in","https://plus.google.com/_/socialgraph/lookup/incoming/?o=%5Bnull%2Cnull%2C%22","%22%5D&n=1000&rt=j")

crawl.edges <- function(graph,vertices, type){
  writeLines(c(""), "log.txt")
  cat("Start crawling ", type[1], " edges...\n")
  edge.list <- foreach(vertex = vertices, .combine=rbind, .verbose=T, .packages = c('igraph','RCurl','gsubfn'), .export = c('graph','type'), .inorder=F)%dopar%{
    source("util.R")
    #sink("log.txt", append=T)
    cat(vertex,"\n")
    request <- paste0(type[2],V(graph)[vertex]$name,type[3])
    result <- tryCatch({
        response <- getURL(request)
        follow <- strapply(response,"(\\d{21})")[[1]]
        cat(vertex, ": response ", length(follow),"\n")
        if(type[1] == "in"){
          b2 <- create.matrix(V(graph)[vertex]$name,follow,out=FALSE)
        }else{
          b2 <- create.matrix(V(graph)[vertex]$name,follow)  
        }
        cat(vertex, ": ",length(b2[,1]),"\n")
        b2
      },error=function(cond) {
        return(NA)
      })
      result
  }
  edge.list
}

library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

print(length(V(graph)))
max <- 20
seq <- seq_along(V(graph))
chunks <- split(V(graph),ceiling(seq/max))
edge.list <- data.frame(from=character(0), to=character(0))
for(chunk in chunks){
  result <- crawl.edges(graph,chunk,out.going)
  edge.list <- rbind(edge.list,result)
  cat("Current length", length(edge.list[,1]))
}

stopCluster(cl)

#edge.list <- data.frame(from=character(0), to=character(0))
#for(vertex in V(graph)){
  #request <- paste("https://plus.google.com/u/0/_/socialgraph/lookup/incoming/?o=%5Bnull%2Cnull%2C%22",V(graph)[vertex]$name,"%22%5D&n=1000&rt=j",sep="")
#  request <- paste0("https://plus.google.com/_/socialgraph/lookup/visible/?o=%5Bnull%2Cnull%2C%22",V(graph)[vertex]$name,"%22%5D&rt=j")
  #print(request)
 # response <- getURL(request)
  #follow <- strapply(response,"(\\d{21})")[[1]]
  #b2 <- matrix(c(rep(V(graph)[vertex]$name,length(follow)),follow),ncol=2,nrow=length(follow))
  #edge.list <- rbind(edge.list,b2)
  #print(length(edge.list[,1]))
#}
