rm(list = ls())
library(igraph)
library(poweRlaw)

load("livejournal/graph.RDa")

######################################################################################
# Degree Distribution
######################################################################################
show.degree.distribution <- function(graph,mode){
  cat("Evaluating degree distribution...\n")
  data <- as.vector(degree(graph,mode=mode))
  data <- data[data > 0]
  par(mfrow=c(2,2))
  cat("Estimate power-law...\n")
  data.pl <- displ$new(data)
  estimate_pars(data.pl)
  (est.pl <- estimate_xmin(data.pl))
  data.pl$setXmin(est.pl)
  plot(data.pl,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Power-law")
  lines(data.pl, col = 2)
  
  cat("Estimate log-normal...\n")
  data.lnormal <- dislnorm$new(data)
  estimate_pars(data.lnormal)
  (est.lnormal = estimate_xmin(data.lnormal))
  data.lnormal$setXmin(est.lnormal)
  plot(data.lnormal,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Log-normal")
  lines(data.lnormal, col = 2)
  
  cat("Estimate poisson...\n")
  data.pois <- dispois$new(data)
  estimate_pars(data.pois)
  (est.pois <- estimate_xmin(data.pois))
  data.pois$setXmin(est.pois)
  plot(data.pois,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Poisson")
  lines(data.pois, col = 2)
  
  cat("Estimate exponential...\n")
  data.exp = disexp$new(data)
  estimate_pars(data.exp)
  (est.exp = estimate_xmin(data.exp))
  data.exp$setXmin(est.exp)
  plot(data.exp,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Exponential")
  lines(data.exp, col = 2)
  
  
  result <- list()
  result["pl"] <- data.pl
  result["lnorm"] <- data.lnormal
  result["pois"] <- data.pois
  result["exp"] <- data.exp
  return(result)
} 

show.degree.distribution(graph,"all")


cc.local <- transitivity(graph,type="local")
ec <- ecdf(cc.local)


par(mfrow=c(1,1))
par(mar=c(5,5,4,2)+0.1)
plot(ec,ylab="CDF",xlab="Clustering Coefficient",main="",cex=2,cex.lab=1.3, cex.axis=1.3,lwd=3)


