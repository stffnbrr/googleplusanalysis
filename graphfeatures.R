rm(list = ls())
library(igraph)
require(ggplot2)
library(poweRlaw)
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

asdf <- show.degree.distribution(graph,"in")


old.degree.distribution <- function(){

dd.out <- degree.distribution(graph,mode='out')
dd.in <- degree.distribution(graph,mode='in')

b.in <- as.vector(degree(graph,mode='in'))
b.in <- b.in[b.in > 0]
avg.degree.in <- mean(b.in)
#a.in <- plfit(b.in)
m_m = dislnorm$new(b.in)

estimate_pars(m_m)
(est = estimate_xmin(m_m))
m_m$setXmin(est)
## Plot the data (from xmin)
plot(m_m,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3)
## Add in the fitted distribution
lines(m_m, col = 2,lwd=3)

a <- plot(m_m)
b <- lines(m_m)
ggplot(a, aes(x,y)) + 
  geom_point() +  
  coord_trans(x="log2",y="log2") + 
  geom_line(data=b,color="red") +
  scale_x_continuous(breaks=c(2000,4000,12000))+
  scale_y_continuous(breaks=c(0.25,0.5,1))+
  xlab("Vertices (Rank)") +
  ylab("Degree (#)") +
  theme_bw() +
  theme(legend.title=element_blank())



b.out <- as.vector(degree(graph,mode='out'))
b.out <- b.out[b.out > 0]
avg.degree.out <- mean(b.out)

#a.out <- plfit(b.out)
m_m1 = displ$new(b.out)
estimate_pars(m_m1)
(est1 = estimate_xmin(m_m1))
m_m1$setXmin(est1)

m_m2 = dislnorm$new(b.out)
estimate_pars(m_m2)
(est2 = estimate_xmin(m_m2))
m_m2$setXmin(est2)


m_m3 = dispois$new(b.out)
estimate_pars(m_m3)
(est3 = estimate_xmin(m_m3))
m_m3$setXmin(est3)

m_m4 = disexp$new(b.out)
estimate_pars(m_m4)
(est4 = estimate_xmin(m_m4))
m_m4$setXmin(est4)

## Plot the data (from xmin)
par(mfrow=c(2,2))
plot(m_m1,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Power-law")
lines(m_m1, col = 2)
plot(m_m2,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Log-normal")
lines(m_m2, col = 2)
plot(m_m3,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Poisson")
lines(m_m3, col = 2)
plot(m_m4,xlab="Vertices",ylab="Degree",cex=2,cex.lab=1.3, cex.axis=1.3,main="Exponential")
lines(m_m4, col = 2)

b.all <- as.vector(degree(graph,mode='all'))
b.all <- b.all[b.all > 0]
avg.degree.all <- mean(b.all)
#a.out <- plfit(b.out)
m_m = dislnorm$new(b.all)

estimate_pars(m_m)
(est = estimate_xmin(m_m))
m_m$setXmin(est)
## Plot the data (from xmin)
plot(m_m)
## Add in the fitted distribution
lines(m_m, col = 2)

degree.d2 <- data.frame(mode=as.factor(c(rep('Out Degree',length(b.out)),
                                         rep('In Degree',length(b.in)))),
                        vertex=c(1:length(b.out),1:length(b.in)),
                        degree=c(sort(b.out,decreasing=T),sort(b.in,decreasing=T)))

degree.d <- data.frame(mode=as.factor(c(rep('Out Degree',length(dd.out)-1),rep('In Degree',length(dd.in)-1))),vertex=c(2:length(dd.out),2:length(dd.in)),degree=c(dd.out[-1],dd.in[-1]))
require(scales)
ggplot(degree.d,aes(x=vertex,y=degree)) +
  geom_point(aes(group = mode,colour = mode)) +
  coord_trans(x="log")+
  scale_y_continuous(trans=log_trans()) +
  xlab("Vertices") +
  ylab("Degree") + 
  theme(legend.title=element_blank())  

ggsave(filename="degree.pdf", width=10,height=6,dpi=100)


require(plyr)
de.d <- ddply(degree.d,.(mode),transform, ecd = ecdf(degree)(degree))

ggplot(de.d,aes(x = degree, y = ecd)) + 
  geom_line(aes(group = mode,colour = mode)) +
  xlab("Degree") +
  ylab("CDF") +
  scale_x_continuous(trans=log_trans()) +
  theme(legend.title=element_blank())
}

######################################################################################
# Compute reciprocity of whole graph
######################################################################################
reciprocity <- sapply(V(graph),relation.reciprocity, graph=graph)

reci.dd.graph <- data.frame(vertex=1:length(reciprocity),reci=reciprocity)
save(reci.dd.graph,file="graphreciprocity.RDa")

#ggplot(reci.dd.graph,aes(x=reci)) + 
#  stat_ecdf() + 
#  xlab("Reciprocity") +
#  ylab("CDF")

#ggsave(filename="reciprocity.pdf", width=8,height=6,dpi=100)

