rm(list = ls())
library(igraph)
require(ggplot2)
setwd("~/code/R/googleplus")
source("util.R")

# Select network:
# google
# facebook
# twitter
network <- "facebook" 
cat("Network:",network)

cat("Loading graph...\n")
load(get.path(network,"graph.RDa"))


######################################################################################
# Clustering Coefficient
######################################################################################
#transitivity(graph,type="average",isolates="NaN")
#transitivity(graph,type="localaverage")
#transitivity(graph,type="global")

cat("Transitivity...\n")
cc.local <- transitivity(graph,type="local")
ec <- ecdf(cc.local)

local.data <- data.frame(network=rep("Vertex",length(cc.local)),
value=cc.local)

require(plyr)
mydf_m <- ddply(local.data,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,linetype = network)) +
  xlab("Clustering Coefficient (#)") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")


par(mfrow=c(1,1))
par(mar=c(5,5,4,2)+0.1)
plot(ec,ylab="CDF",xlab="Clustering Coefficient",main="",cex=2,cex.lab=1.3, cex.axis=1.3,lwd=3)