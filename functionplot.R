rm(list = ls())
library(igraph)
require(ggplot2)
setwd("~/code/R/googleplus")
source("util.R")

# Select network:
# google
# facebook
# twitter
load(get.path("facebook","groupmetrics.RDa"))
facebook <- data

load(get.path("google","groupmetrics.RDa"))
google <- data

load(get.path("twitter","groupmetrics.RDa"))
twitter <- data

load("livejournalgroupmetrics.RDa")
livejournal <- data

load("orkutgroupmetrics.RDa")
orkut <- data

load("avgdegree.RDa")
load("conductance.RDa")
load("modularity.RDa")
load("ratiocut.RDa")


graphs <- list("Google"=google,
               "Twitter"=twitter,
               "LiveJournal"=livejournal,
           #    "Facebook"=facebook,
               "Orkut"=orkut)
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
  scale_linetype_manual(breaks=c("Google","LiveJournal","Orkut","Twitter"), values=c(5,3,6,1))+
  xlab("Average Degree") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position=c(0.7,0.4),legend.key.width=unit(1,"in"))
# legend.position = "none"


ratioc <- create.ggplot.data.frame(graphs,"cutratio")
mydf_m <- ddply(ratioc,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network, linetype = network),size=2) +
  scale_linetype_manual(breaks=c("Google","LiveJournal","Orkut","Twitter"), values=c(5,3,6,1))+
  xlab("Ratio Cut") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

cond <- create.ggplot.data.frame(graphs,"conductance")
mydf_m <- ddply(cond,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network, linetype = network),size=2) +
  scale_linetype_manual(breaks=c("Google","LiveJournal","Orkut","Twitter"), values=c(5,3,6,1))+
  xlab("Conductance") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")

mod <- create.ggplot.data.frame(graphs,"modularity")
mydf_m <- ddply(mod,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,color = network,linetype = network),size=2) +
  scale_linetype_manual(breaks=c("Google","LiveJournal","Orkut","Twitter"), values=c(5,3,6,1))+
  xlab("Modularity") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position = "none")



avgdegree <- data.frame(network=as.factor(c(
  rep("Facebook",length(facebook[,1])),
  rep("Google",length(google[,1])),
  rep("Twitter",length(twitter[,1])),
  rep("Livejournal",length(livejournal[,1])),
  rep("Orkut",length(orkut[,1]))
  )),
  rank= c(1:length(facebook[,1]),
          1:length(google[,1]),
          1:length(twitter[,1]),
          1:length(livejournal[,1]),
          1:length(orkut[,1])),
  value=c(facebook[order(facebook$avgdegree,decreasing=T),]$avgdegree, 
          google[order(google$avgdegree,decreasing=T),]$avgdegree, 
          twitter[order(twitter$avgdegree,decreasing=T),]$avgdegree, 
          livejournal[order(livejournal$avgdegree,decreasing=T),]$avgdegree,
          orkut[order(orkut$avgdegree,decreasing=T),]$avgdegree))

require(plyr)
mydf_m <- ddply(avgdegree,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,linetype = network)) +
  xlab("Average Degree") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank())


ratiocut <- data.frame(network=as.factor(c(
  rep("facebook",length(facebook[,1])),
  rep("google",length(google[,1])),
  rep("twitter",length(twitter[,1])),
  rep("livejournal",length(livejournal[,1]))
)),
rank= c(1:length(facebook[,1]),
        1:length(google[,1]),
        1:length(twitter[,1]),
        1:length(livejournal[,1])),
value=c(facebook[order(facebook$cutratio,decreasing=T),]$cutratio, 
        google[order(google$cutratio,decreasing=T),]$cutratio, 
        twitter[order(twitter$cutratio,decreasing=T),]$cutratio, 
        livejournal[order(livejournal$cutratio,decreasing=T),]$cutratio))

require(plyr)
mydf_m <- ddply(ratiocut,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,colour = network))


conductance <- data.frame(network=as.factor(c(
  rep("facebook",length(facebook[,1])),
  rep("google",length(google[,1])),
  rep("twitter",length(twitter[,1])),
  rep("livejournal",length(livejournal[,1]))
)),
rank= c(1:length(facebook[,1]),
        1:length(google[,1]),
        1:length(twitter[,1]),
        1:length(livejournal[,1])),
value=c(facebook[order(facebook$conductance,decreasing=T),]$conductance, 
        google[order(google$conductance,decreasing=T),]$conductance, 
        twitter[order(twitter$conductance,decreasing=T),]$conductance, 
        livejournal[order(livejournal$conductance,decreasing=T),]$conductance))

require(plyr)
mydf_m <- ddply(conductance,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,colour = network))

modularity <- data.frame(network=as.factor(c(
  rep("facebook",length(facebook[,1])),
  rep("google",length(google[,1])),
  rep("twitter",length(twitter[,1])),
  rep("livejournal",length(livejournal[,1]))
)),
rank= c(1:length(facebook[,1]),
        1:length(google[,1]),
        1:length(twitter[,1]),
        1:length(livejournal[,1])),
value=c(facebook[order(facebook$modularity,decreasing=T),]$modularity, 
        google[order(google$modularity,decreasing=T),]$modularity, 
        twitter[order(twitter$modularity,decreasing=T),]$modularity, 
        livejournal[order(livejournal$modularity,decreasing=T),]$modularity))

require(plyr)
mydf_m <- ddply(modularity,.(network),transform, ecd = ecdf(value)(value))
ggplot(mydf_m,aes(x = value, y = ecd)) + 
  geom_line(aes(group = network,linetype = network)) +
  xlab("Reciprocity") +
  ylab("CDF") +
  theme_bw() +
  theme(legend.title=element_blank())


save(avgdegree,file="avgdegree.RDa")
save(conductance,file="conductance.RDa")
save(ratiocut,file="ratiocut.RDa")
save(modularity,file="modularity.RDa")



#plot(1:length(livejournal[,1]),livejournal[order(livejournal$avgdegree,decreasing=T),]$avgdegree)
#lines(1:length(google[,1]),google[order(google$avgdegree,decreasing=T),]$avgdegree)
#lines(facebook[order(facebook$avgdegree,decreasing=T),]$avgdegree)
#lines(twitter[order(twitter$avgdegree,decreasing=T),]$avgdegree)

#p <- ggplot(avgdegree, aes(network, value))
#p + scale_y_continuous(limits=c(0,200))
#p + geom_boxplot(outlier.size = 0)

#h <- ggplot(avgdegree, aes(fill=network, x=value)) + geom_histogram() + facet_wrap(~network)

#c <- ggplot(avgdegree,aes(x=value)) + stat_ecdf()


