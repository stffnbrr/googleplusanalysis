load("livejournal/graph.RDa")
load("livejournal/groupmetrics.RDa")


data.1 <- data
data.1$conductance <- 1- data.1$conductance
par(mfrow=c(2,2))
plot(data.1[order(data.1$avgdegree,decreasing=T),]$avgdegree,ylab="Average Degree",xlab="Ranked Circles",main="Average Degree")
plot(data.1[order(data.1$cutratio,decreasing=T),]$cutratio,ylab="Cut Ratio",xlab="Ranked Circles",main="Cut Ratio")
plot(data.1[order(data.1$conductance,decreasing=T),]$conductance,ylab="Conductance",xlab="Ranked Circles",main="Conductance")
plot(data.1[order(data.1$modularity,decreasing=T),]$modularity,ylab="Modulartiy",xlab="Ranked Circles",main="Modularity")



load("/nfs/projects/mindstone/livejournal/groupmetrics.RDa")
data.2 <- data

load("~/code/R/googleplus/google/data/groupmetrics.RDa")
data.1 <- data

#data.1$conductance <- 1- data.1$conductance 
#data.2$conductance <- 1- data.2$conductance
par(mfrow=c(2,2))
plot(data.1[order(data.1$avgdegree,decreasing=T),]$avgdegree,ylab="Average Degree",xlab="Ranked Circles",main="Average Degree",ylim=c(0,350))
lines(data.2[order(data.2$avgdegree,decreasing=T),]$avgdegree,type="p",pch=23,col="red")
#legend(300,300,  c("LiveJournal","Google+"))

plot(data.1[order(data.1$cutratio,decreasing=T),]$cutratio,ylab="Ratio Cut",xlab="Ranked Circles",main="Ratio Cut")
lines(data.2[order(data.2$cutratio,decreasing=T),]$cutratio,type="p",pch=23,col="red")

plot(data.1[order(data.1$conductance,decreasing=T),]$conductance,ylab="Conductance",xlab="Ranked Circles",main="Conductance",ylim=c(0,1))
lines(data.2[order(data.2$conductance,decreasing=T),]$conductance,type="p",pch=23,col="red")

plot(data.1[order(data.1$modularity,decreasing=T) , ]$modularity,ylab="Modulartiy",xlab="Ranked Circles",main="Modularity",ylim=c(0,22000))
lines(data.2[order(data.2$modularity,decreasing=T) , ]$modularity,,type="p",pch=23,col="red")

