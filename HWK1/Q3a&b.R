#create random graph by stimulating evolution
#a) each time a new vertex is added it creates a number of links to old vertices and the probability that an
#old vertex is cited depends on its in-degree (preferential attachment) and age. Produce such an undirected 
#network with 1000 nodes. Plot the degree distribution.
#if(!require(installr)) {
#install.packages("installr"); require(installr)}

#Q3a
install.packages("igraph")
library("igraph")
graphQ3a1<-aging.prefatt.game(1000,directed=FALSE,pa.exp=1,aging.exp=0,aging.bin=1000)
distributionQ3a1<-degree_distribution(graphQ3a1,cumulative=FALSE)
plot(graphQ3a1, vertex.size=1, vertex.label=NA, edge.arrow.size=0 )
plot(distributionQ3a1,main="Degree Distribution",xlab="Degree",ylab="Density")
lines(distributionQ3a1)
graphQ3a2<-aging.prefatt.game(1000,directed=FALSE,pa.exp=1,aging.exp=-4,aging.bin=1000)
distributionQ3a2<-degree_distribution(graphQ3a2,cumulative=FALSE)
plot(graphQ3a2, vertex.size=1, vertex.label=NA, edge.arrow.size=0 )
plot(distributionQ3a2,main="Degree Distribution",xlab="Degree",ylab="Density")
lines(distributionQ3a2)

#Q3
community<-fastgreedy.community(graphQ3a1)
modularity(community)
communitysize <- as.vector(sizes(community))
plot(communitysize,main="Community Sizes",xlab="Index",ylab="Community Size")
lines(communitysize)