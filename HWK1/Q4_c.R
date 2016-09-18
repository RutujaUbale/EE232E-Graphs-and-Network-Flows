library(igraph)

g <- forest.fire.game(1000, fw.prob=0.37, bw.factor=0.32/0.37)

clust <- clusters(g)
gccindex <- which.max((unlist(clust$csize)))
nongccs <- (1:vcount(g))[clust$membership!=gccindex]
gcc <- delete.vertices(g, nongccs)


com_struct <- edge.betweenness.community (gcc,directed = TRUE)

com_sizes <- as.vector(sizes(com_struct))
com_sizes
par(mfrow=c(1,2))
plot(com_sizes,main="Community Sizes",xlab="Index",ylab="Community Size")
lines(com_sizes)
cat("The modularity of graph is ",modularity(com_struct))
plot( g, vertex.size=1, vertex.label=NA, edge.arrow.size=0 )

