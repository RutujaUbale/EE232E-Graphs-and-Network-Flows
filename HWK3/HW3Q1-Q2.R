require(igraph)
require(netrw)

g=read.graph("C:\\Users\\Hp\\Dropbox\\Graphs and Network Flows\\HWK3\\sorted_directed_net.txt",format = c("ncol"),weights='yes',directed=TRUE)

##############Question 1 and 2 #####################

is.connected(g)
cl=clusters(g)
gcc=induced.subgraph(g, which(cl$membership == which.max(cl$csize)))
edge_count<-ecount(gcc)
vertex_count<-vcount(gcc)
#plot( gcc, vertex.size=1, vertex.label=NA, edge.arrow.size=0 )

#par(mfrow=c(1,2))
in_deg_dist=degree.distribution(gcc,mode='in')
h <- hist(degree(gcc,mode='in'), breaks=seq(-0.5, by=1 , length.out=max(degree(gcc,mode='in'))+2),main="Histogram of In degree distribution")

plot(in_deg_dist,main="In degree distribution",xlab="degree",ylab="density")
lines(in_deg_dist)
out_deg_dist=degree.distribution(gcc,mode='out')
h <- hist(degree(gcc,mode='out'), breaks=seq(-0.5, by=1 , length.out=max(degree(gcc,mode='out'))+2),main="Histogram of out degree distribution")

plot(out_deg_dist,main="Out degree distribution",xlab="degree",ylab="density")
lines(out_deg_dist)
