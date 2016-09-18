library(igraph)
require(netrw)
rm(list = setdiff(ls(), lsf.str()))

N=100

graph<-random.graph.game(N,0.01,directed=F)
#plot( graph,main="Network p=0.01",vertex.size=2, vertex.label=NA, edge.arrow.size=1 )

step_index=10
distance_from_start<-matrix(0,N,T)
walker.num=100
degree_dist_end_node<-c()
for(start_node in 1:length(V(graph))){
  if(degree(graph,start_node)==0)     next
  sim<- netrw(graph, walker.num,
              start.node=start_node,#sample(0:(vcount(graph)-1),walker.num,replace=TRUE),
              damping=1, weights=NULL, step_index, seed=NULL,
              output.walk.path=TRUE, output.walkers=0:(walker.num-1),
              output.visit.prob=TRUE, output.nodes=0:(vcount(graph)-1),
              walk.path.file="walk_path.txt", visit.prob.file="visit_prob.txt",
              local.pagerank=FALSE, teleport.prob=NULL)
  
  tmp3 <- shortest.paths(graph,v=start_node)
  
  for(t in 1:step_index){
    
    distance_from_start[start_node,t]<- mean(tmp3[1,sim$walk.path[t,1:walker.num]])
  }
  degree_dist_end_node[start_node]<-c(degree_dist_end_node,degree(graph,sim$walk.path[T,1:walker.num]))
}
#par(mfrow=c(2,2)) 
average_distance_from_start<-colMeans(distance_from_start^2)
plot(average_distance_from_start,xlab="step index",ylab="Avg.distance from start", main="Avg distance from start vs step N = 100")
lines(average_distance_from_start)

average_sd <- apply(distance_from_start,2,sd)
plot(average_sd,ylab="Avg.std dev from start",xlab="step index",main="Avg.std dev from start vs step index N = 100")
lines(average_sd)

dia<-diameter(graph)

h <- hist(degree_dist_end_node, breaks=seq(-0.5, by=1 , length.out=max(degree_dist_end_node)+2))
pl <- data.frame(x=h$mids, y=h$density)
plot(pl ,xlab="Degree",ylab="Density",type="o",main="Degree Distribtuion of ending node")
plot(degree.distribution(graph),ylab="Density", xlab= "Degree",main="Degree Distribution of Graph ")
lines(degree.distribution(graph),ylab="Density", xlab= "Degree",main="Degree Distribution of Graph")
degrees<-degree(graph)


