getlibrary(igraph)
require(netrw)
rm(list = setdiff(ls(), lsf.str()))

N=10000
graph<-barabasi.game(N,directed=F)
#plot(graph,main="barabasi.game Graph",vertex.size=2, vertex.label=NA, edge.arrow.size=1)


##let a random walker start from a randomly selected node. Measure and plot <s(t)> vs t and
##(sigma)^2(t) vs t.
##s(t) is the average distance from start traversed by random walker and t is the step taken from start
T=30
distfromstart<-matrix(0,N,T)
#matrix stores average distance. Value stored in the first cell is the average distance walked by the 
#thousand walkers (N=1000) at t=1 or the distance covered by the thousand walkers in t=1
walker.num=1000
deg_distribution_end_node<-c()
for(start_node in 1:length(V(graph))){
  if(degree(graph,start_node)==0)     next
  sim<- netrw(graph, walker.num,
              start.node=start_node,#sample(0:(vcount(graph)-1),walker.num,replace=TRUE),
              damping=1, weights=NULL, T, seed=NULL,
              output.walk.path=TRUE, output.walkers=0:(walker.num-1),
              output.visit.prob=TRUE, output.nodes=0:(vcount(graph)-1),
              walk.path.file="walk_path.txt", visit.prob.file="visit_prob.txt",
              local.pagerank=FALSE, teleport.prob=NULL)
#The probability, at any step, that the person will continue is a damping factor d. Various 
#studies have tested different damping factors, but it is generally assumed that the damping factor 
#will be set around 0.85.
  
  tmp3 <- shortest.paths(graph,v=start_node)
  
  for(t in 1:T){
    
    distfromstart[start_node,t]<- mean(tmp3[1,sim$walk.path[t,1:walker.num]])
  }
  deg_distribution_end_node<-c(deg_distribution_end_node,degree(graph,sim$walk.path[T,1:walker.num]))
}
par(mfrow=c(2,2)) 
Average_Distance_From_Start<-colMeans(distfromstart)
#plot(Average_Distance_From_Start, main = "Average Distance vs Steps", xlab = "Steps", ylab = "Average Distance from Start")
#lines(Average_Distance_From_Start)

Average_StandDev <- apply(distfromstart,2,sd)
#plot(Average_StandDev)
#lines(Average_StandDev)

diameter(graph)
#diameter is length of longest path

h <- hist(deg_distribution_end_node, breaks=seq(-0.5, by=1 , length.out=max(deg_distribution_end_node)+2))
pl <- data.frame(x=h$mids, y=h$density)
plot(pl ,xlab="Degree",ylab="Density",type="o")

plot(degree.distribution(graph))
lines(degree.distribution(graph))
