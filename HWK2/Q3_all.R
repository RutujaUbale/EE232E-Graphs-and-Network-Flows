library(igraph)
require(netrw)
graph<-random.graph.game(1000,0.01,directed= FALSE)
T=1000
distance_from_start<-matrix(0,1000,T)
average_prob_visit_nodes <- matrix(0,1,1000)
walker.num=1
degree_dist_end_node<-integer(1000)
s=matrix(0,1,995)
n=c(1,1,1,1,1)
tele_vec=c(n,s)

for(start_node in 1:length(V(graph))){
  sim<- netrw(graph, walker.num,
              start.node=start_node,#sample(0:(vcount(graph)-1),walker.num,replace=TRUE),
              damping=0.85, weights=NULL, T, seed=NULL,
              output.walk.path=TRUE, output.walkers=0:(walker.num-1),
              output.visit.prob=TRUE, output.nodes=0:(vcount(graph)-1),
              walk.path.file="walk_path.txt", visit.prob.file="visit_prob.txt",
              local.pagerank=F, teleport.prob=matrix(1,1,1000))#tele_vec)
  average_prob_visit_nodes<-average_prob_visit_nodes+sim$ave.visit.prob
}

average_prob_visit_nodes<-average_prob_visit_nodes/1000
#par(mfrow=c(1,1)) 

degrees<-degree(graph)
plot(degrees,colMeans(average_prob_visit_nodes),ylab="Probability",main="Avg. Probability of visiting nodes vs Degree\n (undirected) with  teleport probabilites assigned to all 1000 nodes ")
#lines(colMeans(average_prob_visit_nodes))
abline(lm( colMeans(average_prob_visit_nodes) ~ degrees))
