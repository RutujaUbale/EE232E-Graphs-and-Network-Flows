library(igraph)
require(netrw)
rm(list = setdiff(ls(), lsf.str()))

graph <- random.graph.game(1000,0.01,directed= TRUE)
T <- 30
distance_from_start <- matrix(0,1000,T)
Page_Rank_Scores1 <- matrix(0,1,1000)
walker.num <- 1000
degdistance_end_node <- integer(1000)
plot( graph,main="Network",vertex.size=2, vertex.label=NA, edge.arrow.size=1 )


for(start_node in 1:length(V(graph))){
  sim<- netrw(graph, walker.num,
              start.node = start_node,
              damping=0.85, weights = NULL, T, seed = NULL,
              output.walk.path = TRUE, output.walkers = 0:(walker.num-1),
              output.visit.prob = TRUE, output.nodes = 0:(vcount(graph)-1),
              walk.path.file = "walk_path.txt", visit.prob.file = "visit_prob.txt",
              local.pagerank = FALSE, teleport.prob = t(matrix(page.rank(graph)$vector)))
  Page_Rank_Scores1 <- Page_Rank_Scores1+sim$ave.visit.prob
}

Page_Rank_Scores1 <- Page_Rank_Scores1/1000
#par(mfrow=c(2,1)) 
plot(Page_Rank_Scores1[1,], main = "Simulated Page Rank with random walk on Directed Graph", xlab = "Index",ylab="Page Rank Scores", col="blue" )
deg <- degree(graph)


Page_Rank_Scores2 <- matrix(0,1,1000)

for(start_node in 1:length(V(graph))){
  sim2<- netrw(graph, walker.num,
              start.node = start_node,
              damping = 0.85, weights = NULL, T, seed = NULL,
              output.walk.path = TRUE, output.walkers = 0:(walker.num-1),
              output.visit.prob = TRUE, output.nodes = 0:(vcount(graph)-1),
              walk.path.file = "walk_path.txt", visit.prob.file = "visit_prob.txt",
              local.pagerank = FALSE, teleport.prob = Page_Rank_Scores1)
  Page_Rank_Scores2 <- Page_Rank_Scores2 + sim2$ave.visit.prob
}

Page_Rank_Scores2 <- Page_Rank_Scores2/1000

plot(Page_Rank_Scores2[1,], main = "Personalized Page Rank with random walk on Directed Graph", xlab = "Index",ylab="Page Rank Scores", col="blue")

plot(Page_Rank_Scores1[1,], Page_Rank_Scores2[1,], main = "Page Rank 1 vs Page Rank 2", xlab = "Page Rank 1", ylab ="Page Rank 2")


