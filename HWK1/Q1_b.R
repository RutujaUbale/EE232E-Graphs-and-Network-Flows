library(igraph)

nodes = 1000
num_iterations = 100

p1 = 0.01
cnt1 = numeric()
d1 = numeric()

p2 = 0.05
cnt2 = numeric()
d2 = numeric()

p3 = 0.1
cnt3 = numeric()
d3 = numeric()

for (i in 1:num_iterations) {
  g1 = random.graph.game(nodes, p1, directed = FALSE)
  cnt1 = c(cnt1, as.integer(is.connected(g1)))
  d1 = c(d1, diameter(g1))
  
  g2 = random.graph.game(nodes, p2, directed = FALSE)
  cnt2 = c(cnt2, as.integer(is.connected(g2)))
  d2 = c(d2, diameter(g2))
  
  g3 = random.graph.game(nodes, p3, directed = FALSE)
  cnt3 = c(cnt3, as.integer(is.connected(g3)))
  d3 = c(d3, diameter(g3))
}

is.connected(g1)
mean(cnt1)
mean(d1)

is.connected(g2)
mean(cnt2)
mean(d2)

is.connected(g3)
mean(cnt3)
mean(d3)