library(igraph)

nodes = 1000
num_iterations = 100
pc = numeric()

for (i in 1:num_iterations) {
  for (p in seq(from = 0, to = 1.00, by = 1e-04)) {
    g = random.graph.game(nodes, p, directed = FALSE)
    if (is.connected(g)) 
      break
  }
  pc = c(pc, p)
}

mean(pc)