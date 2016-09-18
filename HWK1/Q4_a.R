library(igraph)

nodes = 1000
num_iterations = 500
deg1 = numeric()
deg2 = numeric()

for (i in 1:num_iterations){
  graph = forest.fire.game(nodes, fw.prob = 0.37, bw.factor = 0.32/0.37, directed = TRUE)
  deg1 = c(deg1, degree(graph, mode = "in"))
  deg2 = c(deg2, degree(graph, mode = "out"))
}

hist1 = hist(deg1, breaks = seq(-0.5, to = max(deg1) + 1, by = 1), freq = FALSE, main = "Histogram of in degree distribution", xlab = "Degree")
x1 = data.frame(x = hist1$mids, y = hist1$density)
plot(x1, type = "o", main = "In Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")
plot(x1, type = "o", log = "xy", main = "In Degree Distribution with 1000 nodes(log)", xlab = "Degree", ylab = "Density")

hist2 = hist(deg2, breaks = seq(-0.5, to = max(deg2) + 1, by = 1), freq = FALSE, main = "Histogram of out degree distribution", xlab = "Degree")
x2 = data.frame(x = hist2$mids, y = hist2$density)
plot(x2, type = "o", main = "Out Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")
plot(x2, type = "o", log = "xy", main = "Out Degree Distribution with 1000 nodes(log)", xlab = "Degree", ylab = "Density")

