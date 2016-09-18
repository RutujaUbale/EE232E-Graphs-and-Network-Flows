library(igraph)

iteration = 10
num = 1000
d = numeric()
count = numeric()
deg1 = numeric()
deg2 = numeric()

for (i in 1:iteration) {
  g = forest.fire.game(num, fw.prob = 0.37, bw.factor = 0.32/0.37, directed = TRUE)
  d = c(d, diameter(g))
}
mean(d)
