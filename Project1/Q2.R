library("igraph")

# read the Facebook graph edgelist file
g <- read.graph('/Users/rutuja/Documents/Graphs and Network Flows/Project1/facebook_combined.txt', format="ncol", directed=FALSE)

# create a sub graph with 1-neighborhood of node 1
personal_network = induced.subgraph(g, c(1, neighbors(g,1)))
vertexvector = rep(3,vcount(personal_network))
vertexvector[1]=5
vertexcolor = rep("green",vcount(personal_network))
vertexcolor[1] ="red"
plot.igraph(personal_network,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor)

# number of nodes
node_count = vcount(personal_network)
node_count

# number of edges
edge_count = ecount(personal_network)
edge_count
