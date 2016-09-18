library(igraph)

g <- read.graph('/Users/rutuja/Documents/Graphs and Network Flows/Project1/facebook_combined.txt', format="ncol", directed=FALSE)

PN_core_removed = induced.subgraph(g,neighbors(g,1))
# community structure of the personal network with core nodes removed
vertexvector = rep(3,vcount(PN_core_removed))
vertexvector[1]=5
vertexcolor = rep("blue",vcount(PN_core_removed))
vertexcolor[1] ="yellow"
plot.igraph(PN_core_removed,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor, main="Plot of Personal Network with core nodes removed")

# community structure using fastgreedy.community
PN_core_removed_fg = fastgreedy.community(PN_core_removed)
colorVector = PN_core_removed_fg$membership+1
plot(PN_core_removed, vertex.color=colorVector, vertex.label=NA, vertex.size=3, main = "Network Plot using Fast-Greedy Algorithm")

# community structure using edge.betweenness.community
PN_core_removed_eb = edge.betweenness.community(PN_core_removed)
colorVector = PN_core_removed_eb$membership+1
plot(PN_core_removed, vertex.color=colorVector,vertex.label=NA, vertex.size=3, main = "Network Plot using Edge-Betweenness Algorithm")

# community structure using infomap.community
PN_core_removed_im = infomap.community(PN_core_removed)
colorVector = PN_core_removed_im$membership+1
plot(PN_core_removed, vertex.color=colorVector,vertex.label=NA, vertex.size=3, main = "Network Plot using Infomap Algorithm")

