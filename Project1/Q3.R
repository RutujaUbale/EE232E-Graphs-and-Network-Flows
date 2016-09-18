library(igraph)

g <- read.graph('/Users/rutuja/Documents/Graphs and Network Flows/Project1/facebook_combined.txt', format="ncol", directed=FALSE)

personal_network = induced.subgraph(g, c(1, neighbors(g,1)))
# community structure of the personal network
vertexvector = rep(3,vcount(personal_network))
vertexvector[1]=5
vertexcolor = rep("orange",vcount(personal_network))
vertexcolor[1] ="green"
plot.igraph(personal_network,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor, main="Plot of Personal Network")

core_index = numeric(0)
core_degree = numeric(0)
for(i in 1: length(degree(g))){
  if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}
length(core_index)     #Number of core nodes in the network
core_average_degree = mean(core_degree)
core_average_degree    #Average degree of core nodes

# using fastgreedy.community
community_fg = fastgreedy.community(personal_network)
colorVector = community_fg$membership+1
plot(personal_network,vertex.color=colorVector,vertex.label=NA,vertex.size=3, main = "Network Plot using Fast-Greedy Algorithm")

# using edge.betweenness.community
community_eb = edge.betweenness.community(personal_network)
colorVector = community_eb$membership+1
plot(personal_network,vertex.color=colorVector,vertex.label=NA,vertex.size=3, main = "Network Plot using Edge-Betweenness Algorithm")

# using infomap.community
community_im = infomap.community(personal_network)
colorVector = community_im$membership+1
plot(personal_network,vertex.color=colorVector,vertex.label=NA,vertex.size=3, main = "Network Plot using Infomap Algorithm")

