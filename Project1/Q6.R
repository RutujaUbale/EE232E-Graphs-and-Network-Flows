library('igraph')

filePath = "/Users/rutuja/Documents/Graphs and Network Flows/Project1/facebook_combined.txt"
g = read.graph(file = filePath,directed=FALSE)
core_index = numeric(0)
core_degree = numeric(0)
for(i in 1: length(degree(g))){
  if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}
sub_graph = induced.subgraph(g, c(1, neighbors(g,1)))
vertexvector = rep(3,vcount(sub_graph))
vertexvector[1]=5
vertexcolor = rep("orange",vcount(sub_graph))
vertexcolor[1] ="green"
plot.igraph(sub_graph,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor)
node_count= vcount(sub_graph)
edge_count = ecount(sub_graph)
V(g)$name = V(g)

max_avg_degree = numeric(0)
max_cluster_coeff = numeric(0)
max_density = numeric(0)
min_avg_degree = numeric(0)
min_cluster_coeff = numeric(0)
min_density = numeric(0)
max_index_avg_degree = numeric(0)
max_index_cluster_coeff = numeric(0)
max_index_density = numeric(0)
min_index_avg_degree = numeric(0)
min_index_cluster_coeff = numeric(0)
min_index_density = numeric(0)

for(i in 1:length(core_index))
{
  core_neighbors =neighbors(g,core_index[i])
  core_personal = induced.subgraph(g,c(core_index[i],core_neighbors))
  core_community = walktrap.community(core_personal)
  comNodes_greaterthan10 = numeric(0)
  for(j in 1:length(core_community))
  {
    community_number=V(core_personal)[which(core_community$membership==j)]
    if(length(community_number)>10)
    {
      comNodes_greaterthan10 = c(comNodes_greaterthan10,j)
    }
  }
  average_degree = numeric(0)
  global_cluster_coeff = numeric(0)
  density = numeric(0)
  for(k in 1:length(comNodes_greaterthan10))
  {
    community_graph = induced.subgraph(core_personal,V(core_personal)[which(core_community$membership==comNodes_greaterthan10[k])])
    average_degree = c(average_degree,mean(degree(community_graph))/vcount(community_graph))
    global_cluster_coeff = c(global_cluster_coeff,transitivity(community_graph,type="global"))
    density = c(density,graph.density(community_graph))
  }
  max_index_avg_degree=c(max_index_avg_degree,comNodes_greaterthan10[which.max(average_degree)])
  max_index_cluster_coeff = c(max_index_cluster_coeff,comNodes_greaterthan10[which.max(global_cluster_coeff)])
  max_index_density=c(max_index_density,comNodes_greaterthan10[which.max(density)])
  min_index_avg_degree=c(min_index_avg_degree,comNodes_greaterthan10[which.min(average_degree)])
  min_index_cluster_coeff = c(min_index_cluster_coeff,comNodes_greaterthan10[which.min(global_cluster_coeff)])
  min_index_density=c(min_index_density,comNodes_greaterthan10[which.min(density)])
  cat("max ")
  cat("average_degree",comNodes_greaterthan10[which.max(average_degree)]," ")
  cat("cluster_coefficient",comNodes_greaterthan10[which.max(global_cluster_coeff)]," ")
  cat("density",comNodes_greaterthan10[which.max(density)],"\n")
  cat("min ")
  cat("average_degree",comNodes_greaterthan10[which.min(average_degree)]," ")
  cat("cluster_coefficient",comNodes_greaterthan10[which.min(global_cluster_coeff)]," ")
  cat("density",comNodes_greaterthan10[which.min(density)],"\n")
  max_avg_degree = c(max_avg_degree,max(average_degree))
  max_cluster_coeff = c(max_cluster_coeff,max(global_cluster_coeff))
  max_density = c(max_density,max(density))
  min_avg_degree = c(min_avg_degree,min(average_degree))
  min_cluster_coeff = c(min_cluster_coeff,min(global_cluster_coeff))
  min_density = c(min_density,min(density))
}
max_index_avg_degree = t(data.matrix(max_index_avg_degree))
max_index_cluster_coeff = t(data.matrix(max_index_cluster_coeff))
max_index_density = t(data.matrix(max_index_density))
min_index_avg_degree = t(data.matrix(min_index_avg_degree))
min_index_cluster_coeff = t(data.matrix(min_index_cluster_coeff))
min_index_density = t(data.matrix(min_index_density))
max_avg_degree = t(data.matrix(max_avg_degree))
max_cluster_coeff = t(data.matrix(max_cluster_coeff))
max_density = t(data.matrix(max_density))
min_avg_degree = t(data.matrix(min_avg_degree))
min_cluster_coeff = t(data.matrix(min_cluster_coeff))
min_density = t(data.matrix(min_density))


#Core nodes calculation
dg = degree(g)
nodes_list = dg>200
core_nodes =(which(nodes_list==TRUE))

#Rows - core_node, comm_no, size of community, density of community, modularity of community, transitivity
stat_table = matrix(0,0,7)

#For each core node - calculate personal networks
for(node in core_nodes){
  neighbor_list = c(node,neighbors(g,node))
  sub_graph = induced.subgraph(g,neighbor_list)
  
  fgc = fastgreedy.community(sub_graph,merges=TRUE,modularity = TRUE, membership = TRUE)
  
  for(idx in 1:(length(unique(fgc$membership)))){
    members = which(fgc$membership==idx)
    
    member_no = length(members)
    comm_graph = induced.subgraph(sub_graph,members)
    if(member_no>10){
      stat_list = c(node,idx,member_no,graph.density(comm_graph),max(fastgreedy.community(comm_graph)$modularity), transitivity(comm_graph), mean(degree(comm_graph))/vcount(comm_graph))
      stat_table = rbind(stat_table,stat_list,deparse.level=0)
    }
  }  
}


#Round to the closest multiple of 25
stat_table[,3] = round(stat_table[,3]/25) * 25  

#Round to 1 decimal digit
stat_table[,4:7] = round(stat_table[,4:7],1)

print("No. of personal networks: ")
print(length(unique(stat_table[,1])))
print("Table of community size: ")
print(table(stat_table[,3]))
print("Table of community density: ")
print(table(stat_table[,4]))
print("Table of community modularity: ")
print(table(stat_table[,5]))
print("Table of community clustering coefficient: ")
print(table(stat_table[,6]))
print("Table of community average degree: ")
print(table(stat_table[,7]))