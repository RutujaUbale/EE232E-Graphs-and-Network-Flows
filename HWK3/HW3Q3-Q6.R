require(igraph)
require(netrw)

g=read.graph("C:\\Users\\Hp\\Dropbox\\Graphs and Network Flows\\HWK3\\sorted_directed_net.txt",format = c("ncol"),weights='yes',directed=TRUE)

##############Question 3 to 6 #####################

is.connected(g)
cl=clusters(g)
gcc=induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

##################################Question 3#################
combineweight<-function(x){
  if(length(x)!=1)
    return(sqrt(prod(x)))
  return(x)
}

gcc_undirected=as.undirected(gcc,mode=c("collapse"),edge.attr.comb=list(weight=function(x) sqrt(prod(x)),name="ignore"))
gcc_undirected=simplify(gcc,remove.multiple=TRUE,edge.attr.comb=list(weight=combineweight))

comstruct_fg=fastgreedy.community(as.undirected(gcc_undirected))
comstruct_fg_commsize=c()

for (i in 1:length(unique(comstruct_fg$membership))){
  communities_indexes= which(comstruct_fg$membership == i)
  comstruct_fg_commsize= c(comstruct_fg_commsize,length(communities_indexes))
}
max_size_fg_comm=which.max(comstruct_fg_commsize)
max_size_fg_comm_nodes=which(comstruct_fg$membership==max_size_fg_comm)
plot(comstruct_fg_commsize,main="Community Size Distribution (FastGreedy)",ylab="Size")
lines(comstruct_fg_commsize)

comstruct_lp=label.propagation.community(gcc_undirected)
comstruct_lp_commsize=c()
for (i in 1:length(unique(comstruct_lp$membership))){
  communities_indexes= which(comstruct_lp$membership == i)
  comstruct_lp_commsize= c(comstruct_lp_commsize,length(communities_indexes))
}
max_size_lp_comm=which.max(comstruct_lp_commsize)
max_size_lp_comm_nodes=which(comstruct_lp$membership==max_size_lp_comm)
plot(comstruct_lp_commsize,main="Community Size Distribution(Label Propagation)",ylab="Size")
lines(comstruct_lp_commsize)
############################Question 4##############################

largestcom_g=induced.subgraph(gcc_undirected,max_size_fg_comm_nodes)

comstruct_fg_new=fastgreedy.community(as.undirected(largestcom_g))
comstruct_fg_new_commsize=c()
for (i in 1:length(unique(comstruct_fg_new$membership))){
  communities_indexes= which(comstruct_fg_new$membership == i)
  comstruct_fg_new_commsize= c(comstruct_fg_new_commsize,length(communities_indexes))
}
max_size_fg_new_comm=which.max(comstruct_fg_new_commsize)
max_size_fg_new_comm_nodes=which(comstruct_fg_new$membership==max_size_fg_new_comm)
plot(comstruct_fg_new_commsize,main="Sub-community Size Distribution (FastGreedy)",ylab="Size")
lines(comstruct_fg_new_commsize)

##########################Question 5################################
for (j in 1:length(unique(comstruct_fg$membership))){
  comm_nodes=which(comstruct_fg$membership == j)
  largestcom_g=induced.subgraph(gcc_undirected,comm_nodes)
  comstruct_fg_new=fastgreedy.community(as.undirected(largestcom_g))
  comstruct_fg_new_commsize=c()
  for (i in 1:length(unique(comstruct_fg_new$membership))){
    communities_indexes= which(comstruct_fg_new$membership == i)
    comstruct_fg_new_commsize= c(comstruct_fg_new_commsize,length(communities_indexes))
  }
  #max_size_fg_new_comm=which.max(comstruct_fg_new_commsize)
  #max_size_fg_new_comm_nodes=which(comstruct_fg_new$membership==max_size_fg_new_comm)
  plot(comstruct_fg_new_commsize,main=sprintf("Sub-community %d Size Distribution (FastGreedy)",j),ylab="Size")
  lines(comstruct_fg_new_commsize)
}




####################### Question 6#####################################
walker.num=1
i1=1
node_index=c()
comstruct_larg100_indexes=gcc_undirected
T=1000#length(V(g))
membship_matrix=matrix(0,length(comstruct_larg100_indexes),length(V(g)))
membship_matrix_dupe=matrix(0,length(comstruct_larg100_indexes),length(V(g)))
com_mem_matrix=diag(x = 1, length(comstruct_larg100_indexes),length(comstruct_larg100_indexes))
threshold1=0.2
threshold2=0.45
for (start_node in 1:length(V(g))){
  print(start_node)
  personalised_pagerank_dist=vector(length = length(V(g)))
  personalised_pagerank_dist[start_node]=1
  
  sim<- netrw(g, walker.num,
              start.node=start_node,damping=0.85, weights=E(g)$weights, T, seed=NULL,
              output.walk.path=TRUE, output.walkers=0:(walker.num-1),
              output.visit.prob=TRUE, output.nodes=0:(vcount(g)-1),
              walk.path.file="walk_path.txt", visit.prob.file="visit_prob.txt",
              local.pagerank=FALSE, teleport.prob=personalised_pagerank_dist)
  #  sim$ave.visit.prob[which(sim$ave.visit.prob<threshold)] <- 0
  for (mem_nodes in 1:length(V(g))){
    for (which_comm in 1:length(comstruct_larg100_indexes)){
      if (is.element(mem_nodes, which(comstruct_fg$membership==which_comm))) break
    }
    membship_matrix[,start_node]=membship_matrix[,start_node]+sim$ave.visit.prob[mem_nodes]*com_mem_matrix[,which_comm]
    
  }
  if (sum(membship_matrix[,start_node]) !=0) membship_matrix[,start_node]=membship_matrix[,start_node]/sum(membship_matrix[,start_node])
  
  membship_matrix_dupe[,start_node]= membship_matrix[,start_node]
  membship_matrix[which(membship_matrix[,start_node]<threshold1),start_node] = 0
  membship_matrix[which(membship_matrix[,start_node]>threshold2),start_node] = 0
  non_zero_indices= which(membship_matrix[,start_node]!=0)
  if(length(non_zero_indices)>1){
    node_index[i1]=start_node
    i1=i1+1
  }
  if (length(node_index)>8) break
}

