graph_fb <- read.graph('C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\Project_1\\facebook_combined.txt', format = "edgelist")
corenodes <- which(neighborhood.size(graph_fb, 1, nodes=V(graph_fb)) > 200)

personal_network<-function(u, g)
{
  pnNodes <- neighborhood(g , 1 , nodes=u)[[1]]
  nonPNNodes <- which( !( (1:vcount(g)) %in% pnNodes)  )
  perNetw <- delete.vertices(g , nonPNNodes)
  perNetw$name =  sort(pnNodes)
  perNetw
}


comm_find <- function(u,v,g)
{
  neighborsU <- neighborhood(g,1,u)[[1]][-1]
  neighborsV <- neighborhood(g,1,v)[[1]][-1]
  intersect(neighborsU, neighborsV)
}

dispersion <- function(u,v,g) 
{
  disp <- 0
  commonUV <- comm_find(u, v, g)
  gNoUV <- delete.vertices(g, c(u, v))
  
  for(s in commonUV) 
  {
    for(t in commonUV) 
    {
      if(s != t) 
      {
        if( !is.na(match(s,V(gNoUV))) && !is.na(match(t,V(gNoUV))) && !are.connected(gNoUV,s,t) && length(commNeib_find(s,t,gNoUV)) == 0) {
          disp = disp + 1
        }
      }
    }
  }
  disp
}
embededdness <- function (u,v,g)
{
  emdb = length(comm_find(u,v,g))
  emdb
}

nodeindex <- function(g, vertex)
{
  temp <- which(g$name == vertex)
  temp
}


##Part 1 of Question 5##

for (core in corenodes)
{
  #For every core node create a personal network
  
  subgraph_core <- personal_network(core,g)
  
  
  #Calculate dispersion and embeddedness for every core nore node and it's 1-neighbourhood connections
  
  u <- nodeindex(subgraph_core, coreNode)
  #print(u %in% V(pnOfU))
  
  neighbour_nodes <- V(subgraph_core)
  for(v in neighbour_nodes)
  {
    if(v == u)
      next
    
    embed= embededdness(u,v,subgraph_core)
    dipersion= dispersion(u,v,subgraph_core)
    emb_vec <- c(emb_vec, embed);
    disp_vec <- c(disp_Vec, dispersion);
  }
}
hist (emb_vec, breaks=seq (-0.5, by=1, length.out=max(emb_vector) +2), main ="embeddedness Distribution");

hist (disp_vec, breaks=seq (-0.5, by=1, length.out=max(dispVec) +2), main="dispersion Distribution");

### Question 5 part 2 ###
disp_vec <- c();
emb_vec <- c();
dispEmb_find <- function(g,coreNode)
{
  subgraph_core <- personal_network(coreNode,g)
  
  
  u <- nodeindex(subgraph_core, coreNode)
  neighbour_nodes <- V(subgraph_core)
  for(v in neighbour_nodes)
  {
    if(v == u)
      next
    
    embed= embededdness(u,v,subgraph_core)
    dipersion= dispersion(u,v,subgraph_core)
    emb_vec <- c(emb_vec, embed);
    disp_vec <- c(disp_vec, dispersion);
  }
  ratio <-disp_vec/emb_vec;
  
  #Plot  node of maximum embeddeness
  e_node<-which.max(emb_vec);
  eg = edge.betweenness.community(subgraph_core); 
  mfc = membership(eg)
  edge_set <- E(subgraph_core)[from(u)]
  V(subgraph_core)$color[e_node] <- "black"
  V(subgraph_core)$color[u] <- "blue"
  E(subgraph_core)$color <- "black"
  E(subgraph_core)$color[edge_set] <- "green"
  widEd = rep(1, length(E(subgraph_core)));
  plot(subgraph_core,vertex.size=4,vertex.label=NA,vertex.color=V(subgraph_core)$color,edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),edge.color=E(subgraph_core)$color)
  
  
  #Plot  node of maximum dispersion
  
  d_node<-which.max(disp_vec);
  eg = edge.betweenness.community(subgraph_core); 
  mfc = membership(fc)
  edge_set <- E(subgraph_core)[from(u)]
  V(subgraph_core)$color[e_node] <- "black"
  V(subgraph_core)$color[u] <- "blue"
  E(subgraph_core)$color <- "black"
  E(subgraph_core)$color[edge_set] <- "green"
  widEd = rep(1, length(E(subgraph_core)));
  plot(subgraph_core,vertex.size=4,vertex.label=NA,vertex.color=V(subgraph_core)$color,edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),edge.color=E(subgraph_core)$color)
  
  
  #Plot  node of maximum ratio
  
  r_node<-which.max(ratio);
  eg = edge.betweenness.community(subgraph_core); 
  mfc = membership(fc)
  edge_set <- E(subgraph_core)[from(u)]
  V(subgraph_core)$color[e_node] <- "black"
  V(subgraph_core)$color[u] <- "blue"
  E(subgraph_core)$color <- "black"
  E(subgraph_core)$color[edge_set] <- "green"
  widEd = rep(1, length(E(subgraph_core)));
  plot(subgraph_core,vertex.size=4,vertex.label=NA,vertex.color=V(subgraph_core)$color,edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),edge.color=E(subgraph_core)$color)
  
  
}

dispEmb_find(g,core_nodes[1])
dispEmb_find(g,core_nodes[18])
dispEmb_find(g,core_nodes[22])