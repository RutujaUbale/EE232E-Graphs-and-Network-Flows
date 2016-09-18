#Question 2a and 2b

# Create network of 1000 nodes following power law distribution

deg_dist <- sample(1:1000,1000,replace = TRUE, prob = (1:1000)^-3) # Creating the distribution and sampling
if (sum(deg_dist) %% 2 != 0) {deg_dist[1] <- deg_dist[1] + 1} # Just a check to make sure degrees sum to even
g_p <- degree.sequence.game(out.deg=deg_dist, method = "simple.no.multiple") # Generating the random network by connecting the stubs
#g_p<-barabasi.game(10000,directed = FALSE)
dia<-diameter(g_p)
par(mfrow=c(2,1))
hist1=hist(degree(g_p),seq(-0.5, by=1, length.out = max(degree(g_p))+2))
x1 = data.frame(x = hist1$mids, y = hist1$density)
plot(x1, type = "o", main = "Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")
is_connected(g_p)
ecount(g_p)


# Creating the giant connected component

gr_conn_list <- decompose.graph(g_p)
gcc_index <- which.max(sapply(gr_conn_list,vcount))
gcc <- gr_conn_list[[gcc_index]]
vtx<-vcount(gcc)
plot( g_p, vertex.size=2, vertex.label=NA, edge.arrow.size=0 )
plot( gcc, vertex.size=2, vertex.label=NA, edge.arrow.size=0 )

# Creating the communities

g_p_comm <- cluster_fast_greedy(gcc)
modularity(g_p_comm)
cm_size <- as.vector(sizes(g_p_comm))
plot(cm_size,main="Community Sizes for GCC",xlab="Index",ylab="Community Size")
lines(cm_size)

#Question 2c

# 
# Create network of 10000 nodes following power law distribution


deg_dist_large <- sample(1:10000,10000,replace = TRUE, prob = (1:10000)^-3) # Creating the distribution and sampling
if (sum(deg_dist_large) %% 2 != 0) {deg_dist_large[1] <- deg_dist_large[1] + 1} # Just a check to make sure degrees sum to even
g_p_large <- degree.sequence.game(out.deg=deg_dist, method = "simple.no.multiple") # Generating the random network by connecting the stubs
#g_p_large<-barabasi.game(10000,directed = FALSE)
dia<-diameter(g_p_large)
hist1=hist(degree(g_p_large),seq(-0.5, by=1, length.out = max(degree(g_p_large))+2))
x1 = data.frame(x = hist1$mids, y = hist1$density)
plot(x1, type = "o", main = "Degree Distribution with 10000 nodes", xlab = "Degree", ylab = "Density")
is_connected(g_p_large)
ecount(g_p_large)


# Creating the giant connected component

gr_conn_list_large <- decompose.graph(g_p_large)
gcc_index_large <- which.max(sapply(gr_conn_list_large,vcount))
gcc_large <- gr_conn_list[[gcc_index_large]]
v<-vcount(gcc)
plot( g_p_large, vertex.size=2, vertex.label=NA, edge.arrow.size=0 )
plot( gcc_large, vertex.size=2, vertex.label=NA, edge.arrow.size=0 )
# Creating the communities

g_p_comm_large <- cluster_fast_greedy(gcc_large)
modularity(g_p_comm_large)
cm_sizes <- as.vector(sizes(g_p_comm_large))
plot(cm_sizes,main="Community Sizes for GCC",xlab="Index",ylab="Community Size")
lines(cm_sizes)
#question 2d
degrees = array(0,vcount(g_p));
for(k in 1:vcount(g_p))
{
  i = sample(vcount(g_p),1);
  n = neighbors(g_p, i, mode = "total");
  
  if(length(n)>0){
    
    j = sample(length(n),1);
    degrees[k] = degree(g_p,n[j]);
    
    
  }
}


hist(degrees,seq(-0.5,by=1,length.out=max(degrees)+2))
