
MG=read.csv("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\network_of_movies_20.txt",sep='\t',header = FALSE)
MG$V2=NULL

#m1="Batman v Superman: Dawn of Justice (2016)"
m1="Minions (2015)"
#m1="Mission: Impossible - Rogue Nation (2015)"
g=read.graph("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\network_of_movies_20.txt", format = c("ncol"),weights='yes')
n_m1=V(g)$name[neighbors(g,m1)]
#n_m2=V(g)$name[neighbors(g,m2)]
#n_m3=V(g)$name[neighbors(g,m3)]

edge_weights=c()
for (i in 1:length(n_m1)){
  edge_weights=c(edge_weights,E(g)$weight[get.edge.ids(g,c(m1,n_m1[i]))])
}
top_5_indices=unlist(as.matrix(sort.int(edge_weights, index.return=TRUE,decreasing=TRUE))[2])[1:5]
top_5_names=n_m1[top_5_indices]

com_struct$membership[com_struct$names %in% top_5_names]

