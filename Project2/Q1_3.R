library("igraph")
library("hash")
library(e1071)
#create the graph from the document
g<-read.graph("/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/edge_weight.txt",format="ncol",directed=TRUE)
actor_id_file<-file("/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/processed_actors_actresses.txt",open="r")
actor_id_map<-readLines(actor_id_file,encoding="latin1")

#top 10 actor/actress based on pagerank algorithm
rank_score<-page.rank (g, algo = c("prpack", "arpack", "power"),
                       directed = TRUE, damping = 0.85,
                       weights = NULL)

high_score<-sort(rank_score$vector,decreasing=TRUE)
high_id<-NULL
for(i in 1:10)
{
  high_id<-as.numeric(names(high_score[i]))
  # the index of actor_id_map starts from 1
  high_ID<-actor_id_map[high_id+1]
  aline=strsplit(high_ID,"\t\t")
  print(aline[[1]][1])
  print(high_score[i])
}

famous_name<-c("Cruise, Tom","Portman, Natalie","Clooney, George","Winslet, Kate","Downey Jr., Robert",
                              "DiCaprio, Leonardo", "Pitt, Brad","Roberts, Julia","Depp, Johnny","Bullock, Sandra")
famous_id<-c(215993,219305,26672,241733,37717,35920,110499,222859,35092,165853)


famous_score<-hash()
for(i in 1:length(rank_score$vector))
{
  for(j in 1:10)
  {
    id<-famous_id[j]
    if(names(rank_score$vector[i])==id)
    {
      .set(famous_score,keys=famous_name[j],values=rank_score$vector[i])
    }
  }
}
print(values(famous_score,keys=famous_name))
close(actor_id_file)

#find the significant pairs
undirected_g<-as.undirected(g,mode="collapse",edge.attr.comb=list(weight="mean"))
high_weight<-sort(E(undirected_g)$weight,index.return=TRUE,decreasing=TRUE)
for(i in 1:10)
{
  highest=high_weight$ix[i]
  print(highest)
  print(high_weight$x[i])
  print(E(undirected_g)[highest])
}

#find the names of those significant pairs
significant_id<-c(669,42246,7132,2278,75491,129907,7282,95252,93967,103518,52397)
for(i in 1:length(significant_id)){
  idx<-significant_id[i]
  print(idx)
  id<-as.numeric(names(V(undirected_g)[idx]))
  print(id)
  significant_ID<-actor_id_map[id+1]
  aline=strsplit(significant_ID,"\t\t")
  print(aline[[1]][1])
}

