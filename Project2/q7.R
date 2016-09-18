#moviedID<-which(v_name %in% "Mission: Impossible - Rogue Nation (2015)")
#movieID<-which(v_name %in% "Minions (2015)")
movieID<-which(v_name %in% "Batman v Superman: Dawn of Justice (2016)")

edgeWeight<-E(g)$weight
movie1=neighbors(g,V(g)[movieID])
movie1_rating<-c()
temp1=0
temp2=0
neighbour_rating=c()
neighbour_wt=c()

ind=1
i=1
while(i < 10){
  if(!is.null(map[[v_name[movie1[i]]]])){
  movie1_rating<-c(movie1_rating,map[[v_name[movie1[i]]]])
  ID = get.edge.ids(g,c(V(g)[movieID],V(g)[movie1[i]]))
  neighbour_rating=c(neighbour_rating,as.double(movie1_rating[ind]))
  neighbour_wt=c(neighbour_wt,edgeWeight[ID])
  temp1=temp1+(as.double(movie1_rating[ind])*edgeWeight[ID])
  temp2=temp2+edgeWeight[ID]
  ind=ind+1
  }
  i=i+1
}
