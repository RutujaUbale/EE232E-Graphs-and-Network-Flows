#load("C:/Acads/Movie_Comm_Detect/Rimage.RData")

MG=read.csv("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\movie_genre.txt",sep='\t',header = FALSE)
MG$V2=NULL

#tel= read.table("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\network_of_movies_20.txt",sep='\t',colClasses=c('character','character','numeric'),comment.char = "",stringsAsFactors=FALSE,quote = "")
g=read.graph("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\network_of_movies_20.txt", format = c("ncol"),weights='yes')
com_struct=fastgreedy.community(g)


comstruct_fg_commsize=c()
for (i in 1:length(unique(com_struct$membership))){
  print(i)
  communities_indexes= which(com_struct$membership == i)
  comstruct_fg_commsize= c(comstruct_fg_commsize,length(communities_indexes))
}
barplot(comstruct_fg_commsize,main="Community Sizes",ylab="Community Sizes",names.arg=1:length(comstruct_fg_commsize))
###
library("igraph")

#filePath = "C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\network_of_movies_smaller.txt"
#g_movie = read.graph(file = filePath ,format = "ncol",directed = F)
tel= read.table("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\network_of_movies.txt",sep='\t',colClasses=c('numeric','numeric','numeric'),comment.char = "",stringsAsFactors=FALSE,quote = "")
new_data_frame=tel
new_data_frame = data.frame("V1"=new_data_frame$V1, "V2"=new_data_frame$V2,"weight"=new_data_frame$V3,stringsAsFactors = FALSE)
g_movie=graph.data.frame(new_data_frame,directed=FALSE)
print("done")
is.directed(g_movie)
g_movie_com = fastgreedy.community(g_movie)

## add attribute genre to the graph
fileName = "C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\movie_genre.txt"
file_genre = file(fileName,open = "r")
lines = readLines(file_genre)
Genre = rep("null",vcount(g_movie))
count = 0
for (i in 1:length(lines)){
  if(count %in% V(g_movie)$name){
    list = strsplit(lines[i],"\t\t")
    index = which(V(g_movie)$name == count)
    Genre[index] = list[[1]][2]
    cat(lines[count+1],index,list[[1]][2],"\n")
  }
  count = count + 1
}
close(file_genre)
V(g_movie)$genre = Genre
## tag the communtiy with genre that appear more than 20% in the community
sink("C:\\Users\\Hp\\Desktop\\UCLA_Q3\\graphs_network_flows\\project_2\\Community_Genres.txt")
com_tag = numeric(0)
for (i in 1:length(g_movie_com)){
  com_genre = V(g_movie)[which(g_movie_com$membership ==i)]$genre
  max = 0
  max_index = "null"
  genre_type = unique(com_genre)
  for(genre in genre_type)
  {
    if(length(which(com_genre == genre))>max && length(which(com_genre == genre))>length(com_genre)*0.2 && genre!="null")
    {
      max = length(com_genre[which(com_genre == genre)])
      max_index = genre
    }
  }
  cat(i,"\t",max_index,"\n")
  com_tag = c(com_tag,max_index)
  
}
sink()
closeAllConnections()
