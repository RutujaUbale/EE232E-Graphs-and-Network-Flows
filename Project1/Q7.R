q7 <- function(fileName){
  require(igraph)
  
  options(scipen=999) # This line is because R was reading the large numbers in scientific notation
    
  dataFile = readLines(paste("C:/Users/user/Downloads/gplus/",fileName,".edges",sep=""))
  d <- list()
  temp1 <- unlist(strsplit(dataFile[1]," "))
  d[[1]] <- temp1[1]
  d[[2]] <- temp1[2]
  
  for(i in 2:length(dataFile)){
    temp1 <- unlist(strsplit(dataFile[i]," "))
    d[[1]] <- c(d[[1]],temp1[1])
    d[[2]] <- c(d[[2]],temp1[2])
  }
  
  g=graph.data.frame(d, directed=TRUE)
  
  #Adding the core node
  g=g+vertices(fileName)
  vertNames = V(g)$name
  
  #Connecting all the edges between core node and other nodes
  for(i in (1:(length(vertNames)-1))){
    g=g+edges(vertNames[length(vertNames)],vertNames[i],directed=TRUE)
  }
  
  names <- V(g)$name 
  comm1<-walktrap.community(g)
  comm2<-infomap.community(g)
  commList1 <- list()
  commList2 <- list()
  
  for(i in (1:length(comm1))){
    ind<-which(comm1$membership==i)
    commList1[[i]] <- names[ind]
  }
  
  for(i in (1:length(comm2))){
    ind<-which(comm2$membership==i)
    commList2[[i]] <- names[ind]
  }
  
  allNodes <- unlist(names)
  circleFile <- readLines(paste("C:/Users/user/Downloads/gplus/",fileName,".circles",sep=""))
  common1 <- matrix(0,length(circleFile),length(commList1))
  common2 <- matrix(0,length(circleFile),length(commList2))
  
  for(i in (1:length(circleFile))){
    print(i)
    user <- unlist(strsplit(circleFile[i],"\t"))
    userList <- intersect(allNodes,user)
    print(length(userList))
    for(j in 1:length(commList1)){
      common1[i,j]=length(intersect(userList,unlist(commList1[[j]])))/length(union(userList,commList1[[j]]))
    }
    
    for(j in 1:length(commList2)){
      common2[i,j]=length(intersect(userList,unlist(commList2[[j]])))/length(union(userList,commList2[[j]]))
    }
  
  }
  print(paste("Max for walktrap: ",max(common1)))
  print(paste("Max for infomap: ",max(common2)))
  return(common1)
}