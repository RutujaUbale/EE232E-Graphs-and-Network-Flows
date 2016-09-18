writefile = open( "/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/merging_actor_actress.txt",'w')
actorfile = open( "/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/project_2_data/actor_movies.txt",'r')
actressfile = open("/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/project_2_data/actress_movies.txt",'r')
threshold = 5
id= 0

#Processing the date in actor_movies.txt
for line in actorfile.readlines():
    #print id, line
    line = line[:-1]
    temp=line.split("\t\t")
    if len(temp)< threshold+1:
        continue
    writefile.write(str(id))
    id+=1
    for i in tmp[1:]:
        end = i.find(")")
        i = i[:end+1]
        i.strip(" ")
        i.strip("\t")
        writefile.write("\t\t")
        writefile.write(str(i))
    writefile.write("\n")
print"actor"

#Processing the date in actress_movies.txt
for line in actressfile.readlines():
    # print id, line
    line = line[:-1]
    temp=line.split("\t\t")
    if len(temp)<threshold+1:
        continue
    writefile.write(str(id))
    id+=1
    for i in tmp[1:]:
        end = i.find(")")
        i = i[:end+1]
        i.strip(" ")
        i.strip("\t")
        writefile.write("\t\t")
        writefile.write(str(i))
    writefile.write("\n")
writefile.close()
print"actress"