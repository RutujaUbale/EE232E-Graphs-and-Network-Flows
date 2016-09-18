import re
actor_read_file = open("/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/project_2_data/actor_movies.txt", 'r')
actress_read_file = open("/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/project_2_data/actress_movies.txt", 'r')

process_file = open("/Users/rutuja/Documents/Graphs_and_Network_Flows/Project_2/processed_actors_actresses.txt", 'w')

threshold = 5
id = 0
cnt=0

for line in actor_read_file.readlines():
	line = line[:-1]
	temp = line.split("\t\t")
	
	if len(temp)< threshold+1:
		continue
	temp[0].strip(" ")
	temp[0].strip("\t")
	process_file.write(str(temp[0]) + "\t\t" + str(id))
	id +=1
	process_file.write("\n")
	cnt = cnt+1

print("Finished actors")

for line in actress_read_file.readlines():
	line = line[:-1]
	line = re.sub("^\\(", "",line)
	line = re.sub("\\([^[:digit:]]+\\)", "",line)
	line = re.sub("^\\s+|\\s+$", "",line)
	temp=line.split("\t\t")

	if len(temp)< threshold+1:
		continue
	temp[0].strip(" ")
	temp[0].strip("\t")
	id +=1
	process_file.write(str(temp[0]) + "\t\t" + str(id))
	process_file.write("\n")
	cnt = cnt+1

print("Finished actresses")

print "Total number of actors and actresses:" , cnt

