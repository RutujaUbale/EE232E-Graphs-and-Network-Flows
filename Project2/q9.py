#from itertools import imap



def clean_str(inp):
    inp1 = inp.replace("(voice)","")
    inp2 = inp1.replace("(VG)","")
    inp3 = inp2.replace("(uncredited)","")
    return inp3.rstrip()

    
new_movies = ["Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)", "Minions (2015)"]
movie_actor = {}

for m in new_movies:
    movie_actor[m] = []
    
actor_movie = {}
for line in open("/Users/user/Downloads/ee ml/project_2_data/actor_movies.txt"):
    actor = line.split("\t\t")[0]
    movies = line[:-1].split("\t\t")[1:]
    for m in new_movies:
        if m in line:
            movie_actor[m].append(actor)
            actor_movie[actor] = map(clean_str,movies)


movie_rating = {}
for line in open("/Users/user/Downloads/ee ml/project_2_data/movie_rating.txt"):
    movie_rating[line.split("\t\t")[0]] = float(line[:-2].split("\t\t")[1])

actor_rating = {}
for k in actor_movie.keys():
    temp = 0
    number = 0
    for m in actor_movie[k]:
        if m in movie_rating.keys():
            temp += movie_rating[m]
            number += 1
    if number==0:
        actor_rating[k] = 0
    else:
        actor_rating[k] = temp/number

for m in movie_actor.keys():
    temp = 0
    number = 0
    for a in movie_actor[m]:
        if actor_rating[a]!=0:
            temp += actor_rating[a]
            number += 1
    print (m)
    rating = temp/number
    print (rating)
