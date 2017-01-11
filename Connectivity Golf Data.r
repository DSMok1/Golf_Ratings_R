###########

# Check connectivity of Golf Data

###########


##### Import from CSV File ######
Golf_Data <-
  read.csv(gzfile("Data/Player_Results_RVest.csv.gz"))

Golf_Data$Event_Date <- as.Date(Golf_Data$Event_Date)

#View(Golf_Data)
#str(Golf_Data)

##########


require(plyr)

Trial_Subset <- subset(Golf_Data, (Event_Date < as.Date("2017-01-01")
                                   & Event_Date > as.Date("2014-12-31")
                                   & Event_Tour_1 != "MENA Tour"
#                                    & Event_Tour_1 != "ProGolf Tour"
#                                    & Event_Tour_1 != "Nordic Golf League"
#                                    & Event_Tour_1 != "Alps Golf Tour"
                                   ) )



require(plyr)
Rounds_Each_Tour <- ddply(Trial_Subset,c("Player_Name","Player_ID","Event_Tour_1"),
                                summarise,
                                Count = length(Round_Num))

# View(Rounds_Each_Tour)

Rounds_Each_Tour <- subset(Rounds_Each_Tour, Count > 10 )


#  Remove all players who haven't played multiple "Tours"

require(plyr)
Number_Tours_Player <- ddply(Rounds_Each_Tour,c("Player_Name","Player_ID"),
                                    summarise,
                                    Tours_Played = length(Count))

Rounds_Each_Tour <-
  merge(Rounds_Each_Tour,Number_Tours_Player,by = c("Player_Name","Player_ID"))

Rounds_Each_Tour$Player_ID <- factor(Rounds_Each_Tour$Player_ID)

str(Rounds_Each_Tour)

Rounds_Each_Tour <- subset(Rounds_Each_Tour, Tours_Played > 1 )

Rounds_Each_Tour <- droplevels(Rounds_Each_Tour)

View(Rounds_Each_Tour)
str(Rounds_Each_Tour)

Rounds_Each_Tour <- arrange(Rounds_Each_Tour,Player_ID,Event_Tour_1)


###  Identify all combinations

Rounds_Each_Tour$Tour <- as.character(Rounds_Each_Tour$Event_Tour_1)

str(Rounds_Each_Tour)


require(plyr)
Edges <- do.call(rbind,dlply(Rounds_Each_Tour,.(Player_ID),function(x)t(combn(x$Tour,2))))

Edges <- as.data.frame(Edges)

### Group by how many of each combination

Edges$weight <-1
Edges <- aggregate(weight ~ V1 + V2, Edges, sum)


Edges_2 <- subset(Edges, weight > 5 )

require(igraph)
# Load (DIRECTED) graph from data frame
g <- graph.data.frame(Edges_2, directed=FALSE)


# Plot graph
plot(g, 
     edge.width=0.4*(E(g)$weight)^0.75,
     layout=layout.fruchterman.reingold(g,weights=sqrt((E(g)$weight))),

     vertex.label.family="sans",
     vertex.label.color="black",
     vertex.label.cex=1.2,

     vertex.color=ifelse(V(g)$name =="Major Championship", "tomato3","skyblue"),
     vertex.size = 12,
     vertex.frame.color=NA,

     edge.label=ifelse(E(g)$weight <15, "",E(g)$weight),
     edge.label.cex=1,
     edge.label.family="sans",
     edge.label.color="black",

     edge.color=ifelse(E(g) %in% incident(g,"Major Championship","all"),"tomato","gold"),
     edge.curved=0.3
     )

