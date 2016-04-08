

###  This file runs a Monte Carlo Simulation on a Golf Tournament  ###

#  By Daniel Myers, 10/30/15

###

library(dplyr)
library(magrittr)



### Primary Variables to Adjust ####



#Tournament <- read.csv("~/ETC/Sports/Golf/Golf_Ratings_R/Output/Upcoming_Fields_RVest.csv")
Tournament <- read.csv("~/ETC/Sports/Golf/2016mastersfield.csv")

Ratings <- read.csv("~/ETC/Sports/Golf/Golf_Ratings_R/Output/Current_Ratings_4_Years_0.98_2016-04-06_Masters.csv")
Partial_Results <- read.csv("~/ETC/Sports/Golf/Golf_Ratings_R/Output/Masters_Round_1.csv")


<<<<<<< HEAD
Trials <- 10000
=======
Trials <- 1000
>>>>>>> ca031320975da43d8096ff427e545f1479b81a7b



### Map Data ####

# names(Tournament) <- c("World_Rnk","Country","Player")

Tournament_Projection <- merge(Tournament,Ratings[,c("Player_ID","Rank","OWGR_Rank","Projected_Rating","Projected_Stdev","Weight_Sum","Recent_Tour","Rounds_Last_Year")],by = c("Player_ID"),all.x = TRUE)


Tournament_Projection$Projected_Rating[Tournament_Projection$Player_Name=="Mark O'Meara"] <- 2.0
Tournament_Projection$Projected_Stdev[Tournament_Projection$Player_Name=="Mark O'Meara"] <- 3.0


Tournament_Projection$Projected_Rating[is.na(Tournament_Projection$Projected_Rating)] <- 3.0
Tournament_Projection$Projected_Stdev[is.na(Tournament_Projection$Projected_Stdev)] <- 3.0

Tournament_Projection$Projected_Stdev <- Tournament_Projection$Projected_Stdev - 0.6

str(Tournament_Projection)


### Partial Results ####

# Update Player Ratings - give current tournament more weight

<<<<<<< HEAD
Current_Tourney_Wt <- 2
Weight_Add <- 12   # Add weight to current ratings (for priors)
=======
Current_Tourney_Wt <- 3
Weight_Add <- 10   # Add weight to current ratings (for priors)
>>>>>>> ca031320975da43d8096ff427e545f1479b81a7b

Tournament_Projection <- merge(Tournament_Projection,Partial_Results) %>%
  mutate(Projected_Rating_Start = Projected_Rating,
         Projected_Rating = ((Projected_Rating_Start*(Weight_Sum+Weight_Add) + 
                               Round_1 * Current_Tourney_Wt)/
                              (Weight_Sum+Weight_Add+Current_Tourney_Wt))
           )


### Simulation Engine ####

Sim_Once <- function(Data,Iteration) {
  
  Sim_Result_1 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_2 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_3 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  # Sim_Result_4 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  
  Results_Frame <- cbind.data.frame (Sim_Result_1,Sim_Result_2,Sim_Result_3)

  Result <- Data[,c("Player_Name","Player_ID","Event_ID","Round_1")]
  
  Result$Sim_Result_Raw <- (rowSums(Results_Frame)  + 3*Data$Projected_Rating)
  
  Result$Sim_Result <- Result$Sim_Result_Raw + Result$Round_1
   
  Result$Rank <- unlist(with(Result,tapply(Sim_Result,Event_ID,rank)))[order(order(Result$Event_ID))]
  
  return (Result)
  
}


# Trial <- Sim_Once (Tournament_Projection)
# str(Trial)

library(data.table)

Monte_Carlo_Sim <- function(Data,Iterations) {
  
  results <- (replicate(Iterations, Sim_Once(Data)))

  Output <- as.data.frame(rbindlist(as.data.frame(results)))
  
  return(Output)
}


Sys.time()
Trial_Sim <- Monte_Carlo_Sim (Tournament_Projection,Trials)
Sys.time()
str(Trial_Sim)


library(dplyr)
Player_ID_Group <- group_by(Trial_Sim, Player_ID)
Finishes <- summarise(Player_ID_Group,
                                  Win = sum(Rank == 1)/Trials, Top_5 = sum(Rank<6)/Trials, Top_10 = sum(Rank<11)/Trials)

# Finishes$Win_Rank <- rank(-Finishes$Win)

Tournament_Projection_Out <-
  merge(Tournament_Projection,Finishes,by = c("Player_ID"))

Tournament_Projection_Out$Win_Rank <- unlist(with(Tournament_Projection_Out,tapply(-Win,Event_ID,rank)))[order(order(Tournament_Projection_Out$Event_ID))]

### Rearrange and export results ####

Tournament_Projection_Out <-
  Tournament_Projection_Out[,c("Event_ID",
                            "Event_Name",
                            "Event_Date",
                            "Event_Tour_1",
                            "Rank",
                            "OWGR_Rank",
                            "Player_Name",
                            "Player_ID",
                            "Projected_Rating",
                            "Projected_Stdev",
                            "Win_Rank",
                            "Win",
                            "Top_5",
                            "Top_10",
                            "Country",
                            "Rounds_Last_Year",
                            "Recent_Tour"
  )]

Tournament_Projection_Out <-
  Tournament_Projection_Out[order(Tournament_Projection_Out$Rank, decreasing = FALSE),]

Tournament_Projection_Out <-
  Tournament_Projection_Out[order(Tournament_Projection_Out$Win_Rank, decreasing = FALSE),]

Tournament_Projection_Out <-
  Tournament_Projection_Out[order(Tournament_Projection_Out$Event_ID, decreasing = FALSE),]


write.csv(Tournament_Projection_Out, file = "~/ETC/Sports/Golf/Golf_Ratings_R/Output/Masters_Simulation_Trial_Round_1.csv", row.names = FALSE)

