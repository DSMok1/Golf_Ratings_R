

###  This file runs a Monte Carlo Simulation on a Golf Tournament  ###

#  By Daniel Myers, 10/30/15

###

### Primary Variables to Adjust ####



#Tournament <- read.csv("~/ETC/Sports/Golf/Golf_Ratings_R/Output/Upcoming_Fields_RVest.csv")
Tournament <- read.csv("~/ETC/Sports/Golf/2016mastersfield.csv")

Ratings <- read.csv("~/ETC/Sports/Golf/Golf_Ratings_R/Output/Current_Ratings_4_Years_0.98_2016-04-06_Masters.csv")

Trials <- 500000



### Map Data ####

# names(Tournament) <- c("World_Rnk","Country","Player")

Tournament_Projection <- merge(Tournament,Ratings[,c("Player_ID","Rank","OWGR_Rank","Projected_Rating","Projected_Stdev","Recent_Tour","Rounds_Last_Year")],by = c("Player_ID"),all.x = TRUE)


Tournament_Projection$Projected_Rating[Tournament_Projection$Player_Name=="Mark O'Meara"] <- 2.0
Tournament_Projection$Projected_Stdev[Tournament_Projection$Player_Name=="Mark O'Meara"] <- 3.0


Tournament_Projection$Projected_Rating[is.na(Tournament_Projection$Projected_Rating)] <- 3.0
Tournament_Projection$Projected_Stdev[is.na(Tournament_Projection$Projected_Stdev)] <- 3.0

str(Tournament_Projection)



### Simulation Engine ####

Sim_Once <- function(Data,Iteration) {
  
  Sim_Result_1 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_2 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_3 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_4 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  
  Results_Frame <- cbind.data.frame (Sim_Result_1,Sim_Result_2,Sim_Result_3,Sim_Result_4)

  Result <- Data[,c("Player_Name","Player_ID","Event_ID")]
  
  Result$Sim_Result <- (rowSums(Results_Frame)  + 4*Data$Projected_Rating)
   
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


write.csv(Tournament_Projection_Out, file = "~/ETC/Sports/Golf/Golf_Ratings_R/Output/Masters_Simulation.csv", row.names = FALSE)

