

###  This file runs a Monte Carlo Simulation on a Golf Tournament  ###

#  By Daniel Myers, 10/30/15

###

library(dplyr)
library(magrittr)



### Primary Variables to Adjust ####

Save_Location_Sims <-  paste0("Output/Archive/Event_Sims_",Sys.Date(),".csv")


Tournament <- read.csv("Data/Upcoming_Fields_RVest.csv")
# Tournament <- read.csv("Data/2016usopenfield.csv")
Tournament$Country <- NULL

Ratings <- read.csv("Output/Golf_Ratings_Current.csv")
# Partial_Results <- read.csv("Output/Current_Event_Simulation.csv")


# Tour Championship Fedex Cup Code
# Fedex_Pts_Before_TC <- read.csv("Data/Fedex_Pts_Reset_Tour_Champ.csv")
# Fedex_Pts_From_TC <- read.csv("Data/Fedex_Pts_Tour_Champ.csv")
# TC_Tournament_ID <- 6381
# 
# Tournament %<>% filter(Event_ID == TC_Tournament_ID)



Trials <- 10000


### Map Data ####

# names(Tournament) <- c("World_Rnk","Country","Player")

Tournament_Projection <- merge(Tournament,Ratings[,c("Player_ID","Rank","OWGR_Rank","Projected_Rating","Projected_Stdev","Weight_Sum","Recent_Tour","Rounds_Last_Year","Country")],by = c("Player_ID"),all.x = TRUE)

# Tournament_Projection <- merge(Tournament_Projection,Fedex_Pts_Before_TC)

# Tournament_Projection$Projected_Rating[Tournament_Projection$Player_Name=="Mark O'Meara"] <- 2.0
# Tournament_Projection$Projected_Stdev[Tournament_Projection$Player_Name=="Mark O'Meara"] <- 3.0


Tournament_Projection$Projected_Rating[is.na(Tournament_Projection$Projected_Rating)] <- 3.0
Tournament_Projection$Projected_Stdev[is.na(Tournament_Projection$Projected_Stdev)] <- 3.0

Tournament_Projection$Projected_Stdev <- Tournament_Projection$Projected_Stdev - 0.5

str(Tournament_Projection)


### Partial Results ####

# Update Player Ratings - give current tournament more weight
# 
# Current_Tourney_Wt <- 0
# Weight_Add <- 12   # Add weight to current ratings (for priors)
# 
# Tournament_Projection <- merge(Tournament_Projection,Partial_Results) %>%
#   mutate(Projected_Rating_Start = Projected_Rating,
#          Projected_Rating = ((Projected_Rating_Start*(Weight_Sum+Weight_Add) + 
#                                Round_1 * Current_Tourney_Wt)/
#                               (Weight_Sum+Weight_Add+Current_Tourney_Wt))
#            )




### Simulation Engine ####

Sim_Once <- function(Data,Iteration) {
#   # For prototyping:
#   Data = Tournament_Projection
#   Iteration = 1
  
  Sim_Result_1 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_2 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_3 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  Sim_Result_4 <- unlist(lapply(Data[,c("Projected_Stdev")],function(x) rnorm(1,sd = x)))
  
  Results_Frame <- cbind.data.frame (Sim_Result_1,Sim_Result_2,Sim_Result_3,Sim_Result_4)

  Result <- Data[,c("Player_Name","Player_ID","Event_ID")]
  
  Result$Sim_Result_Raw <- (rowSums(Results_Frame)  + 4*Data$Projected_Rating)
  
  Result$Sim_Result <- Result$Sim_Result_Raw # + Result$Round_1
  
  Result$Sim_Result_Rounded <- round(Result$Sim_Result)  # round to nearest stroke
   
  Result %<>% group_by(Event_ID) %>%
  mutate( Rank = min_rank(Sim_Result),
          Rank_Round = min_rank(Sim_Result_Rounded),
          Rank_Final = Rank_Round) %>% ungroup()  # Rank the results
  
  Result$Rank_Final[Result$Rank_Round==1 & !Result$Rank==1] <- 2  # Break ties for winner

#   Result <- merge(Result, Fedex_Pts_From_TC,by.x = "Rank", by.y = "Tour_Champ_Rank" )
#   
#   Tour_Champ_Pts_Split <- group_by(Result, Event_ID, Rank_Final) %>%
#     summarise( Fedex_Pts_to_split = sum(Fedex_Pts_Earned),
#                Num_Tied = length(Fedex_Pts_Earned),
#                Fedex_Pts_Split = Fedex_Pts_to_split/Num_Tied) %>% ungroup() 
#   
#   Result$Fedex_Pts_Earned <-  
#     Tour_Champ_Pts_Split$Fedex_Pts_Split[match(Result$Rank_Final,Tour_Champ_Pts_Split$Rank_Final)]
#   
#   Result$Fedex_Pts_Final <-  Result$Fedex_Pts + Result$Fedex_Pts_Earned
  
  Result %<>% group_by(Event_ID) %>%
#    mutate( Fedex_Rank = min_rank(-Fedex_Pts_Final)) %>% 
    ungroup()  # Rank the results  
  
  # View(Result)
  
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
                      Win = sum(Rank == 1)/Trials, 
                      Top_5 = sum(Rank<6)/Trials, 
                      Top_10 = sum(Rank<11)/Trials
#                       Fedex_Champ = sum(Fedex_Rank == 1)/Trials,
#                       Fedex_2nd = sum(Fedex_Rank == 2)/Trials,
#                       Fedex_Top_5 = sum(Fedex_Rank<6)/Trials,
#                       Fedex_Top_10 = sum(Fedex_Rank<11)/Trials,
#                       Avg_Fedex_Pts = mean(Fedex_Pts_Final)
                      )


Tournament_Projection_Out <-
  merge(Tournament_Projection,Finishes,by = c("Player_ID"))

Tournament_Projection_Out %<>% group_by(Event_ID) %>%
  mutate(Win_Rank = min_rank(-Win)
         # Fedex_Win_Rank = min_rank(-Fedex_Champ)
         ) %>% 
  ungroup()


### Rearrange and export results ####

Tournament_Projection_Out <-
  Tournament_Projection_Out[,c("Event_ID",
                            "Event_Name",
#                            "Event_Date",
                            "Event_Tour_1",
                            "Rank",
                            "OWGR_Rank",
                            "Player_Name",
                            "Player_ID",
                            "Projected_Rating",
                            "Projected_Stdev",
#                             "Fedex_Win_Rank",
#                             "Fedex_Champ",
#                             "Fedex_2nd",
#                             "Fedex_Top_5",
#                             "Fedex_Top_10",
#                             "Avg_Fedex_Pts",
                            "Win_Rank",
                            "Win",
                            "Top_5",
                            "Top_10",
                            "Country",
                            "Rounds_Last_Year",
                            "Recent_Tour"
  )]


Tournament_Projection_Out <- arrange(Tournament_Projection_Out,Event_ID, Win_Rank, Rank)


write.csv(Tournament_Projection_Out, file = "Output/Current_Event_Simulation.csv", row.names = FALSE)

write.csv(Tournament_Projection_Out, file = Save_Location_Sims, row.names = FALSE)

