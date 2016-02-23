###

# Importing Historic OWGR

###

###  Imports ####
# Grab JSON:

library(jsonlite)
library(rvest)
library(data.table)
library(dplyr)

### Function to scrape OWGR Data ####

Import_Historic_OWGR <- function (ID) {
  #  ID <- 2
  
  Raw <-
    scan(
      file = paste0(
        "http://www.owgr.com/layouts/OWGR/PlayerRankingsForGraph.aspx?playerID=",ID
      ), what = character(), quiet = TRUE
    )
  
  Data <- try(fromJSON(Raw, simplifyDataFrame = TRUE),silent = TRUE)
  
  if (typeof(Data) == "character") {
    Player_OWGR_Rank <- data.frame(week = NA, rank = NA)
  } else {
    Player_OWGR_Rank <-
      (Data$player[2:nrow(Data$player),])[,c("week","rank")]
    Player_OWGR_Rank$week <- as.Date(Player_OWGR_Rank$week)
    Player_OWGR_Rank$rank <- as.integer(Player_OWGR_Rank$rank)
  }
  
  # plot(as.Date(Player_OWGR_Rank$week),Player_OWGR_Rank$rank,ylim=c(500,1),type="l")
  
  Raw2 <-
    scan(
      file = paste0(
        "http://www.owgr.com/Global/Widgets/PlayerProfileFeed/PlayerProfileFeed.aspx?playerID=",ID
      ), what = character(), quiet = TRUE
    )
  
  Player_Name <- Raw2[match("playerName",Raw2) + 2]
  
  Player_OWGR_Rank$Player_Name <- Player_Name
  Player_OWGR_Rank$Player_ID <- ID
  
  Output <- list(Player_OWGR_Rank)
  
}

### Compile Outputs of scraping ####

# Player_History <- data.frame()

# ID = 5

Begin_ID <- 1
End_ID <- 22000

Begin_time <- Sys.time()

for (ID in Begin_ID:End_ID) {
  Output_Data <- Import_Historic_OWGR(ID)   # Scrape the given ID
  
  # Compile the status of the scraping
  if (nrow(Player_History) == 0) {
    Player_History <- as.data.frame(Output_Data[[1]])
    
  } else {
    Player_History <-
      rbindlist(list(Player_History,as.data.frame(Output_Data[[1]])),use.names = TRUE)
  }

  cat ("ID",ID,"of",End_ID,"(",round(ID/End_ID*100,1),"% )\n")
  
  if (ID %% 50 == 0) {
    Elapsed_time <- difftime(Sys.time(),Begin_time,units ="mins")
    Estimated_time <- (End_ID-Begin_ID)/(ID-Begin_ID)*Elapsed_time
    Estimated_completion <- Begin_time + Estimated_time
    Time_left <- Estimated_time - Elapsed_time
    cat (Elapsed_time,"mins taken so far; Time Left:",Time_left,"mins; will finish at about",strftime(Estimated_completion,"%H:%M:%S"),"\n")
  }
  

}

Player_History <- rename(Player_History,
                         OWGR_Rank = rank,
                         OWBR_Rank_Date = week)


write.csv(
  Player_History,file = gzfile(
    "~/ETC/Sports/Golf/Golf_Ratings_R/Output/Player_OWGR_History.csv.gz"
  ), row.names = FALSE
)