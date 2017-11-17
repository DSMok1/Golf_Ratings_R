###

# Scraping the OWGR stats with RVest

###

### Imports ####
library(plyr)
library(rvest)
library(magrittr)
library(lubridate)
library(stringr)
library(dplyr)

### Make this a Function ####

Import_Tourney_Results <- function(ID)  {
  ### Imports ####
  library(plyr)
  library(rvest)
  library(magrittr)
  library(lubridate)
  library(stringr)
  library(dplyr)
  
  ### Variables ####
  
  Event_ID = ID
  
  
  ### Download Page ####
  
  URL_Source = paste0("http://www.owgr.com/en/Events/EventResult.aspx?eventid=",Event_ID)
  
  HTML_Source <- read_html(URL_Source)
  
  
  ### Import Elements ####
  
  # Event Wide Data
  Event_Name <-
    html_node(HTML_Source, "#event_result_table h2") %>% html_text()
  Status_Scrape <- "Tournament ID Not Valid"
  
  Event_Data <- NA
  Player_Data <- NA
  
  if (Event_Name != "") {
    Status_Scrape <-
      "No Player Information for this tournament available"
    
    Event_Tour <-
      html_nodes(HTML_Source, ".event_logo") %>% html_attr("src") %>%
      gsub("^.*/","",.) %>% gsub("\\.a.*$","",.)
    
    Event_Tour <- revalue(
      Event_Tour, c(
        "WGC" = "WGC",
        "JapanGolfTour" = "Japan Golf Tour",
        "Major Combo 2" = "Major Championship",
        "asian_tour1" = "Asian Tour",
        "PGA" = "PGA Tour",
        "sunshine_tour" = "Sunshine Tour",
        "Korean Golf Tour 2" = "Korean Golf Tour",
        "oneAsia" = "OneAsia Tour",
        "PGATOUR_Australasia" = "PGA Tour Australasia",
        "AUS Logo2" = "PGA Tour Australasia",
        "european_tour" = "European Tour",
        "european_challenge" = "European Challenge Tour",
        "webcom" = "Web.com Tour",
        "pga_latinoamerica" = "PGA Tour Latinoamerica",
        "asian_development" = "Asian Development Tour",
        "PGA Tour China Logo" = "PGA Tour China",
        "PGA Tour Canada2" = "PGA Tour Canada",
        "CAN3" = "PGA Tour Canada",
        "EPT Logo" = "EuroPro Tour" ,
        "NGL Logo" = "Nordic Golf League",
        "PGT Logo" = "ProGolf Tour",
        "Alps Golf Tour" = "Alps Golf Tour",
        "MGT Logo 2" = "MENA Tour"
      ),
      warn_missing = FALSE
    )
    
    Event_Tour_1 = Event_Tour[1]
    Event_Tour_2 = Event_Tour[2]
    Event_Tour_3 = Event_Tour[3]
    Event_Date <-
      html_node(HTML_Source, "#event_result_table time") %>% html_text() %>% dmy() %>% as.Date()
    
    Event_Data <-
      cbind.data.frame(
        Event_Name,Event_ID,Event_Date,Event_Tour_1,Event_Tour_2,Event_Tour_3,stringsAsFactors = FALSE
      )
    
    # Is this a completed event, coming soon, or far in the future?
    First_Col <-
      html_nodes(HTML_Source, "#event_result_table th") %>% html_text()
    
    if (length(First_Col) == 0) {
      First_Col <- 0
    } else {
      First_Col <-
        First_Col %>% extract2(1) %>% gsub("[\r\n]", "",.) %>% str_trim()
    }
    
    
    if (First_Col == "Pos") {
      # Player Results if the tournament is completed
      Status_Scrape <- "Tournament Results Collected"
      
      Pos <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(1)") %>% html_text()
      Pos_Num <- as.integer(gsub("\\D","",Pos))
      Country <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent .flag") %>% html_attr("alt")
      Player_Name <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent a") %>% html_text()
      Player_ID <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent a") %>% html_attr("href") %>% gsub("^.*=","",.) %>% as.integer()
      Round_1 <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(4)") %>% html_text() %>% as.integer()
      Round_2 <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(5)") %>% html_text() %>% as.integer()
      Round_3 <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(6)") %>% html_text() %>% as.integer()
      Round_4 <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(7)") %>% html_text() %>% as.integer()
      Total <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(8)") %>% html_text() %>% as.integer()
      WGR_Pts <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(9)") %>% html_text() %>% as.integer()
      
      # Combine into data frame
      Player_Data_Raw <-
        cbind.data.frame(
          Player_Name,Player_ID,Country,Pos,Pos_Num,Round_1,Round_2,Round_3,Round_4,Total,WGR_Pts
        )
      remove(
        Player_Name,Player_ID,Country,Pos,Pos_Num,Round_1,Round_2,Round_3,Round_4,Total,WGR_Pts
      )
      
      if (max(Player_Data_Raw$Total) == 0) {
        Status_Scrape <- "No Tournament Scores Available"
        
        Player_Data <-
          Player_Data_Raw[,c("Player_Name","Player_ID","Country","Pos","Pos_Num","WGR_Pts")]
        
      } else {
        # Reshame data into tall form, with one row per round
        Player_Data <-
          reshape(
            Player_Data_Raw, varying = c("Round_1","Round_2","Round_3","Round_4"), timevar = "Round_Num",
            sep = "_", direction = "long"
          )
        names(Player_Data)[names(Player_Data) == "Round"] <- "Score"
        Player_Data <- subset(Player_Data,select = -c(id))
      }
      
      #Merge in Tournament Information
      Player_Data <- mutate(
        Player_Data,
        Event_Name = Event_Name,
        Event_ID = Event_ID,
        Event_Date = as.Date(Event_Date),
        Event_Tour_1 = Event_Tour_1,
        Event_Tour_2 = Event_Tour_2,
        Event_Tour_3 = Event_Tour_3
      )
      
      
    }
    
    if (First_Col == "World Ranking") {
      # Players in the tournament if the tournament is coming soon
      Status_Scrape <- "Upcoming Tournament Field Collected"
      
      WGR_Rank <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent td:nth-child(1)") %>% html_text()
      Country <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent .flag") %>% html_attr("alt")
      Player_Name <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent a") %>% html_text()
      Player_ID <-
        html_nodes(HTML_Source, "#phmaincontent_0_ctl00_PanelCurrentEvent a") %>% html_attr("href") %>% gsub("^.*=","",.) %>% as.integer()
      
      Player_Data <-
        cbind.data.frame(Player_Name,Player_ID,Country,WGR_Rank)
      remove(Player_Name,Player_ID,Country,WGR_Rank)
      
      #Merge in Tournament Information
      Player_Data <- mutate(
        Player_Data,
        Event_Name = Event_Name,
        Event_ID = Event_ID,
        Event_Date = as.Date(Event_Date),
        Event_Tour_1 = Event_Tour_1,
        Event_Tour_2 = Event_Tour_2,
        Event_Tour_3 = Event_Tour_3
      )
      
    }
  }
  
  Status <- cbind.data.frame(Event_ID,Status_Scrape)
  
  Status$Scrape_Date <- Sys.Date()
  Event_Data$Scrape_Date <- Sys.Date()
  Player_Data$Scrape_Date <- Sys.Date()
  
  
  #Here's what to return
  Output <- list(Status_Scrape,Status,Event_Data,Player_Data)
  
}

### Try out the Function ####


# Result <- Import_Tourney_Results(6085)
# str(Result)


### IDs to Scrape ####


# Previous Scrape Output:
Player_Results_Raw <- 
  read.csv("Data/Player_Results_RVest.csv", stringsAsFactors = FALSE)
Tournament_Info_Raw <- read.csv("Data/Tournament_Info_RVest.csv", stringsAsFactors = FALSE)
Scrape_Status_Raw <- read.csv("Data/Scrape_Status_RVest.csv", stringsAsFactors = FALSE)
# Upcoming_Fields <- read.csv("Data/Upcoming_Fields_RVest.csv")


Player_Results <- Player_Results_Raw[!is.na(Player_Results_Raw$Event_Date),]
Player_Results$Event_Date <- as.Date(Player_Results$Event_Date)
Player_Results$Scrape_Date <- as.Date(Player_Results$Scrape_Date)

Tournament_Info <- Tournament_Info_Raw[!is.na(Tournament_Info_Raw$Event_Date),]
Tournament_Info$Event_Date <- as.Date(Tournament_Info$Event_Date)
Tournament_Info$Scrape_Date <- as.Date(Tournament_Info$Scrape_Date)

Scrape_Status <- Scrape_Status_Raw[,c("Event_ID","Status_Scrape","Scrape_Date")]
Scrape_Status$Scrape_Date <- as.Date(Scrape_Status$Scrape_Date)



# Rescrape last 2 weeks of data:

Last_Date <- max(as.Date(Player_Results$Event_Date)) - 15
Last_ID <- max(Tournament_Info[as.Date(Tournament_Info$Event_Date) < Last_Date,]$Event_ID)

# Grab future data as well
Future_ID <- as.integer(Sys.Date() - Last_Date + 14) * 2 + Last_ID


Begin_ID <- as.integer(Last_ID + 1)
End_ID <- as.integer(Future_ID)


# This is only needed if this is a new scrape:
# Scrape_Status <- data.frame()
# Player_Results <- data.frame()
# Tournament_Info <- data.frame()

Upcoming_Fields <- data.frame()


### Clear out results to be updated ####

Player_Results <- Player_Results[Player_Results$Event_ID < Begin_ID,]
Tournament_Info <- Tournament_Info[Tournament_Info$Event_ID < Begin_ID,]
Scrape_Status <- Scrape_Status[Scrape_Status$Event_ID < Begin_ID,]


### Loop Extracting Data ####

for (ID in Begin_ID:End_ID) {
  Output_Data <- Import_Tourney_Results(ID)   # Scrape the given ID
  
  
  # Compile the status of the scraping
  if (nrow(Scrape_Status) == 0) {
    Scrape_Status <- as.data.frame(Output_Data[[2]])
    
  } else {
    Scrape_Status <-
      rbind.data.frame(Scrape_Status,as.data.frame(Output_Data[[2]]))
  }
  
  
  # If the ID is valid, compile the event information
  if (!Output_Data[[1]] == "Tournament ID Not Valid") {
    if (nrow(Tournament_Info) == 0) {
      Tournament_Info <- as.data.frame(Output_Data[[3]])
      
    } else {
      Tournament_Info <-
        rbind.data.frame(Tournament_Info,as.data.frame(Output_Data[[3]]))
    }
    
  }
  
  
  # If the event is completed, collect the results
  if (Output_Data[[1]] == "Tournament Results Collected") {
    if (nrow(Player_Results) == 0) {
      Player_Results <- as.data.frame(Output_Data[[4]])
      
    } else {
      Player_Results <-
        rbind.data.frame(Player_Results,as.data.frame(Output_Data[[4]]))
    }
    
  }
  
  
  #If the event is upcoming, collect the fields
  if (Output_Data[[1]] == "Upcoming Tournament Field Collected") {
    if (nrow(Upcoming_Fields) == 0) {
      Upcoming_Fields <- as.data.frame(Output_Data[[4]])
      
    } else {
      Upcoming_Fields <-
        rbind.data.frame(Upcoming_Fields,as.data.frame(Output_Data[[4]]))
    }
    
  }
  
 
  
   
  #Progress Indicator:
  cat((ID - Begin_ID + 1) / (End_ID - Begin_ID + 1) * 100,"%  --  ID",ID,"of",Begin_ID,"to",End_ID,"\n"
  )
  
  # Sys.sleep(1)  # Sleep the loop for 1 second
}

#
# library(microbenchmark)
# times <- microbenchmark()


### Filter out unnecessary player result data ####
Player_Results <- Player_Results %>% 
  filter(Score > 55 & Score < 125 & Pos != "WD" & Pos != "DQ" )

Scrape_Status <- merge(Scrape_Status,Tournament_Info[,c("Event_ID","Event_Name","Event_Date")],all.x = TRUE)

###  Output CSVs ####

write.csv(
  Player_Results,file = (
    "Data/Player_Results_RVest.csv"
  ), row.names = FALSE
)

write.csv(
  Tournament_Info,file = (
    "Data/Tournament_Info_RVest.csv"
  ), row.names = FALSE
)
write.csv(
  Scrape_Status,file = (
    "Data/Scrape_Status_RVest.csv"
  ), row.names = FALSE
)
write.csv(
  Upcoming_Fields,file = (
    "Data/Upcoming_Fields_RVest.csv"
  ), row.names = FALSE
)



###  Import Current OWGR Rankings ####

library(plyr)
library(rvest)
library(magrittr)
library(lubridate)
library(stringr)
library(dplyr)


URL_Source = "http://www.owgr.com/ranking?pageNo=1&pageSize=All&country=All"
HTML_Source <- read_html(URL_Source)

Ranking_Date <-
  html_node(HTML_Source, "#ranking_table .sub_header") %>%
  html_text() %>% dmy() %>% as.Date()

# Player Data
OWGR_Rank <-
  html_nodes(HTML_Source, "#ranking_table td:nth-child(1)") %>%
  html_text() %>% as.integer()
OWGR_Rank_Last_Wk <-
  html_nodes(HTML_Source, "#ranking_table td:nth-child(2)") %>%
  html_text() %>% as.integer()
OWGR_Rank_Last_Yr <-
  html_nodes(HTML_Source, "#ranking_table td:nth-child(3)") %>%
  html_text() %>% as.integer()
Player_Country <-
  html_nodes(HTML_Source, "#ranking_table img.flag") %>% html_attr("alt")
Player_Name <-
  html_nodes(HTML_Source, "#ranking_table td.name") %>% html_text()
Player_ID <-
  html_nodes(HTML_Source, "#ranking_table .name a") %>% html_attr("href") %>% gsub("^.*=","",.) %>% as.integer()
Player_Avg_OWGR_Pts <-
  html_nodes(HTML_Source, "#ranking_table td:nth-child(6)") %>% html_text() %>% as.numeric()

Player_OWGR_Ranking <-
  cbind.data.frame(
    OWGR_Rank,OWGR_Rank_Last_Wk,OWGR_Rank_Last_Yr,Player_Country,
    Player_Name,Player_ID,Player_Avg_OWGR_Pts
  )
Player_OWGR_Ranking$OWGR_Rank_Date <- Ranking_Date
remove(
  OWGR_Rank,OWGR_Rank_Last_Wk,OWGR_Rank_Last_Yr,Player_Country,
  Player_Name,Player_ID,Player_Avg_OWGR_Pts
)

write.csv(
  Player_OWGR_Ranking,file = (
    "Data/Player_OWGR_Ranking_RVest.csv"
  ), row.names = FALSE
)

### Add in new OWGR to the Historic OWGR File


OWGR_History <- list.files(path="Data/", pattern="^Player_OWGR_Hist.+\\.csv$") %>%
  paste("Data/",., sep="") %>%
  lapply(., read.csv, stringsAsFactors = FALSE) %>%
  bind_rows() %>% transform(OWGR_Rank_Date = as.Date(OWGR_Rank_Date))


# Player_OWGR_Ranking <-
# read.csv(("Data/Player_OWGR_Ranking_RVest.csv"), stringsAsFactors = FALSE)

# OWGR_History <- rename(OWGR_History,
#                       OWGR_Rank = rank,
#                       OWGR_Rank_Date = week)

if (max(OWGR_History$OWGR_Rank_Date[!is.na(OWGR_History$OWGR_Rank_Date)])< max(Player_OWGR_Ranking$OWGR_Rank_Date)) {
  OWGR_Hist_Combined <-
    merge(OWGR_History,Player_OWGR_Ranking,all = TRUE)
  
    OWGR_History_Split <- split(OWGR_Hist_Combined, year(OWGR_Hist_Combined$OWGR_Rank_Date))
    
    for (i in seq_along(OWGR_History_Split)) {
      filename = paste("Data/Player_OWGR_Hist_",names(OWGR_History_Split)[i], ".csv", sep="")
      write.csv(OWGR_History_Split[[i]], filename, row.names = FALSE)
    }
}




