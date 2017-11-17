

###  This file is for running a linear regression on tall golf data  ###

#  By Daniel Myers

###

### Libraries ####

library(plyr)
library(rvest)
library(magrittr)
library(lubridate)
library(stringr)
library(dplyr)


# ### Primary Variables to Adjust ####
# 
# Input_Date <-  # Sys.Date()
#    as.Date("2010-03-29")   # This regression will do the XX years prior to this date
# Split_Type <-
#   "Before"                 # "Before" or "After" .. This also controls weighting type (exponential for before, Step after)
# 


Golf_Ratings_Regression <- function(Split_Date = Sys.Date(),Split_Type = "Before"){
  
Split_Date <- as.Date(Split_Date)

Rating_Date <- Split_Date + (4 - wday(Split_Date))   # Wednesday of the rated week

Prev_Rating_Date <- Rating_Date -7         # Wednesday prior to rated week

Exponential_Decay_Constant <- 0.98
Step_Weights <-
  c(1,0.25,0.1)           # Vector of 3 numbers, first for end of split season, then for each of following seasons


Min_Player_Rounds <-
  50                     # The sufficient number of rounds by a player to include that player (50)
Min_Player_Rounds_Last_Yr <-
  20                      # The sufficient number of rounds by a player to include that player (25)

Minimum_Player_In_Round <-
  15                      # The minimum number of players present in a round to include it (17)

Save_Location <-  paste0("Output/Archive/Golf_Ratings_",Rating_Date,".csv")

Previous_Ratings <-   paste0("Output/Archive/Golf_Ratings_",Prev_Rating_Date,".csv")


# Output the Rating_Date
cat("The current rating is for the date", format(Rating_Date, "%Y-%m-%d"),".\n")


### Import from CSV File ######



# Results_Source_old <- read.csv(("Data/Tournament_Results_Since_2007_Results_for_LM.csv"))
Results_Source <- read.csv(("Data/Player_Results_RVest.csv"))

Results_Source$Round_ID <-
  paste(Results_Source$Event_ID,Results_Source$Round_Num,sep = "_")
Results_Source$Tour_Name <- Results_Source$Event_Tour_1

#Results_Source <-
#  fread("Data/Tournament_Results_Since_2007_Results_for_LM.csv.gz")
#Results_Source <- as.data.frame(Results_Source)

Results_Source$Event_Date <- as.Date(Results_Source$Event_Date)

Factor_Cols <- c("Player_Name","Player_ID","Country","Event_Name","Event_ID","Round_ID","Tour_Name")
Results_Source[Factor_Cols] <-
  lapply(Results_Source[Factor_Cols],as.factor)
remove(Factor_Cols)



#View(Results_Source)
str(Results_Source)


### Weeks before beginning of 2016 ####

Results_Source$Week_Delta <-
  as.integer(round(as.integer(
    as.Date("2016-01-01") - Results_Source$Event_Date
  ) / 7))
# head(Results_Source)


### Reduced Regression Data ####

Results <-
  subset(
    Results_Source, select = c(
      Player_Name,Player_ID,Round_ID,Score,Event_Date, Event_Name,Event_ID,Tour_Name, Week_Delta, Country
    )
  )

remove(Results_Source)

str(Results)


### Choose Split Date and Intervals ####

Interval_Before <-
  as.integer(as.Date("2010-07-14")) - as.integer(as.Date("2006-07-14"))  # Prior 4 years
Interval_After <-
  as.integer(as.Date("2012-10-10")) - as.integer(as.Date("2010-07-14"))  # End of season and following 2 seasons


Target_Subset <- if (Split_Type == "Before") {
  subset(Results, (
    Event_Date < Rating_Date &
      Event_Date >= as.Date(Rating_Date - Interval_Before)
  ))
} else if (Split_Type == "After") {
  subset(Results, (Event_Date >= Rating_Date &
                     Event_Date < as.Date(Rating_Date + Interval_After)))
} else {
  "Error"
}



### Choose Weighting function for data ####

# Target_Subset$Weight <- ifelse(Target_Subset$Event_Date < as.Date("2010-10-01"),1,0.20)

Target_Subset$Weight <- if (Split_Type == "Before") {
  (Exponential_Decay_Constant ^ (Target_Subset$Week_Delta))*10
} else {
  ifelse ((Target_Subset$Event_Date < as.Date(Rating_Date + 88)),
          Step_Weights[1],ifelse((
            Target_Subset$Event_Date >= as.Date(Rating_Date + 88) &
              Target_Subset$Event_Date < as.Date(Rating_Date + 88 + 365)
          ),Step_Weights[2],Step_Weights[3]
          )
  )
}


Max_Weight <- max(Target_Subset$Weight)

Target_Subset$Weight <- Target_Subset$Weight/Max_Weight



### Functions for Removing Rare Players/Rounds ####

library(dplyr)

Count_Rounds_Players <- function (Data) {
  library(dplyr)
  Player_ID_Group <- group_by(Data, Player_ID)
  Number_Rounds_Target <- summarise(Player_ID_Group,
                                    Rounds_Player = length(Score))
  
  Data <-
    merge(Data,Number_Rounds_Target,by = c("Player_ID"))
  
  # Number of Recent Rounds
  Data_Recent <-   subset(Data, (Event_Date >= as.Date(Rating_Date - 365)&
                                            Event_Date <= as.Date(Rating_Date + 365)))
  
  Player_ID_Group <- group_by(Data_Recent,Player_ID)
  Rounds_Last_Year <- summarise(Player_ID_Group,
                                Rounds_Last_Year = length(Weight))
  Data <- merge(Data,Rounds_Last_Year, by = c("Player_ID"), all.x=TRUE)
  Data$Rounds_Last_Year[is.na(Data$Rounds_Last_Year)] <- 0
  
  return (Data)
}

Count_Rounds_Tournaments <- function (Data) {
  library(dplyr)
  Round_ID_Group <- group_by(Data,Round_ID)
  Number_Rounds_Target <- summarise(Round_ID_Group,
                                    Rounds_Round_ID = length(Score))
  Data <-
    merge(Data,Number_Rounds_Target,by = c("Round_ID"))
  
  
  return (Data)
}

Remove_Rare_Data <-
  function (Results_Sample) {
    Results_Sample <- Count_Rounds_Players(Results_Sample)
    Results_Sample <-
      subset(Results_Sample, Rounds_Player > Min_Player_Rounds | Rounds_Last_Year > Min_Player_Rounds_Last_Yr )
    
    Results_Sample <- droplevels(Results_Sample)
    
    
    Results_Sample <- Count_Rounds_Tournaments(Results_Sample)
    Results_Sample <-
      subset(Results_Sample, Rounds_Round_ID > Minimum_Player_In_Round)
    
    Results_Sample <- droplevels(Results_Sample)
    
    Results_Sample <- subset(
      Results_Sample,
      select = -c(
        Rounds_Player,Rounds_Last_Year,Rounds_Round_ID
      )
    )
    
    Results_Sample <- Count_Rounds_Players(Results_Sample)
    Results_Sample <-
      subset(Results_Sample, Rounds_Player > Min_Player_Rounds | Rounds_Last_Year > Min_Player_Rounds_Last_Yr)
    
    Results_Sample <- droplevels(Results_Sample)
    
    return (Results_Sample)
    
  }


### Choose what players & Rounds to include in regression  ####


Target_Subset <-
  Remove_Rare_Data (Target_Subset)

str(Target_Subset)




### Function Identifying "Primary Players"  ####


Primary_Tour_Players <- function (Results_Sample) {
  library(dplyr)
  
  Primary_Tours <-
    c("European Tour","Major Championship","PGA Tour", "WGC")
  
  Results_Sample$Primary_Round <-
    1 - as.integer(is.na((
      match(Results_Sample$Tour_Name,Primary_Tours)
    )))
  
  Primary_Round_Group <-
    group_by(Results_Sample,Player_Name, Player_ID,Rounds_Player)
  Number_Rounds_Primary <- summarise (Primary_Round_Group,
                                      Sum_Primary = sum(Primary_Round*Weight),Sum_Weight = sum(Weight))
  
  Number_Rounds_Primary$Primary_Player <-
    round(Number_Rounds_Primary$Sum_Primary / Number_Rounds_Primary$Sum_Weight)
  
  Results_Sample <-
    merge(Results_Sample,subset(Number_Rounds_Primary, select = c(Player_ID,Primary_Player)),by = c("Player_ID"))
  return (Results_Sample)
}


Target_Subset <- Primary_Tour_Players(Target_Subset)





###  Player Information Developed ####

Player_Info <-
  subset(
    Target_Subset, select = c(
      Player_ID,Player_Name,Rounds_Player,Rounds_Last_Year,Primary_Player,Country,Tour_Name,Weight,Event_Date,Week_Delta
    )
  )

# Find out most common tour and total weight over entire ratings interval
Player_ID_Group <- group_by(Player_Info,Player_ID)
Common_Tour <- summarise(Player_ID_Group,
                         Common_Tour = names(which.max(table(Tour_Name))))
Player_Info <- merge(Player_Info,Common_Tour, by = c("Player_ID"))
Weight_Sums <- summarise(Player_ID_Group,
                         Weight_Sum = sum(Weight),Center_Wt = (sum(Weight*Week_Delta)/sum(Weight)))
Weight_Sums$Center_Wt <- (Exponential_Decay_Constant ^ (Weight_Sums$Center_Wt))/Max_Weight*10
Player_Info <- merge(Player_Info,Weight_Sums, by = c("Player_ID"))
remove(Common_Tour)
remove(Weight_Sums)
remove(Player_ID_Group)

# Find out most recent info

# Recent Tour
Player_Info_2 <-   subset(Player_Info, Tour_Name != "Major Championship" & 
                            Tour_Name != "WGC" & 
                            Event_Date >= as.Date(Rating_Date - 365) &
                            Event_Date <= as.Date(Rating_Date + 365))
Player_Info_2$Recent_Tour_Wt <- 0.90^Player_Info_2$Week_Delta 


Player_ID_Group <- group_by(Player_Info_2,Player_Name,Player_ID,Tour_Name)
Recent_Tour <- summarise(Player_ID_Group,
                         Tour_Wt = sum(Recent_Tour_Wt)) %>%
  filter(Tour_Wt == max(Tour_Wt)) %>% 
  rename(Recent_Tour=Tour_Name) %>%
  select(Player_ID,Recent_Tour)

Recent_Tour$Player_Name <- NULL


Player_Info <- merge(Player_Info,Recent_Tour, by = c("Player_ID"), all.x=TRUE)

remove(Player_Info_2)
remove(Recent_Tour)
remove(Player_ID_Group)



Player_Info <- subset(Player_Info, select = -c(Tour_Name,Weight,Event_Date,Week_Delta))
Player_Info <- Player_Info[!duplicated(Player_Info$Player_ID),]
Player_Info$Recent_Tour[is.na(Player_Info$Recent_Tour)] <- "None"
Player_Info$Recent_Tour[Player_Info$Rounds_Last_Year<17] <- "None"

str(Player_Info)


### Set a prior  - Does not work currently ####

# Target_Prior <- subset(Number_Rounds_Primary, Primary_Player > 0 )
# Target_Prior$Weight <- 0.2
# Target_Prior$Round_ID <- NA
# Target_Prior$Score <- 0


###  The BigLM Regression Function ####

BigLM_Golf_Regression <- function (Golf_Data) {
  Sys.time()
  
  library(biglm)
  
  chunksize <- 1000                           #Set Chunk size
  length_Target <-
    NROW(Golf_Data)           #Get length of the dataset
  
  
  Chunk_1 <-
    Golf_Data[1:chunksize,]       #Get the starting chunk of data
  # levels(Chunk_1$Player_ID) <- c(Factor_Player_ID)
  # str(Chunk_1)
  
  round_down <- function(x,to = 10)
  {
    to * (x %/% to - as.logical(x %% to))
  }
  
  End_of_Chunks <-
    (round_down(length_Target,chunksize))       #Identify the end of the chunks
  
  Chunk_Last <-
    Golf_Data[End_of_Chunks + 1:length_Target,]        #Get the last, odd chunk
  
  Begin_Time <- Sys.time()
  cat("Regression started at ",format(Begin_Time, "%b %d %H:%M:%S"),"\n")
  
  fit_Target <-
    biglm(Score ~ Player_ID + Round_ID, data = Chunk_1, weights = ~ Weight)
  
  Current_Time <- Sys.time()
  Begin_Time <- Sys.time()
  
  for (i in seq(chunksize,End_of_Chunks,chunksize)) {
    # Update regression
    fit_Target = update(fit_Target, moredata = Golf_Data[(i + 1):(i + chunksize),])
    
    # Block tracking progress and predicting finish time
    Percent_Done <- round((i)/length_Target*100,2)/100  
    Prev_Time <- Current_Time
    Current_Time <- Sys.time()
    Current_Interval <- (Current_Time - Prev_Time)
    Average_Interval <- (Current_Time - Begin_Time)*chunksize/(i)
    Interval_Slope <- (Current_Interval - Average_Interval)/(Percent_Done/2)
    Future_Avg_Interval <- Average_Interval + (1-Percent_Done/2)*Interval_Slope
    Finish_Time <- Current_Time + Future_Avg_Interval*(length_Target-(i))/chunksize
    
    # Text output of progress
    cat(i + chunksize,"of", length_Target, "- (", Percent_Done*100,
        ")%, Time is",
        format(Current_Time, "%b %d %H:%M:%S"),
        " Estimated finish at",
        format(Finish_Time, "%b %d %H:%M:%S"),"\n")
  }

  fit_Target = update(fit_Target, moredata = Chunk_Last)
  
  cat("Regression ended at ",format(Sys.time(), "%b %d %H:%M:%S"),"\n")
  
  # ### Store Levels of Factors
  # Factor_Round_ID <- levels(factor(Golf_Data$Round_ID))
  #
  # Target_Prior$Round_ID <- as.factor(Target_Prior$Round_ID)
  # levels(Target_Prior$Round_ID) <- Factor_Round_ID
  #
  # fit_Target=update(fit_Target, moredata=Target_Prior)
  
  
  return (fit_Target)
}


### Call Regression and Clean Up Results ####

BigLM_Fit_Results <- BigLM_Golf_Regression (Target_Subset)


library(broom)
Target_Results <- tidy(BigLM_Fit_Results)
Target_Results_Rounds <-
  Target_Results[grep("Round_ID",Target_Results$term),]
Target_Results_Players <-
  Target_Results[grep("Player_ID",Target_Results$term),]
Target_Results_Rounds$Round_ID <-
  as.factor((gsub("^.*ID","",Target_Results_Rounds$term)))
Target_Results_Players$Player_ID <-
  as.factor((gsub("^.*ID","",Target_Results_Players$term)))

Intercept_Results <- Target_Results[1,2]



Target_Results_Players <-
  merge(Target_Results_Players,Player_Info,by = c("Player_ID"))
Target_Results_Players <-
  Target_Results_Players[setdiff(names(Target_Results_Players), "term")]




### Section calculating and incorporating standard deviations ####


Center_Estimates <- function (Data){
  Primary_Players <- Data[Data$Primary_Player==1,]
  Avg_estimate <- mean(Primary_Players$estimate)
  Data$estimate <- Data$estimate-Avg_estimate
  return (Data)
}

Target_Results_Players <- Center_Estimates(Target_Results_Players)







Target_Subset <-
  merge(Target_Subset,Target_Results_Players[,c("Player_ID","estimate")], by = c("Player_ID"))
names(Target_Subset)[names(Target_Subset) == "estimate"] <-
  "Player_Est"

Target_Subset <-
  merge(Target_Subset,Target_Results_Rounds[,c("Round_ID","estimate")], by = c("Round_ID"))
names(Target_Subset)[names(Target_Subset) == "estimate"] <-
  "Round_Est"

Target_Subset$Predicted_Score <-
  Target_Subset$Player_Est + Target_Subset$Round_Est + Intercept_Results
Target_Subset$Residual <-
  Target_Subset$Score - Target_Subset$Predicted_Score



library(Hmisc)
library(dplyr)
Player_ID_Group <- group_by(Target_Subset,Player_ID)

Stdevs <- summarise(Player_ID_Group,
                    Sample_Stdev = sqrt(wtd.var(Residual,Weight *
                                                  10000000)))
Target_Results_Players <-
  merge(Target_Results_Players,Stdevs, by = c("Player_ID"))



Round_ID_Group <- group_by(Target_Subset,Round_ID)

Round_Strength <- summarise(Round_ID_Group,
                            Avg_Player = mean(Player_Est))

Target_Subset_2 <- merge(Target_Subset,Round_Strength, by = c("Round_ID"))

Player_ID_Group_2 <- group_by(Target_Subset_2,Player_ID)
Avg_Round_SoS <- summarise(Player_ID_Group_2,
                           Avg_SoS = wtd.mean(Avg_Player,Weight * 1000000))

Target_Results_Players <-
  merge(Target_Results_Players,Avg_Round_SoS, by = c("Player_ID"))


remove(Player_ID_Group, Round_ID_Group,Player_ID_Group_2,Target_Subset_2)




### Post_Processing ####


Projection <- function (Data) {
  
  library(dplyr)
  
  # Create 2 Bayesian Priors, one based on average tournament entered, other based on overall player distribution
  # Weight the first by a constant + recency of tournaments * coefficient

  Prior_Tournaments_wt_Const <- 2.0                     # Constant weight
  Prior_Tournaments_wt_Time_Ago <- 0.0                  # Time ago weight
  Prior_Tournaments_wt_Time_Ago_x_Total <- 0.05         # (Time ago * Total observations wt)
  Prior_Tournaments_wt_sqrt_Time_Ago_x_Total <- 0.0     # sqrt(Time ago * Total observations wt)
  
  Prior_Tournaments_Stderr <- 7.0
  
  
  Prior_Players_wt_const <- 1.5
  Prior_Players_Value <- 8.0
  Prior_Players_Stderr <- 0.0
  
  Prior_Stdev_value <- 2.75
  Prior_Stdev_wt_const <- 115
  
  Data$Prior_Tournament_SoS_Weight <-  (Prior_Tournaments_wt_Const 
                                   + Prior_Tournaments_wt_Time_Ago * Data$Center_Wt
                                   + Prior_Tournaments_wt_Time_Ago_x_Total * (Data$Center_Wt * Data$Weight_Sum)
                                   + Prior_Tournaments_wt_sqrt_Time_Ago_x_Total * sqrt(Data$Center_Wt * Data$Weight_Sum))
  
  Data$Total_Weight <-
    (Data$Weight_Sum 
     +  Data$Prior_Tournament_SoS_Weight
     +  Prior_Players_wt_const
    )  
  
  Data$Projected_Rating <-
    (
      (Data$estimate * Data$Weight_Sum 
       + Data$Avg_SoS * Data$Prior_Tournament_SoS_Weight
       + Prior_Players_Value * Prior_Players_wt_const
       ) / 
        Data$Total_Weight
    )
  
  
  Data$Expected_Stdev <-
    (Data$Sample_Stdev * Data$Weight_Sum + Prior_Stdev_value * Prior_Stdev_wt_const) / (Data$Weight_Sum +
                                                                                          Prior_Stdev_wt_const)
  
  # Generate a forward standard error
  
  Data$Rating_StdErr <- sqrt(
    (Prior_Players_wt_const/Data$Total_Weight)^2*(Prior_Players_Stderr)^2
    + (Data$Prior_Tournament_SoS_Weight/Data$Total_Weight)^2*(Prior_Tournaments_Stderr)^2
    + (Data$Weight_Sum/Data$Total_Weight)^2*(Data$std.error)^2
      )
  
  Data$Projected_Stdev <- sqrt(Data$Expected_Stdev^2 + Data$Rating_StdErr^2)
  
  return (Data)
}

Target_Results_Players<- Projection(Target_Results_Players)

Target_Results_Players$Rank <- rank(Target_Results_Players$Projected_Rating)




###  Get Previous Ratings and show delta ####

Prev_Results_Available <- ifelse(file.exists(Previous_Ratings),TRUE,FALSE)

if(Prev_Results_Available) {Previous_Target_Results <- read.csv(file = Previous_Ratings)

Previous_Target_Results <- Previous_Target_Results[,c("Player_ID","Projected_Rating","Rank")]

names(Previous_Target_Results)[names(Previous_Target_Results) == "Projected_Rating"] <-
  "Previous_Rating"

names(Previous_Target_Results)[names(Previous_Target_Results) == "Rank"] <-
  "Prev_Rank"

Target_Results_Players <-
  merge(Target_Results_Players,Previous_Target_Results, by = c("Player_ID"), all.x = TRUE)

Target_Results_Players$Change <- Target_Results_Players$Projected_Rating - Target_Results_Players$Previous_Rating
Target_Results_Players$Rank_Change <- Target_Results_Players$Rank - Target_Results_Players$Prev_Rank
}


###  Import OWGR Ratings

OWGR_Players <- read.csv(("Data/Player_OWGR_History.csv")) %>%
  mutate(Date = as.Date(OWGR_Rank_Date)) %>%
  filter(Date < Rating_Date, Date > Prev_Rating_Date)

Target_Results_Players <- merge(Target_Results_Players,OWGR_Players[c("Player_ID","OWGR_Rank")],all.x = TRUE)


### Rearrange and export results ####

Target_Results_Players$Rating_Date <- Rating_Date

if(Prev_Results_Available) {Target_Results_Players <-
  Target_Results_Players[,c("Rank",
                            "OWGR_Rank",
                            "Player_Name",
                            "Player_ID",
                            "Projected_Rating",
                            "Rating_StdErr",
                            "Projected_Stdev",
                            "Prev_Rank",
                            "Rank_Change",
                            "Previous_Rating",
                            "Change",
                            "Weight_Sum",
                            "Total_Weight",
                            "Rounds_Player",
                            "Avg_SoS",
                            "Center_Wt",
                            # "Player_Avg_OWGR_Pts",
                            "Primary_Player",
                            "Country",
                            "Rounds_Last_Year",
                            "Recent_Tour",
                            "Common_Tour",
                            "estimate",
                            "std.error",
                            "p.value",
                            "Sample_Stdev",
                            "Expected_Stdev",
                            "Rating_Date"
  )]} else {
    Target_Results_Players <-
      Target_Results_Players[,c("Rank",
                                "OWGR_Rank",
                                "Player_Name",
                                "Player_ID",
                                "Projected_Rating",
                                "Rating_StdErr",
                                "Projected_Stdev",
                                "Weight_Sum",
                                "Total_Weight",
                                "Rounds_Player",
                                "Avg_SoS",
                                "Center_Wt",
                                # "Player_Avg_OWGR_Pts",
                                "Primary_Player",
                                "Country",
                                "Rounds_Last_Year",
                                "Recent_Tour",
                                "Common_Tour",
                                "estimate",
                                "std.error",
                                "p.value",
                                "Sample_Stdev",
                                "Expected_Stdev",
                                "Rating_Date"
      )]
    
    
  }






Target_Results_Players <-
  Target_Results_Players[order(Target_Results_Players$Rank),]


write.csv(Target_Results_Players, file = Save_Location, row.names = FALSE)

Current_Rating_Date <- Sys.Date() + (4 - wday(Sys.Date()))   # Wednesday of the current week

if (Rating_Date == Current_Rating_Date) {
  Save_Location_Current <-
    "Output/Golf_Ratings_Current.csv"
  
  Save_Location_Web_Table <-
    "Output/Golf_Ratings_Current_Web_Table.csv"
  
  if(Prev_Results_Available){Website_Results_Table <-
    Target_Results_Players[,c("Rank",
                              "OWGR_Rank",
                              "Player_Name",
                              "Projected_Rating",
                              "Projected_Stdev",
                              "Prev_Rank",
                              "Rank_Change",
                              "Previous_Rating",
                              "Change",
                              "Rounds_Last_Year",
                              "Recent_Tour"
    )] } else{
      Website_Results_Table <-
        Target_Results_Players[,c("Rank",
                                  "OWGR_Rank",
                                  "Player_Name",
                                  "Projected_Rating",
                                  "Projected_Stdev",
                                  "Rounds_Last_Year",
                                  "Recent_Tour"
        )] 
    }
  
  write.csv(Target_Results_Players, file = Save_Location_Current, row.names = FALSE)
  
  write.csv(Website_Results_Table, file = Save_Location_Web_Table, row.names = FALSE) 
  
}

# write.csv(Target_Subset, file = "~/ETC/Sports/Golf/Target_Subset_Before_2010_0.98.csv" , row.names = FALSE)
}



