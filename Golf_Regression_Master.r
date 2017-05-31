
### Master File for Running Golf Regressions ####

# Source the Linear Regression Function
Source_Regression <- function () {if (!exists("Golf_Ratings_Regression", mode = "function")) {
  source("Linear_Regression_Golf_Local_Year.r")
}
  }

### Primary Variables to Adjust ####

First_Date <-             # Sys.Date()
  as.Date("2017-04-13")   # This regression will do the XX years prior to this date
Last_Date <- as.Date("2017-04-21")

First_Date <- First_Date + (4 - wday(First_Date))   # Wednesday of the current week

Split_Type <-  "Before"    
# "Before" or "After" .. This also controls weighting type (exponential for before, Step after)

### Create a vector of dates to run

Date_List <- as.list(seq(from = First_Date, to = Last_Date, by = 7))
str(Date_List)


# ###  Set up the Parallelization ####
# # DOES NOT SEEM TO WORK PROPERLY
# require(parallel)
# 
# # Calculate the number of cores
# no_cores <- detectCores() - 4
# 
# # Initiate cluster
# cl <- makeCluster(no_cores,outfile="")
# 
# 
# ### Run the List of Dates ####
# 
# # # Add the needed packages to the cluster environments
# # clusterEvalQ(cl, {
# #   library(plyr)
# #   library(rvest)
# #   library(magrittr)
# #   library(lubridate)
# #   library(stringr)
# #   library(dplyr)
# #   library(biglm)
# #   library(broom)
# #   library(Hmisc)
# # })
# # 
# # # Export the function to the cluster environments
# # clusterExport(cl, list("Golf_Ratings_Regression"))
# 
# clusterCall(cl,Source_Regression)
# 
# Source_Regression()
# 
# parLapply(cl, Date_List,
#           Golf_Ratings_Regression)
# 
# stopCluster(cl)


### Non parallel version ####

#  Add function
# Source_Regression()
# 
# lapply(Date_List, Golf_Ratings_Regression)



####

# Run with the default (current date)

 Source_Regression()
 Golf_Ratings_Regression()
