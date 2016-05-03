###

# Learning Sparse Regression

###

### Imports ####

library(glmnet)
library(speedglm)
library(dplyr)
library(magrittr)
library(Matrix)
library(broom)



Player_Data <- read.csv(gzfile("Data/Player_Results_RVest.csv.gz"))

str(Player_Data)

### Clean and Subset Player Data ####


Player_Data <- Player_Data %>% 
  filter(Score > 55 & Score < 125 & Pos != "WD" ) %>% 
  mutate_each(funs(as.Date),Event_Date)%>%
  filter(Event_Date > as.Date("2015-11-01")) %>%
  mutate(Player_ID=paste0("Player_",Player_ID))%>%
  mutate(Event_ID=paste0("Event_",Event_ID))%>%
  mutate(Round_ID=paste0(Event_ID,"_",Round_Num)) %>%
  mutate_each(funs(factor),Player_ID,Event_ID,Round_ID)%>%
  droplevels()
  
str(Player_Data)
  
### Create a Sparse Matrix of Data ####

Stripped_Data <- Player_Data[,c("Score","Player_ID","Round_ID")]
str(Stripped_Data)

### Approach 1 to creating Sparse Matrix ###

# Sparse <- sparse.model.matrix(~ ., data=Stripped_Data, contrasts.arg = lapply(Stripped_Data[,sapply(Stripped_Data, is.factor)], contrasts, contrasts=FALSE))
# head(Sparse)
# str(Sparse)

### Alternate Approach ###

## From stackoverflow.com/questions/17032264/big-matrix-to-run-glmnet

## Creates a matrix using the first column
Sparse_Alt <- sparse.model.matrix(~Stripped_Data[,1]-1)

## Check if the column have more then one level
for (i in 2:ncol(Stripped_Data)) {
  
  ## In the case of more then one level apply dummy coding 
  if (nlevels(Stripped_Data[,i])>1) {
    column <- sparse.model.matrix(~Stripped_Data[,i]-1)
    Sparse_Alt <- cBind(Sparse_Alt, column)
  }
  ## Transform factor to numeric
  else {
    column <- as.numeric(as.factor(Stripped_Data[,i]))
    Sparse_Alt <- cBind(Sparse_Alt, column)
  }

}


summary(Sparse_Alt)
str(Sparse_Alt)
head(Sparse_Alt)

Sparse_Variables <- Sparse_Alt[,-1]
Observed_Scores <- Sparse_Alt[,1]

Duration <- Sys.time()

Test_Model <- speedlm.fit(Observed_Scores,Sparse_Variables,sparse=TRUE, intercept = TRUE, model=TRUE, method="qr",row.chunk = 5000)

Duration <- Sys.time() - Duration
print(Duration)


str(Test_Model)
# Test_Model$coefficients


Output_Model <- as.data.frame(coef(Test_Model))
