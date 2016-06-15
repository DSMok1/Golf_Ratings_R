###

# Scraping the odds

###

### Imports ####
library(plyr)
library(rvest)
library(magrittr)
library(lubridate)
library(stringr)
library(dplyr)


URL_Source = paste0("http://www.oddschecker.com/golf/us-open/winner")

HTML_Source <- read_html(URL_Source)

Player_Name <-
  html_nodes(HTML_Source, ".nm") %>% html_text()
Player_Odds <-
  html_nodes(HTML_Source, "#t1 :nth-child(16)") %>% html_text()

Player_Odds <- Player_Odds[!(Player_Odds=="Sergio Garcia403545454545403540504040454040403339504050393540514950")]

Player_Data <-
  cbind.data.frame(Player_Name,Player_Odds)
