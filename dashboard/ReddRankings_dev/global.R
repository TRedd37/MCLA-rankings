library(shiny)
library(pool)
library(RMySQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "reddrankings",
  username = "shiny",
  password = "guest",
  host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com"
)

all_conferences <- pool %>%
  tbl("TeamIDs") %>%
  select(Conference) %>%
  filter(!is.na(Conference),
         Conference != "NULL") %>%
  collect() %>%
  unique() %>% 
  as.vector()
