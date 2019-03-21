iterations <- 50000
library(RMySQL)
library(dplyr)
source("../data_pull.R")

results <- getResults("2019") 

con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = Sys.getenv("RDS_PASSWORD"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")