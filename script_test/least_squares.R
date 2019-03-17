library(ReddRankings)
library(dplyr)
source("../data_pull.R")
library(RMySQL)

results <- mcla2019todate %>%
  filter(!(Away == "Florida" & Home == "North Florida")) %>%
  filter(!(Away == "Kennesaw State" & Home == "Georgia")) %>%
  getFinishedResults()

model_least_square  <- leastSquaresRankings(results, FALSE)
model_LS_HFA        <- leastSquaresRankings(results, TRUE)


model_list <- list(scoreBased = model_least_square,
                   scoreBased_HFA = model_LS_HFA)

con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = Sys.getenv("RDS_PASSWORD"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)