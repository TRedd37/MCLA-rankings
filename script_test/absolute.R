library(ReddRankings)
library(dplyr)
source("../data_pull.R")
library(RMySQL)

results <- mcla2019todate %>%
  filter(!(Away == "Florida" & Home == "North Florida")) %>%
  filter(!(Away == "Kennesaw State" & Home == "Georgia")) %>%
  getFinishedResults()

iterations <- 1000

model_output        <- calculateRankings(results, iterations)
model_output_wo_HFA <- calculateRankings(results, iterations, HFA = FALSE)


model_list <- list(absolute_HFA = model_output,
                   absolute = model_output_wo_HFA)

con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = Sys.getenv("RDS_PASSWORD"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)