library(ReddRankings)
library(dplyr)
source("../data_pull.R")
library(RMySQL)

results <- mcla2019todate %>%
  filter(!(Away == "Florida" & Home == "North Florida")) %>%
  filter(!(Away == "Kennesaw State" & Home == "Georgia")) %>%
  getFinishedResults()

iterations <- 1000

model_step          <- calculateRankings(results, iterations, WF_method = "step", HFA = FALSE)
model_step_HFA      <- calculateRankings(results, iterations, WF_method = "step")


model_list <- list(step = model_step,
                   step_HFA = model_step_HFA)

con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = Sys.getenv("RDS_PASSWORD"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)