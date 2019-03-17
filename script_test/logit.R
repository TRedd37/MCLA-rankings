library(ReddRankings)
library(dplyr)
source("../data_pull.R")
library(RMySQL)

results <- mcla2019todate %>%
  filter(!(Away == "Florida" & Home == "North Florida")) %>%
  filter(!(Away == "Kennesaw State" & Home == "Georgia")) %>%
  getFinishedResults()

iterations <- 50000

model_logit         <- calculateRankings(results, iterations, WF_method = "logit", HFA = FALSE)
model_logit_HFA     <- calculateRankings(results, iterations, WF_method = "logit")


model_list <- list(logit = model_logit,
                   logit_HFA = model_logit_HFA)

con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = Sys.getenv("RDS_PASSWORD"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)