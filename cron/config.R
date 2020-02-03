library(ReddRankings)
library(DBI)

iterations <- 50000

results <- getResults("2020")

con <- dbConnect(RMariaDB::MariaDB(),
                 "reddrankings",
                 username = "tredd",
                 password = Sys.getenv("RDS_PASSWORD"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")

