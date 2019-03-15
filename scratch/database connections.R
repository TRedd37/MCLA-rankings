library(RMySQL)
library(dplyr)
con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = rstudioapi::askForPassword("Database password"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")

dbCreateTable(con, name = "testTable", fields = c(ID = "int", Metric = "varchar(20)", Value ="float"))

dbCreateTable(con, "iris", iris)
dbAppendTable(con, "iris", iris)

data_to_insert <- data.frame(ID = 1, Metric = "least_squares", Value = 2.5)

dbWriteTable(con, "testTable", data_to_insert, append = TRUE, row.names = FALSE)
dbDisconnect(con)


