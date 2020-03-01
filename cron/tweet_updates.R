library(pool)
library(ReddRankings)
suppressMessages(library(dplyr))

pool <- createAWSConnection()

pool %>%
  tweetTopTeams()

pool %>%
  poolClose()
