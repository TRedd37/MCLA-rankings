library(rvest)
library(ReddRankings)
library(dplyr)
library(stringr)

results <- read.csv("~/Downloads/Scores.csv", header = FALSE)
colnames(results) <- c("Date", "Away.Team", "AwayGoals", "At", "Home.Team", "HomeGoals", "Time")
results <- results %>%
  mutate(AwayGoals = as.numeric(AwayGoals)) %>%
  mutate(HomeGoals = as.numeric(HomeGoals)) %>%
  filter(!is.na(HomeGoals) ) 

head(results)
leastSquaresRankings(results) %>%
  filter(!str_detect(School, "(CO)")) %>%
  filter(!str_detect(School, "(CA)")) %>%
  filter(!str_detect(School, "(OR)")) %>%
  filter(!str_detect(School, "(ID)")) %>%
  filter(!str_detect(School, "(WA)")) %>%
  filter(!str_detect(School, "(MT)")) 


https://utahlax.app/scores

schedule_url  <- "https://utahlax.app/scores"
schedule_html <- read_html(schedule_url)
schedule_html %>%
  html_table()


