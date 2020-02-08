library(pool)
library(dbplyr)
library(dplyr)

pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "reddrankings",
  host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com",
  username = "tredd",
  password = Sys.getenv("RDS_PASSWORD")
)

base_rankings %>%
  filter(Time1 == today(tz = "America/Denver")) %>%
  select(-Time1)


base_rankings$Time1 %>%
  unique()

teams <- pool %>%
  tbl("TeamIDs") %>%
  collect()


current_rankings <- pool %>%
  tbl("ModelResults") %>%
  collect() %>%
  group_by(ModelID) %>%
  filter(TIME == max(TIME)) %>%
  group_by(TeamID) %>%
  summarize(ranking_average = mean(ModelRank)) %>%
  arrange(ranking_average) %>%
  left_join(teams, by = c(TeamID = "ID"))

findTeamsMissingTwitter <- function(current_rankings, division){
  missing_teams <- current_rankings %>%
    filter(Division == !!division) %>%
    ungroup() %>%
    slice(1:25) %>%
    filter(is.na(TwitterHandle)) %>%
    mutate(output_string =  paste0(TeamName, " (", TeamID, ")")) %>%
    pull(output_string) %>%
    paste(collapse = ', ')
  
  return(missing_teams)
}

missingD1 <- current_rankings %>%
  findTeamsMissingTwitter(1)
missingD2 <- current_rankings %>%
  findTeamsMissingTwitter(2)

DM_message <- paste0("Missing the following teams. D1: ", missingD1, "\n D2: ", missingD2)

