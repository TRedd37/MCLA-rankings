library(rvest)
library(stringr)
library(pool)
library(dplyr)

conference_postfix <- "http://mcla.us/conferences" %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[4:11]

conference_names <- "http://mcla.us/conferences" %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("title") %>%
  .[4:11]

conference_urls <- paste0("http://mcla.us", conference_postfix) %>%
  setNames(conference_postfix %>%
             str_replace("/conference/", "") %>%
             toupper())

team_info <- plyr::ldply(conference_urls, foo, .id = "Conference") 

foo <- function(ll){
  d1 <- ll %>%
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    rename(School = "SCHOOL / TEAM") %>%
    rename(Coach = "HEAD COACH") %>%
    filter(School != Coach) %>%
    mutate(Division = 1)
  d2 <- ll %>%
      read_html() %>%
      html_table() %>%
      .[[2]] %>%
      rename(School = "SCHOOL / TEAM") %>%
      rename(Coach = "HEAD COACH") %>%
      filter(School != Coach) %>%
      mutate(Division = 2)

  output <- d1 %>%
    bind_rows(d2) %>%
    select(School, Division)
  return(output)
}

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "reddrankings",
  username = "shiny",
  password = "guest",
  host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com"
)

pool %>%
  tbl("TeamIDs") %>%
  collect() %>%
  full_join(team_info, by = c(TeamName = "School"), suffix = c(".x", "")) %>%
  select(ID, TeamName, "Conference", "Division") %>%
  rename(id = "ID") %>%
  dbWriteTable(conn = con, name = "tempTable", append = TRUE, row.names = FALSE)

            