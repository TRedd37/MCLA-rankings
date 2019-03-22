library(rvest)
library(stringr)

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

plyr::ldply(conference_urls, foo, .id = "Conference") 

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


