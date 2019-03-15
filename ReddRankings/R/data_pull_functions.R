
getSchedulePage <- function(i){
  schedule_url  <- paste0("http://mcla.us/schedule/2019?page=", i)
  schedule_html <- htmlParse(schedule_url)
  schedule      <-readHTMLTable(schedule_html, header=TRUE, which=1, stringsAsFactors=FALSE)

  venue_links <- read_html(schedule_url) %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    filter(str_detect(., "venue")) %>%
    rename(VenueURL = ".")

  schedule <- schedule %>%
    bind_cols(venue_links) %>%
    mutate(VenueURL = as.character(VenueURL))
  
  # is this a page of results or future games
  if("Score" %in% names(schedule)){
    schedule <- schedule %>%
      mutate(AwayGoals = str_split(Score, "-", simplify = TRUE)[,1]) %>%
      mutate(AwayGoals = as.numeric(AwayGoals)) %>%
      mutate(HomeGoals = str_split(Score, "-", simplify = TRUE)[,2]) %>%
      mutate(HomeGoals = as.numeric(HomeGoals))
  } else {
    schedule <- schedule %>%
      mutate(AwayGoals = 0) %>%
      mutate(HomeGoals = 0) 
  }
  
  schedule <- schedule %>%
    filter(HomeGoals + AwayGoals != 0)
  
  return(schedule)
}

readVenueTable <- function(venue_URL){
  venue_webpage <-htmlParse(venue_URL)
  table <- readHTMLTable(venue_webpage, header=TRUE, which=1, stringsAsFactors=FALSE)
  return(table)
}


