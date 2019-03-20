#' @export
 
getSchedule <- function(year){
  schedule <- result <- data.frame()
  
  i=1
  while(class(result) != "try-error"){
    result <- try(getSchedulePage(year, i), silent = TRUE)
    if(class(result) != "try-error"){
      schedule <- schedule %>%
        bind_rows(result)
    }
    i <- i + 1
  }
  return(schedule)
}


 
getSchedulePage <- function(year, i){
  schedule_url  <- paste0("http://mcla.us/schedule/", year, "?page=", i)
  schedule_html <- read_html(schedule_url)
  
  gameIsScrimmage <- schedule_html %>%
    html_nodes("table") %>%
    html_nodes("tr") %>%
    .[-1] %>%
    plyr::ldply(rowIsScrimmage) %>%
    rename(isScrimmage = "V1")
  
  venue_links <- schedule_html %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    filter(str_detect(., "venue")) %>%
    rename(VenueURL = ".")

  schedule <- html_table(schedule_html) %>%
    bind_cols(venue_links) %>%
    bind_cols(gameIsScrimmage) %>%
    filter(!isScrimmage) %>%
    mutate(VenueURL = as.character(VenueURL)) %>%
    select(isScrimmage)
  
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


#' @export
getVenuesHomeTeam <- function(schedule, verbose = TRUE){
  web_venues <- unique(schedule$VenueURL)
  
  fields_home_team <- "unknown"
  for(i in 1:length(web_venues)){
    games_table <- readVenueTable(web_venues[i])
    if(is.numeric(games_table) || nrow(games_table) < 5 ){
      if(verbose){
        print(paste("unknown home team - ", web_venues[i]))
      }
      fields_home_team[i] <- "unknown"
    } else {
      fields_home_team[i] <- names(which.max(table(games_table$Home)))
    }
  }
  
  venue_home_team <- data.frame(VenueURL = web_venues, 
                                VenueHomeTeam = fields_home_team, 
                                stringsAsFactors = FALSE)
  return(venue_home_team)
}

readVenueTable <- function(venue_URL){
  venue_webpage <-htmlParse(venue_URL)
  table <- readHTMLTable(venue_webpage, header=TRUE, which=1, 
                         stringsAsFactors=FALSE)
  return(table)
}

getDivisions <- function(){
  teams_html <- htmlParse("http://mcla.us/teams")
  
  d1teams <- readHTMLTable(teams_html, header=FALSE, which=1, stringsAsFactors=FALSE)$V1
  d2teams <- readHTMLTable(teams_html, header=FALSE, which=2, stringsAsFactors=FALSE)$V1
  output <- list(D1 = d1teams, 
                 D2 = d2teams)
  return(output)
}

rowIsScrimmage <- function(row){
  row %>%
    html_nodes("i") %>%
    as.character() %>%
    str_detect("scrimmage") %>%
    any()
}


