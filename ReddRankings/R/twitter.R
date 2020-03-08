
#' @export
twitterHandleChecks <- function(pool, n){
  current_rankings <- getCurrentRankings(pool)
  
  missingD1 <- current_rankings %>%
    findTeamsMissingTwitter(1, n)
  missingD2 <- current_rankings %>%
    findTeamsMissingTwitter(2, n)
  
  DM_message <- paste0("Missing the following Top ", n, " teams' Twitter Handle. D1: ", missingD1, "\n D2: ", missingD2)
  post_message(DM_message, "TRedd")
  
  invisible(NULL)
}

findTeamsMissingTwitter <- function(current_rankings, division, n = 25){
  missing_teams <- current_rankings %>%
    filter(Division == !!division) %>%
    ungroup() %>%
    slice(1:n) %>%
    filter(is.na(TwitterHandle)) %>%
    mutate(output_string =  paste0(TeamName, " (", TeamID, ")")) %>%
    pull(output_string) %>%
    paste(collapse = ', ')
  
  return(missing_teams)
}

#' @export
tweetTopTeams <- function(connection, n = 25){
  current_rankings <- getCurrentRankings(connection)
  
  d1rankings <- current_rankings %>%
    getBaseRankingsStrings(1, n)
  d2rankings <- current_rankings %>%
    getBaseRankingsStrings(2, n)
  
  tweetRankings(d1rankings, 1)
  tweetRankings(d2rankings, 2)
}

getBaseRankingsStrings <- function(current_rankings, division, n){
  base_rankings_strings <- current_rankings %>%
    filter(Division == !!division) %>%
    arrange(ReddRankings) %>%
    mutate(DivisionRank = 1:nrow(.)) %>%
    mutate(TeamNameDisplay = ifelse(!is.na(TwitterHandle), TwitterHandle, TeamName)) %>%
    mutate(RowText = paste(DivisionRank, TeamNameDisplay)) %>%
    slice(1:n) %>%
    pull(RowText)
  
  return(base_rankings_strings)
}

tweetRankings <- function(rankings_string, division){
  tweets <- buildTweetOutput(rankings_string, division, send_dm = FALSE)
  
  suppressMessages(post_tweet(tweets[["first_text"]]))
  Sys.sleep(2)
  last_tweet_id <- get_timeline('reddrankings', 1)$status_id
  suppressMessages(post_tweet(tweets[["reply_test"]], in_reply_to_status_id = last_tweet_id))
  
  invisible(NULL)
}

#' @export
dmRankings <- function(connection, division, n = 25){
  current_rankings <- getCurrentRankings(connection)
  
  d1rankings <- current_rankings %>%
    getBaseRankingsStrings(1, n)
  DM_message <- buildTweetOutput(d1rankings, division, send_dm = TRUE)
  post_message(DM_message, "TRedd")
  
  invisible(NULL)
}

buildTweetOutput <- function(rankings_string, division, send_dm = FALSE){
  cutt_off_date <- (today() - days(1)) %>%
    format(format = "%m/%d")
  base_intro <- paste0("Top 15 Division ", division, " through ", cutt_off_date, ":")
  base_reply <- paste0("16-25 (D", division, ") through ", cutt_off_date, ":")
  
  first_text <- base_intro %>%
    paste(paste(rankings_string[1:15], collapse = '\n'), sep = "\n")
  reply_text <- base_reply %>%
    paste(paste(rankings_string[16:25], collapse = '\n'), sep = "\n")
  
  if(send_dm){
    output <- paste(first_text, reply_text, sep = "\n")
  } else {
    output <- list(first_text = first_text,
                   reply_test = reply_text)
  }
  return(output)
}

