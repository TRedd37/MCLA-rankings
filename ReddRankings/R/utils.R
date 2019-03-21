predictGameOutcome <- function(home_team, visiting_team, output, simulations = 10000, neutral = FALSE ){
  alpha = data.frame(alpha = output$alpha)
  home_team_ratings     <- sample_n(output$rankings, simulations, replace = TRUE)[, home_team]
  visiting_team_ratings <- sample_n(output$rankings, simulations, replace = TRUE)[ , visiting_team]
  hfa                   <- sample_n(alpha, simulations, replace = TRUE)
  if(neutral){
    hfa = sample_n(data.frame(dummy = 0), simulations, replace = TRUE)
  }
  
  home_score <- exp(home_team_ratings + hfa)
  visiting_score <- exp(visiting_team_ratings)
  denominator <- home_score + visiting_score
  
  home_team_win_prob <-  home_score / denominator
  simulated_wins <- rbinom(simulations, 1, home_team_win_prob[,1] )
  home_win_probability <- mean(simulated_wins)
  return(home_win_probability)
}

getRecord <- function(team, results){
  home_games <- subset(results, Home.Team == team)
  wins <- sum(home_games$Winning.Team == "Home")
  losses <- sum(home_games$Winning.Team != "Home")
  
  away_games <- subset(results, Away.Team == team)
  wins <-  wins + sum(away_games$Winning.Team == "Visiting")
  losses <- losses + sum(away_games$Winning.Team != "Visiting")
  return(c(Wins = wins, Losses = losses))
}

#' @export
buildRankingsDF <- function(model_output_list, results){
  divisions <- getDivisions()
  
  divisions <- data.frame(School = divisions$D1, 
                          Division = 1, 
                          stringsAsFactors = FALSE) %>%
    bind_rows(data.frame(School = divisions$D2,
                         Division = 2, 
                         stringsAsFactors = FALSE))
  
  model_rankings <- model_output_list %>%
    plyr::ldply( extractRankings) %>%
    group_by(School) %>%
    mutate(Average = mean(Rank),
           AveScore = mean(Score)) %>%
    ungroup() %>%
    select(-Score) %>%
    spread(.id,Rank) %>%
    as.data.frame() %>%
    arrange(Average, desc(AveScore)) %>%
    mutate(ReddRanking = 1:nrow(.)) %>%
    select(-AveScore) %>%
    select(School, ReddRanking, RR_v1, everything(), -Average, Average) %>%
    mutate(School = as.character(School))
  
  record   <- ldply(model_rankings$School,  getRecord, results = results)
  
  rankings <- model_rankings %>%
    bind_cols(record) %>%
    inner_join(divisions, by = "School")
  return(rankings)
}
