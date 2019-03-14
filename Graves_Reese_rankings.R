
library(lubridate)

calculateRankings <- function(results, iters = 10000, WF_method = "absolute", HFA = TRUE){
  
  results <- results %>%
    mutate(HomeWinFraction = calculcateWinFraction(., WF_method))
  
  A = 15
  B = 10
  S = .15
  candidate_sigma = .1
  
  teams <- unique(c(results$Home.Team, results$Away.Team))
  prior_means <- raw_prior_means[teams]
  prior_means[is.na(prior_means)] <- 0
  names(prior_means) <- teams
  
  rankings <- matrix(0, iters, length(teams))
  colnames(rankings) <- teams
  sigma <- rep(1, iters)
  alpha <- rep(0, iters)
  beta  <- rep(0, iters)
  
  pb <- txtProgressBar(min = 0, max = iters, style = 3)
  
  for(i in 2:iters){
    setTxtProgressBar(pb, i)
    rankings[i, ] <- rankings[i-1, ]
    for(team in teams){
      old_ranking <- rankings[i, team]
      candidate_ranking <- rnorm(1, rankings[i-1, team], sqrt(candidate_sigma))
      g_old <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1],
                          team_name = team, S = S, prior_means)
      rankings[i, team ] <- candidate_ranking
      g_cand <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1],
                           team_name = team, S = S, prior_means)
      log_acceptance_probability 	<- (g_cand - g_old)
      acceptance_value 		<- log(runif(1))
      if(log_acceptance_probability < acceptance_value){
        rankings[i, team ] <- old_ranking
      }
    }
    if(HFA){
      alpha[i] <- rnorm(1, alpha[i-1], sqrt(candidate_sigma))
      
      g_old  <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1],
                           team_name = NULL, S = S, prior_means)
      g_cand <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i],
                           team_name =  NULL, S = S, prior_means)
      log_acceptance_probability 	<- (g_cand - g_old)
      acceptance_value 		<- log(runif(1))
      if(log_acceptance_probability < acceptance_value){
        alpha[i] <- alpha[i-1]
      }
    }
  
    sigma[i] <- 1/rgamma(1, A + (length(teams)/2) , 
                         rate = B + (sum((rankings[i, ] - prior_means[colnames(rankings)]) ^2)/2))
  }
  
  output <- list(rankings = as.data.frame(rankings),
                 alpha = alpha,
                 sigma = sigma,
                 beta = beta)
  return(output)
}



calculateG <- function(results, rankings, sigma, alpha, team_name, S, prior_means){
  if(!is.null(team_name)){
    results <- results %>%
      filter(Home.Team == team_name | Away.Team == team_name)
  } 
  
  hfa <- rep(alpha, nrow(results))
  hfa[(results$Neutral == TRUE)] <- 0

  phi_home    <- exp(rankings[results$Home.Team] + hfa)
  phi_away    <- exp(rankings[results$Away.Team])
  denominator <- phi_home + phi_away
  phi         <- phi_home / denominator
  
  WF <- results$HomeWinFraction
  
  probabilities <- phi^WF * (1 - phi)^(1-WF)

  g <- sum(dnorm(rankings, prior_means[names(rankings)], sqrt(sigma), log = TRUE)) +
    dnorm(alpha, 0, sqrt(S), log = TRUE) +
    sum(log(probabilities)) 
  return(g)
}

predictGameOutcome <- function(home_team, visiting_team, output, simulations = 10000, neutral = FALSE ){
  alpha = data.frame(alpha = output$alpha)
  home_team_ratings     <- dplyr::sample_n(output$rankings, simulations, replace = TRUE)[, home_team]
  visiting_team_ratings <- dplyr::sample_n(output$rankings, simulations, replace = TRUE)[ , visiting_team]
  hfa                   <- dplyr::sample_n(alpha, simulations, replace = TRUE)
  if(neutral){
    hfa = dplyr::sample_n(data.frame(dummy = 0), simulations, replace = TRUE)
  }

  home_score <- exp(home_team_ratings + hfa)
  visiting_score <- exp(visiting_team_ratings)
  denominator <- home_score + visiting_score
  
  home_team_win_prob <-  home_score / denominator
  simulated_wins <- rbinom(simulations, 1, home_team_win_prob[,1] )
  home_win_probability <- mean(simulated_wins)
  return(home_win_probability)
}

extractRankings <- function(model_output, ...){
  UseMethod("extractRankings", model_output)
}

extractRankings.list <- function(model_output, burn_in_rate = 0.1){
  ranking_matrix <- model_output$rankings
  simulations <- nrow(ranking_matrix)
  start_position <- floor(burn_in_rate * simulations)
  rankings <- sort(colMeans(ranking_matrix[start_position:simulations, ]), decreasing = TRUE)
  rankings_df <- data.frame(School = names(rankings), 
                            Score = rankings,
                            Rank = 1:length(rankings))
  return(rankings_df)
}

extractRankings.data.frame <- function(model_output){
  rankings <- model_output %>%
    arrange(desc(Score)) %>%
    mutate(Rank = 1:nrow(.))

  return(rankings)
}

getFinishedResults <- function(df){
  finished_games <- df %>%
    filter(!is.na(HomeGoals), HomeGoals != AwayGoals)

  results <- finished_games %>%
    mutate(Neutral = GameType == "Neutral") %>%
    select(Home, Away, Date, AwayGoals, HomeGoals, Neutral) %>%
    mutate(Date = parse_date_time(Date, "a b d")) %>%
    rename(Home.Team = "Home") %>%
    rename(Away.Team = "Away") %>%
    mutate(Winning.Team = ifelse(AwayGoals > HomeGoals, "Visiting", "Home"))

  return(results)
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

calculcateWinFraction <- function(df, method = "absolute"){
  winFraction <- switch(
    method,
    "absolute" = as.numeric(df$HomeGoals > df$AwayGoals),
    "relative" = df$HomeGoals / (df$AwayGoals + df$HomeGoals),
    "step"     = calculateStepWF(df),
    "logit"    = calculateLogitWF(df), 
    stop("Not a valid method")
  )
  return(winFraction)
}

calculateStepWF <- function(df){
  wf <- bound(.5 + (df$HomeGoals - df$AwayGoals) *.1, 0, 1)
  return(wf)
}

calculateLogitWF <- function(df, denominator = 2){
  differential <- df$HomeGoals - df$AwayGoals
  wf <- exp(differential / denominator) / (1 + exp(differential / denominator))
  return(wf)
}

bound <- function(x, lb, ub){
  x[x < lb] <- lb
  x[x > ub] <- ub
  return(x)
}

buildRankingsDF <- function(model_output_list, results){
  extractRankings(model_output_list[[1]])
  
  divisions <- data.frame(School = d1teams, 
                          Division = 1, 
                          stringsAsFactors = FALSE) %>%
    bind_rows(data.frame(School = d2teams,
                         Division = 2, 
                         stringsAsFactors = FALSE))
  
  model_rankings <- model_output_list %>%
    plyr::ldply( extractRankings) %>%
    group_by(School) %>%
    mutate(Average = mean(Rank),
           AveScore = mean(Score)) %>%
    ungroup() %>%
    select(-Score) %>%
    tidyr::spread(.id,Rank) %>%
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

leastSquaresRankings <- function(game_results, HFA = TRUE){
  teams <- union(game_results$Home.Team, game_results$Away.Team)
  
  A <- matrix(0, nrow(game_results), length(teams))
  colnames(A) <- teams
  if(HFA){
    A <- cbind(A, c(HFA = 1))
    colnames(A) <- c(teams, "HFA")
  }
  
  for(i in 1:nrow(game_results)){
    A[i, game_results$Home.Team[i]] <- 1
    A[i, game_results$Away.Team[i]] <- -1
  }
  
  b <- game_results %>%
    mutate(Difference = HomeGoals - AwayGoals) %>%
    select(Difference) %>%
    as.matrix()
  Score <- MASS::ginv(A) %*% b
  
  ratings <- data.frame(School = colnames(A), 
                        Score = Score) %>%
    rename(Score = "Difference") %>%
    arrange(desc(Score))
  
  return(ratings)
}

