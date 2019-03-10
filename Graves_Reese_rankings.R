
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
    results <- results[results$Home.Team == team_name | results$Away.Team == team_name, ]
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

extractRankings <- function(model_output, burn_in_rate = 0.1){
  ranking_matrix <- model_output$rankings
  simulations <- nrow(ranking_matrix)
  start_position <- floor(burn_in_rate * simulations)
  rankings <- sort(colMeans(ranking_matrix[start_position:simulations, ]), decreasing = TRUE)
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

buildRankingsDF <- function(model_output, results){
  rankings <- data.frame(School = names(extractRankings(model_output)),
                         Score  = extractRankings(model_output),
                         Rank   = 1:length(extractRankings(model_output)),
                         Division = 1,
                         stringsAsFactors = FALSE)
  record   <- ldply(rankings$School,  getRecord, results = results)
  
  rankings <- rankings %>%
    bind_cols(record)
  rankings$Division[rankings$School %in% d2teams] <- 2
  rankings <- rankings %>% 
    filter(School %in% union(d2teams, d1teams))
  return(rankings)
}


