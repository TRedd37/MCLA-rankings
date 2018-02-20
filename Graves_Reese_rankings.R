
calculateRankings <- function(results, iters = 10000){
  A = 15
  B = 10
  S = 1.5
  candidate_sigma = .1
  
  teams <- unique(c(results$Home.Team, results$Away.Team))
  
  rankings <- matrix(0, iters, length(teams))
  colnames(rankings) <- teams
  sigma <- rep(1, iters)
  alpha <- rep(0, iters)
  
  pb <- txtProgressBar(min = 0, max = iters, style = 3)
  
  for(i in 2:iters){
    setTxtProgressBar(pb, i)
    rankings[i, ] <- rankings[i-1, ]
    for(team in teams){
      old_ranking <- rankings[i, team]
      candidate_ranking <- rnorm(1, rankings[i-1, team], sqrt(candidate_sigma))
      g_old <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1], 
                          team_name = team, S = S)
      rankings[i, team ] <- candidate_ranking
      g_cand <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1], 
                           team_name = team, S = S)
      log_acceptance_probability 	<- (g_cand - g_old)
      acceptance_value 		<- log(runif(1))
      if(log_acceptance_probability < acceptance_value){
        rankings[i, team ] <- old_ranking
      }
    }
    
    alpha[i] <- rnorm(1, alpha[i-1], sqrt(candidate_sigma))
    g_old <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1], 
                        team_name = NULL, S = S)
    g_cand <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i], 
                         team_name =  NULL, S = S)
    log_acceptance_probability 	<- (g_cand - g_old)
    acceptance_value 		<- log(runif(1))
    if(log_acceptance_probability < acceptance_value){
      alpha[i] <- alpha[i-1]
    }
    sigma[i] <- 1/rgamma(1, A + (length(teams)/2) , rate = B + (sum(rankings[i, ]^2)/2))
  }
  
  output <- list(rankings = as.data.frame(rankings),
                 alpha = alpha,
                 sigma = sigma)
  return(output)
}



calculateG <- function(results, rankings, sigma, alpha, team_name, S){
  if(!is.null(team_name)){
    results <- results[results$Home.Team == team_name | results$Away.Team == team_name, ]
  }
  
  probabilities <- rep(NA, nrow(results))
  
  for(game in 1:nrow(results)){
    hfa <- alpha
    if(results$Neutral[game]){
      hfa <- 0
    }
    home_ranking     <- exp(rankings[results$Home.Team[game]] + hfa)
    visiting_ranking <- exp(rankings[results$Away.Team[game]])
    denominator      <- home_ranking + visiting_ranking
    probabilities[game] <- switch(results$Winning.Team[game],
                                  "Home" = home_ranking / denominator,
                                  "Visiting" = visiting_ranking / denominator)
  }
  g <- sum(dnorm(rankings, 0, sqrt(sigma), log = TRUE)) +
    dnorm(alpha, 0, sqrt(S), log = TRUE) +
    sum(log(probabilities)) 
  return(g)
}

predictGameOutcome <- function(home_team, visiting_team, output, simulations = 10000 ){
  alpha = data.frame(alpha = output$alpha)
  home_team_ratings     <- dplyr::sample_n(output$rankings, simulations, replace = TRUE)[, home_team]
  visiting_team_ratings <- dplyr::sample_n(output$rankings, simulations, replace = TRUE)[ , visiting_team]
  hfa                   <- dplyr::sample_n(alpha, simulations, replace = TRUE)
  
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
  finished_games <- subset(df, !is.na(HomeGoals))
  
  results <- data.frame(Home.Team = finished_games$Home,
                        Away.Team = finished_games$Away,
                        Neutral = finished_games$GameType == "Neutral",
                        stringsAsFactors = FALSE)
  results$Winning.Team <- "Home"
  results$Winning.Team[finished_games$AwayGoals > finished_games$HomeGoals] <- "Visiting"
  return(results)
}

