#' @export
calculateRankings <- function(results, iters = 10000, 
                              WF_method = "absolute", HFA = TRUE,
                              quietly = FALSE){
  divisions <- getDivisions()

  raw_prior_means <- rep(c(0.75, -0.75), times = c(length(divisions$D1), 
                                                   length(divisions$D2)))
  names(raw_prior_means) <- c(divisions$D1, divisions$D2)
  
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
  if(!quietly){
    pb <- txtProgressBar(min = 0, max = iters, style = 3)
  }
  
  
  for(i in 2:iters){
    if(!quietly){
      setTxtProgressBar(pb, i)
    }
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


#' @export
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
