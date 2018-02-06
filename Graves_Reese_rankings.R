
raw_results <- read.csv("~/Personal projects/MCLA-rankings/MCLA scores.csv", stringsAsFactors = FALSE)

results <- raw_results[, c("Home.Team", "Away.Team")]
results$Winning.Team <- "Home"
results$Winning.Team[raw_results$Away.Score > raw_results$Home.Score] <- "Visiting"
results$Neutral <- raw_results$N.CT.NT != ""


raw_results_2018 <- read.csv("~/Personal projects/MCLA-rankings/2018 scores.csv", 
                        stringsAsFactors = FALSE)

results <- data.frame(Home.Team = raw_results_2018$Home,
                      Away.Team = raw_results_2018$Away,
                      Neutral = FALSE,
                      stringsAsFactors = FALSE)
results$Winning.Team <- "Home"
results$Winning.Team[raw_results$Score.Away > raw_results$Score.Home] <- "Visiting"

plot(density(1/rgamma(10000, shape = 20, rate = 20)))

variance <- 1/rgamma(10000, shape = 15, rate = 10)
plot(density(variance))
rankings <- rnorm(10000, 0, variance)
plot(density(rankings))

exp(2) / (exp(2) + exp(1.5))

plot(density(rnorm(10000, 0, 1.5)))



# results <- data.frame(
# 	Home.Team = c(1, 2, 3, 1, 2, 3),
# 	Visiting.Team = c(3, 3, 1, 2, 1, 2), 
# 	Winning.Team = c("Home", "Visiting", "Visiting", "Home", "Visiting", "Visiting"),
# 	stringsAsFactors = FALSE)


################################

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
      g_old <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1], team_name = team)
      rankings[i, team ] <- candidate_ranking
      g_cand <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1], team_name = team)
      log_acceptance_probability 	<- (g_cand - g_old)
      acceptance_value 		<- log(runif(1))
      if(log_acceptance_probability < acceptance_value){
        rankings[i, team ] <- old_ranking
      }
    }
    
    alpha[i] <- rnorm(1, alpha[i-1], sqrt(candidate_sigma))
    g_old <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i-1], team_name = NULL)
    g_cand <- calculateG(results, rankings[i, ], sigma[i-1], alpha[i],team_name =  NULL)
    log_acceptance_probability 	<- (g_cand - g_old)
    acceptance_value 		<- log(runif(1))
    if(log_acceptance_probability < acceptance_value){
      alpha[i] <- alpha[i-1]
    }
    sigma[i] <- 1/rgamma(1, A + (length(teams)/2) , rate = B + (sum(rankings[i, ]^2)/2))
  }
  
  output <- list(rankings = rankings,
                 alpha = alpha,
                 sigma = sigma)
  return(output)
}



calculateG <- function(results, rankings, sigma, alpha, team_name){
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
    denominator      <- exp(home_ranking) + exp(visiting_ranking)
    probabilities[game] <- switch(results$Winning.Team[game],
                                  "Home" = home_ranking / denominator,
                                  "Visiting" = visiting_ranking / denominator)
  }
  g <- sum(dnorm(rankings, 0, sqrt(sigma), log = TRUE)) +
    dnorm(alpha, 0, sqrt(S), log = TRUE) +
    sum(log(probabilities)) 
  return(g)
}


output <- calculateRankings(results, 100)


save(rankings, alpha, sigma, file = "~/Dropbox/Lacrosse/first_pass.RData")
