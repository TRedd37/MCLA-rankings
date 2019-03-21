#' @export
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
  Score <- ginv(A) %*% b
  
  ratings <- data.frame(School = colnames(A), 
                        Score = Score) %>%
    rename(Score = "Difference") %>%
    arrange(desc(Score))
  
  return(ratings)
}

