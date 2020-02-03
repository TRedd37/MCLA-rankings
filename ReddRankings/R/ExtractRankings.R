#' @export
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
                            Rank = 1:length(rankings), 
                            stringsAsFactors = FALSE)
  return(rankings_df)
}

extractRankings.data.frame <- function(model_output){
  rankings <- model_output %>%
    arrange(desc(Score)) %>%
    mutate(Rank = 1:nrow(.)) %>%
    mutate(School = as.character(School))
  
  return(rankings)
}