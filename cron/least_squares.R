source("config.R")

model_least_square  <- leastSquaresRankings(results, FALSE)
model_LS_HFA        <- leastSquaresRankings(results, TRUE)


model_list <- list(scoreBased = model_least_square,
                   scoreBased_HFA = model_LS_HFA)

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)