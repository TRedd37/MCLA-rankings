games_through <- lubridate::now()
library(ReddRankings)
library(dplyr)

results <- getResults("2019")

iterations <- 50

model_output        <- calculateRankings(results, iterations)
model_output_wo_HFA <- calculateRankings(results, iterations, HFA = FALSE)
model_step          <- calculateRankings(results, iterations, WF_method = "step", HFA = FALSE)
model_logit         <- calculateRankings(results, iterations, WF_method = "logit", HFA = FALSE)
model_step_HFA      <- calculateRankings(results, iterations, WF_method = "step")
model_logit_HFA     <- calculateRankings(results, iterations, WF_method = "logit")
model_least_square  <- leastSquaresRankings(results, FALSE)
model_LS_HFA        <- leastSquaresRankings(results, TRUE)

model_list <- list(absolute_HFA = model_output,
                   absolute = model_output_wo_HFA,
                   step = model_step,
                   logit = model_logit,
                   step_HFA = model_step_HFA,
                   logit_HFA = model_logit_HFA, 
                   scoreBased = model_least_square,
                   scoreBased_HFA = model_LS_HFA)

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

rankings <- buildRankingsDF(model_list, results)
updated_at <- lubridate::now()

#source("tournaments.R")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, 
     file = "../ShinyApps/MCLA_rankings/backup.RData")
time_stamp_file <- paste0("old data/backup_", format(now(), "%Y_%m_%d %H_%M_%S"), ".RData")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = time_stamp_file)

system("touch ../ShinyApps/MCLA_rankings/restart.txt")



