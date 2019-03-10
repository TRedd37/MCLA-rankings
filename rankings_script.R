games_through <- lubridate::now()
source("data_pull.R")
source("Graves_Reese_rankings.R")

mcla2019todate$date <- parse_date_time(mcla2019todate$Date, "a b d")

results <- mcla2019todate %>%
  filter(!(Away == "Florida" & Home == "North Florida")) %>%
  filter(!(Away == "Kennesaw State" & Home == "Georgia")) %>%
  getFinishedResults()


model_output <- calculateRankings(results, 50000)
model_output_wo_HFA <- calculateRankings(results, 50000, HFA = FALSE)
model_step_HFA <- calculateRankings(results, 50000, WF_method = "step")
model_logit_HFA <- calculateRankings(results, 50000, WF_method = "logit")
model_step <- calculateRankings(results, 50000, WF_method = "step", HFA = FALSE)
model_logit <- calculateRankings(results, 50000, WF_method = "logit", HFA = FALSE)

rankings <- buildRankingsDF(model_output, results)
rankings_step <- buildRankingsDF(model_output_step, results)
rankings_logit <- buildRankingsDF(model_output_logit, results)

updated_at <- lubridate::now()

#source("tournaments.R")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = "../ShinyApps/MCLA_rankings/backup.RData")
time_stamp_file <- paste0("old data/backup_", format(now(), "%Y_%m_%d %H_%M_%S"), ".RData")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = time_stamp_file)

system("touch ../ShinyApps/MCLA_rankings/restart.txt")



