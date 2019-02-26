games_through <- lubridate::now()
source("../../../../Personal/Git/MCLA-rankings/data_pull.R")
source("../../../../Personal/Git/MCLA-rankings/Graves_Reese_rankings.R")

mcla2018todate$date <- parse_date_time(mcla2018todate$Date, "a b d")

results <- getFinishedResults(mcla2018todate)

model_output <- calculateRankings(results, 50000, WF_method = "relative")
model_output_step <- calculateRankings(results, 50000, WF_method = "step", HFA = FALSE)

rankings <- buildRankingsDF(model_output_step, results)
rankings

updated_at <- lubridate::now()

#source("tournaments.R")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = "../ShinyApps/MCLA_rankings/backup.RData")
time_stamp_file <- paste0("old data/backup_", format(now(), "%Y_%m_%d %H_%M_%S"), ".RData")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = time_stamp_file)

system("touch ../ShinyApps/MCLA_rankings/restart.txt")



