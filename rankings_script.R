games_through <- lubridate::now()
source("data_pull.R")
source("Graves_Reese_rankings.R")

mcla2018todate$date <- parse_date_time(mcla2018todate$Date, "a b d")

results <- getFinishedResults(mcla2018todate)

model_output <- calculateRankings(results, 50000)

rankings <- data.frame(School = names(extractRankings(model_output)),
                       Score  = extractRankings(model_output),
                       Rank   = 1:length(extractRankings(model_output)),
                       Division = 1,
                       stringsAsFactors = FALSE)
record   <- ldply(rankings$School,  getRecord, results = results)
rankings <- cbind(rankings, record)
rankings$Division[rankings$School %in% d2teams] <- 2

updated_at <- lubridate::now()

#source("tournaments.R")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = "../ShinyApps/MCLA_rankings/backup.RData")
time_stamp_file <- paste0("old data/backup_", format(now(), "%Y_%m_%d %H_%M_%S"), ".RData")

save(games_through, updated_at, results, rankings, model_output, d1_output, d2_output, file = time_stamp_file)

system("touch ../ShinyApps/MCLA_rankings/restart.txt")



