source("../../../../Personal projects/MCLA-rankings/data_pull.R")
source("../../../../Personal projects/MCLA-rankings/Graves_Reese_rankings.R")

mcla2018todate$date <- parse_date_time(mcla2018todate$Date, "a b d")

results <- getFinishedResults(mcla2018todate)


output <- calculateRankings(results, 5000)

rankings <- data.frame(School = names(extractRankings(output)),
                       Score  = extractRankings(output),
                       Rank   = 1:length(extractRankings(output)),
                       stringsAsFactors = FALSE)
record   <- ldply(rankings$School,  getRecord, results = results)
rankings <- cbind(rankings, record)

subset(results, Home.Team == "Brigham Young" | Away.Team == "Brigham Young")

which(rankings$School == "Brigham Young")

1-predictGameOutcome("Utah", "Brigham Young", output)


netteam_list <- as.list(sort(rankings$School))
names(team_list) <- sort(rankings$School)

save(results, rankings, model_output, file = "../../../../Personal projects/MCLA-rankings/backup.RData")



