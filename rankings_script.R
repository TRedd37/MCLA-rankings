source("../../../../Personal projects/MCLA-rankings/data_pull.R")
source("../../../../Personal projects/MCLA-rankings/Graves_Reese_rankings.R")

results <- getFinishedResults(mcla2018todate)

output <- calculateRankings(results, 1000)

rankings <- data.frame(School = names(extractRankings(output)),
                       Score = extractRankings(output),
                       Rank = 1:length(extractRankings(output)),
                       stringsAsFactors = FALSE)


team_list <- as.list(sort(rankings$School))
names(team_list) <- sort(rankings$School)




