source("../../../../Personal projects/MCLA-rankings/data_pull.R")
source("../../../../Personal projects/MCLA-rankings/Graves_Reese_rankings.R")

head(mcla2018todate)

results <- getFinishedResults(mcla2018todate)

subset(results, Home.Team == "Clemson"|Away.Team == "Texas A&M")

output <- calculateRankings(results, 10000)
predictGameOutcome("Boise State", "Brigham Young", output)
1- predictGameOutcome("Stanford", "Brigham Young", output)
1- predictGameOutcome("California", "Brigham Young", output)

rankings <- data.frame(School = names(extractRankings(output)),
                       Score = extractRankings(output),
                       Rank = 1:length(extractRankings(output)),
                       stringsAsFactors = FALSE)


team_list <- as.list(sort(rankings$School))
names(team_list) <- sort(rankings$School)


extractRankings(output)[c("California", "Brigham Young", "UNLV", "Boise State", "Utah")]



