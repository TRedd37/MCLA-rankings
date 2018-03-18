source("../../../../Personal projects/MCLA-rankings/data_pull.R")
#source("http://grimshawville.byu.edu/makedataMCLA2018.R")
source("../../../../Personal projects/MCLA-rankings/Graves_Reese_rankings.R")

mcla2018todate$date <- parse_date_time(mcla2018todate$Date, "a b d")

results <- getFinishedResults(mcla2018todate)


model_output <- calculateRankings(results, 5000)

rankings <- data.frame(School = names(extractRankings(model_output)),
                       Score  = extractRankings(model_output),
                       Rank   = 1:length(extractRankings(model_output)),
                       Division = 1,
                       stringsAsFactors = FALSE)
record   <- ldply(rankings$School,  getRecord, results = results)
rankings <- cbind(rankings, record)
rankings$Division[rankings$School %in% d2teams] <- 2

which(rankings$School == "Brigham Young")

1-predictGameOutcome("Utah", "Brigham Young", output)

save(results, rankings, model_output, file = "../../../../Personal projects/MCLA-rankings/backup.RData")



