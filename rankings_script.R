
raw_results <- read.csv("~/Personal projects/MCLA-rankings/MCLA scores.csv", stringsAsFactors = FALSE)

results <- raw_results[, c("Home.Team", "Away.Team")]
results$Winning.Team <- "Home"
results$Winning.Team[raw_results$Away.Score > raw_results$Home.Score] <- "Visiting"
results$Neutral <- raw_results$N.CT.NT != ""


raw_results_2018 <- read.csv("~/Personal projects/MCLA-rankings/2018 scores.csv", 
                        stringsAsFactors = FALSE)

results <- data.frame(Home.Team = raw_results_2018$Home,
                      Away.Team = raw_results_2018$Away,
                      Neutral = FALSE,
                      stringsAsFactors = FALSE)
results$Winning.Team <- "Home"
results$Winning.Team[raw_results_2018$Score.Away > raw_results_2018$Score.Home] <- "Visiting"


raw_results_2018 <- read.csv("~/Personal projects/MCLA-rankings/2018 scores 2018_02_13.csv", 
                             stringsAsFactors = FALSE)

results <- data.frame(Home.Team = raw_results_2018$Home,
                      Away.Team = raw_results_2018$Away,
                      Neutral = FALSE,
                      stringsAsFactors = FALSE)
results$Winning.Team <- "Home"
results$Winning.Team[raw_results_2018$Away.Score > raw_results_2018$Home.Score] <- "Visiting"



# results <- data.frame(
# 	Home.Team = c(1, 2, 3, 1, 2, 3),
# 	Visiting.Team = c(3, 3, 1, 2, 1, 2), 
# 	Winning.Team = c("Home", "Visiting", "Visiting", "Home", "Visiting", "Visiting"),
# 	stringsAsFactors = FALSE)


################################

output <- calculateRankings(results, 1000)
predictGameOutcome( "California","Brigham Young", output)
extractRankings(output)[c("California", "Brigham Young", "UNLV", "Boise State")]


sort(colMeans(as.data.frame(output$rankings[(nrow(output$rankings) / 10):  ])), decreasing = TRUE)[1:10]

