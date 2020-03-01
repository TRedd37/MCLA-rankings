library(pool)
suppressMessages(library("doFuture"))
library(ReddRankings)
suppressMessages(library(dplyr))

registerDoFuture()  ## tells foreach futures should be used
plan(multisession)  ## specifies what type of futures
iterations <-  50000

results <- getResults("2020")
pool <- createAWSConnection()

model_options <- list(WF_method = c('logit', 'logit', 'step', 'step', 'absolute', 'absolute'),
                      HFA = c(F,T,F,T,F,T))

output <- foreach(i = 1:6, .packages = c("ReddRankings", "tidyverse", "dplyr")) %dopar% {
  calculateRankings(results = results, iters = iterations, 
                    WF_method = model_options$WF_method[i], 
                    HFA = model_options$HFA[i], quietly = TRUE)
}
names(output) <- c("logit", "logit_HFA", "step", "step_HFA", "absolute", "absolute_HFA")
output[["scoreBased"]] <- leastSquaresRankings(results, FALSE)
output[["scoreBased_HFA"]] <- leastSquaresRankings(results, TRUE)

output %>% 
  writeResultsToDatabase(pool)

pool %>%
  twitterHandleChecks(30)

pool %>%
  poolClose()
