library(pool)
suppressMessages(library("doFuture"))
library(ReddRankings)
suppressMessages(library(dplyr))
library(doRNG)

registerDoFuture()  ## tells foreach futures should be used
plan(multisession)  ## specifies what type of futures
iterations <-  50000

results <- getResults("2022")
pool <- createAWSConnection()


output <- foreach(i = 1:6, .packages = c("ReddRankings", "tidyverse", "dplyr")) %dorng% {
model_options <- list(WF_method = c('logit', 'logit', 
                                    'step', 'step', 
                                    'absolute', 'absolute'),
                      HFA = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

  calculateRankings(results = results, iters = iterations, 
                    WF_method = model_options$WF_method[i], 
                    HFA = model_options$HFA[i], quietly = TRUE)
}
names(output) <- c("logit", "logit_HFA", 
                   "step", "step_HFA", 
                   "absolute", "absolute_HFA")
output[["scoreBased"]] <- leastSquaresRankings(results, FALSlkjlkjlkjjlkjE)
output[["scoreBased_HFA"]] <- leastSquaresRankings(results, TRUE)

output %>% 
  writeResultsToDatabase(pool)

pool %>%
  twitterHandleChecks(30)

pool %>%
  dmRankings(1)

pool %>%
  poolClose()

