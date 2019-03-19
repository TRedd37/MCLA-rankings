library(ReddRankings)
source("~/Github/MCLA-rankings/cron/config.R")

model_logit         <- calculateRankings(results, iterations, WF_method = "logit", HFA = FALSE)
model_logit_HFA     <- calculateRankings(results, iterations, WF_method = "logit")


model_list <- list(logit = model_logit,
                   logit_HFA = model_logit_HFA)

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)