library(ReddRankings)
source("config.R")

model_logit         <- calculateRankings(results, iterations, 
                                         WF_method = "logit", HFA = FALS,
                                         quietly = TRUE)
model_logit_HFA     <- calculateRankings(results, iterations, WF_method = "logit",
                                         quietly = TRUE)


model_list <- list(logit = model_logit,
                   logit_HFA = model_logit_HFA)

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)