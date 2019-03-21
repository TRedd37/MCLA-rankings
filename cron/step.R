source("config.R")

model_step          <- calculateRankings(results, iterations, 
                                         WF_method = "step", HFA = FALSE,
                                         quietly = TRUE)
model_step_HFA      <- calculateRankings(results, iterations, WF_method = "step",
                                         quietly = TRUE)


model_list <- list(step = model_step,
                   step_HFA = model_step_HFA)

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)