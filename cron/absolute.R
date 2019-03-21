source("config.R")

model_output        <- calculateRankings(results, iterations,
                                         quietly = TRUE)
model_output_wo_HFA <- calculateRankings(results, iterations, HFA = FALSE,
                                         quietly = TRUE)


model_list <- list(absolute_HFA = model_output,
                   absolute = model_output_wo_HFA)

for(i in seq_along(model_list)){
  writeResultsToDatabase(model_list[[i]], names(model_list)[i], con)
}

dbDisconnect(con)