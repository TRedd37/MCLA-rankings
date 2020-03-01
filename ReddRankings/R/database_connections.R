#' @export
writeResultsToDatabase <- function(model_outputs, connection){
  time_stamp <- now()
  for(i in seq_along(model_outputs)){
    model_name <- names(model_outputs)[i]
    writeModelResultsToDatabase(model_outputs[[i]], model_name, connection, time_stamp)
  }
  
  invisible(NULL)
}

writeModelResultsToDatabase <- function(model_output, model_name, con, time_stamp){
  model_id <- getModelID(model_name, con)
  team_ids <- con %>% 
    tbl("TeamIDs") %>% 
    collect()
  
  writeRankingsToDatabase(model_output, model_id, team_ids, time_stamp, con)
}

writeRankingsToDatabase <- function(model_output, model_id, team_ids, time_stamp, con){
  model_output %>%
    extractRankings() %>%
    left_join(team_ids, by = c(School = "TeamName")) %>%
    createNewTeamIDs(con) %>%
    rename(TeamID = "ID") %>%
    rename(ModelRank = "Rank") %>%
    rename(ModelScore = "Score") %>%
    select(TeamID, ModelRank, ModelScore) %>%
    mutate(Time = time_stamp) %>%
    mutate(ModelID = model_id) %>%
    dbWriteTable(conn = con, name = "ModelResults", append = TRUE, row.names = FALSE)
  invisible(NULL)
}


createNewTeamIDs <- function(df, con){
  na_teams <- df %>%
    filter(is.na(ID)) %>%
    rename(TeamName = "School") %>%
    select(TeamName) 
  
  if(nrow(na_teams) >0){
    na_teams %>%
      dbWriteTable(conn = con, name = "TeamIDs", append = TRUE, row.names = FALSE)
  }
  
  team_ids <- con %>%
    tbl("TeamIDs")
  
  output <- df %>%
    select(-ID) %>%
    left_join(team_ids, by = c(School = "TeamName"), copy = TRUE) 
  
  return(output)
}

getModelID <- function(model_name, con){
  model_id <- con %>%
    tbl("ModelIDs") %>%
    filter(ModelName == model_name) %>%
    select(ID) %>%
    collect()
  
  if(length(model_id$ID) == 0){
    data.frame(ModelName = model_name) %>%
      dbWriteTable(conn = con, name = "ModelIDs", append = TRUE, row.names = FALSE)
    model_id <- con %>%
      tbl("ModelIDs") %>%
      filter(ModelName == model_name) %>%
      collect()
  }
  return(model_id$ID)
}

writeSimulationsToDatabase <- function(model_output, ...){
  UseMethod("writeSimulationsToDatabase", model_output)
}

writeSimulationsToDatabase.list <- function(model_output, model_id, team_ids, 
                                            time_stamp, con, burn_in = 0.1){
  model_output$rankings %>%
    slice(-1:-floor(burn_in * nrow(.))) %>%
    gather(TeamName, Score) %>%
    mutate(TeamName = as.character(TeamName)) %>%
    left_join(team_ids, by = "TeamName") %>%
    select(ID, Score) %>%
    mutate(ModelID = model_id) %>%
    mutate(Time = time_stamp) %>%
    rename(TeamID = "ID") %>%
    dbWriteTable(conn = con, name = "SimulationResults",
                 append = TRUE, row.names = FALSE)
  invisible(NULL)
}

writeSimulationsToDatabase.data.frame <- function(model_output, model_id, team_ids, 
                                                  time_stamp, con){
  model_output %>%
    mutate(TeamName = as.character(School)) %>%
    left_join(team_ids, by = "TeamName") %>%
    select(ID, Score) %>%
    mutate(ModelID = model_id) %>%
    mutate(Time = time_stamp) %>%
    rename(TeamID = "ID") %>%
    dbWriteTable(conn = con, name = "SimulationResults", 
                 append = TRUE, row.names = FALSE)
  invisible(NULL)
}

#' @export
createAWSConnection <- function(){
  pool <- dbPool(
    drv = MariaDB(),
    dbname = "reddrankings",
    host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com",
    username = "tredd",
    password = Sys.getenv("RDS_PASSWORD")
  )
  return(pool)
}

