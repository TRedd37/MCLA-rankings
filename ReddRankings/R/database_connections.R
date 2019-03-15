
writeResultsToDatabase <- function(model_output, model_name, con){
  model_results <- extractRankings(model_output)
  model_id      <- getModelID(model_name, con)
  
  team_ids <- con %>%
    tbl("TeamIDs")
  
  model_results %>%
    left_join(team_ids, by = c(School = "TeamName"), copy = TRUE) %>%
    createNewTeamIDs(con) %>%
    rename(TeamID = "ID") %>%
    rename(ModelRank = "Rank") %>%
    rename(ModelScore = "Score") %>%
    select(TeamID, ModelRank, ModelScore) %>%
    mutate(Time = now()) %>%
    mutate(ModelID = model_id) %>%
    dbWriteTable(conn = con, name = "ModelResults", append = TRUE, row.names = FALSE)
  
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
      filter(ModelName == model_name)
  }
  return(model_id$ID)
}


