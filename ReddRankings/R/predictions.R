con %>%
  tbl("TeamIDs") %>%
  filter(TeamName %in% c("Brigham Young", "Utah Valley", "Colorado", "Colorado State"))

con %>%
  tbl("SimulationResults") %>%
  filter(TeamID %in% c(15, 25, 37, 107)) %>%
  group_by(TeamID, ModelID) %>%
  summarize(Mean = mean(Score)) %>%
  collect() %>% as.data.frame()

predictMatchup(37, 107)


predictMatchup <- function(home_team, away_team, simulations = 10000){
  model_data <- con %>%
    tbl("ModelIDs") %>%
    filter(Active == 1)%>% 
    collect()
  
  team_data <- con %>%
    tbl("TeamIDs")
  
  sims_per_model <- floor(simulations / sum(model_data$Active))
  all_simulated_wins <- NULL
  
  model_ids <- model_data %>%
    pull(ID)

  max_times <- con %>%
    tbl("SimulationResults") %>%
    group_by(ModelID) %>%
    summarize(maxTime = max(Time))
  
  Simulation_results <- con %>%
    tbl("SimulationResults") %>%
    inner_join(max_times, by = c(ModelID = "ModelID", Time = "maxTime")) %>% 
    collect()
  
  for(model in model_ids){
    if(model_data$SimulationStyle[model_data$ID == model] == 1){
      simulated_wins <- bayesianSimulation(model, home_team, away_team, sims_per_model)
    } else {
      simulated_wins <- leastSquaresSimulation(model, home_team, away_team, sims_per_model)
    }
    all_simulated_wins <- c(all_simulated_wins, simulated_wins)
  }
  return(mean(all_simulated_wins))
}

bayesianSimulation <- function(model, home_team, away_team, sims_per_model){
  home_team_scores <- Simulation_results %>%
    filter(ModelID == !!model) %>%
    filter(TeamID == !!home_team) %>%
    collect() %>%
    sample_n(sims_per_model, replace = TRUE) 
  
  away_team_scores <- Simulation_results %>%
    filter(ModelID == !!model) %>%
    filter(TeamID == !!away_team) %>%
    collect() %>%
    sample_n(sims_per_model, replace = TRUE) 
  
  home_score <- exp(home_team_scores$Score)
  away_score <- exp(away_team_scores$Score)
  denominator <- home_score + away_score
  
  home_team_win_prob <-  home_score / denominator
  simulated_wins <- rbinom(sims_per_model, 1, home_team_win_prob )
  return(simulated_wins)
}

leastSquaresSimulation <- function(model, home_team, away_team, sims_per_model){
  home_team_score <- con %>%
    tbl("SimulationResults") %>%
    filter(ModelID == !!model) %>%
    filter(TeamID == !!home_team) %>%
    pull(Score)
  
  away_team_score <- con %>%
    tbl("SimulationResults") %>%
    filter(ModelID == !!model) %>%
    filter(TeamID == !!away_team) %>%
    pull(Score)
  
  simulated_wins <- rep(home_team_score > away_team_score, sims_per_model) %>%
    as.numeric()
  return(simulated_wins)
}
