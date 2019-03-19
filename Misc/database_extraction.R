library(RMySQL)
con <- dbConnect(MySQL(),
                 "reddrankings",
                 username = "tredd",
                 password = rstudioapi::askForPassword("Database password"),
                 host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com")
  

  
writeResultsToDatabase(model_output, "absolute_HFA", con)
writeResultsToDatabase(model_output_wo_HFA, "test", con)

max_timestamp <- con %>%
  tbl("ModelResults") %>%
  group_by(ModelID) %>%
  summarize(maxTimestamp = max(Time, na.rm = TRUE))

rankings_long <- con %>%
  tbl("ModelResults") %>%
  left_join(max_timestamp, by = "ModelID") %>%
  left_join(tbl(con, "ModelIDs"), by = c(ModelID = "ID")) %>%
  left_join(tbl(con, "TeamIDs"), by = c(TeamID = "ID")) %>%
  filter(Active == 1) %>%
  filter(TIME == maxTimestamp) %>%
  select(TeamName, ModelRank, ModelScore, ModelName) %>%
  collect() %>%
  group_by(TeamName) %>%
  mutate(Average = mean(ModelRank),
         AveScore = mean(ModelScore)) %>%
  ungroup() %>%
  select(-ModelScore) %>%
  spread(ModelName, ModelRank) %>%
  arrange(Average, desc(AveScore)) %>%
  mutate(ReddRanking = 1:nrow(.)) %>%
  select(-AveScore) %>%
  rename(RR_v1 = "absolute_HFA")%>%
  select(TeamName, ReddRanking, RR_v1, everything(), -Average, Average) 



dbDisconnect(con)
