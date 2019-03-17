library(shiny)
library(pool)
library(RMySQL)
library(dplyr)
library(tidyr)

# load("backup.RData")

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "reddrankings",
  username = "shiny",
  password = "guest",
  host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com"
)

max_timestamp <- pool %>%
  tbl("ModelResults") %>%
  group_by(ModelID) %>%
  summarize(maxTimestamp = max(Time, na.rm = TRUE))

rankings_long <- pool %>%
  tbl("ModelResults") %>%
  left_join(max_timestamp, by = "ModelID") %>%
  left_join(tbl(pool, "ModelIDs"), by = c(ModelID = "ID")) %>%
  left_join(tbl(pool, "TeamIDs"), by = c(TeamID = "ID")) %>%
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

onStop(function() {
  poolClose(pool)
})


shinyServer(function(input, output) {
  output$rankings   <- renderDataTable({rankings_long
    })
  # output$games      <- renderDataTable({results})
  # output$prediction <- renderText({
  #   paste("Probability", 
  #         input$homeTeam, "beats", input$awayTeam, ":", 
  #         scales::percent(
  #           ifelse(input$homeTeam == input$awayTeam,
  #                  0.5,
  #                  predictGameOutcome(input$homeTeam, 
  #                                     input$awayTeam, 
  #                                     output = model_output,
  #                                     neutral = TRUE))
  #         ))
  # })
  # output$d1_output <- renderDataTable({d1_output})
  # output$d2_output <- renderDataTable({d2_output})
})