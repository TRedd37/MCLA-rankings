library(shiny)
library(pool)
library(RMySQL)
library(dplyr)

# load("backup.RData")

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "reddrankings",
  username = "shiny",
  password = "guest",
  host = "reddrankings.cvjutlbtujzn.us-east-2.rds.amazonaws.com"
)

table <- pool %>%
  tbl("ModelResults") %>%
  as.data.frame()

onStop(function() {
  poolClose(pool)
})


shinyServer(function(input, output) {
  output$rankings   <- renderDataTable({table
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