library(shiny)

#load("backup.RData")
# team_list <- as.list(sort(rankings$School))
#source("Graves_Reese_rankings.R")

shinyUI(fluidPage(
  titlePanel("Redd Rankings"),
  # p(paste0("Games Through: ", games_through)),
  # p(paste0("Last Updated: ", updated_at)),
  mainPanel(
    tabsetPanel(
      tabPanel("Rankings",  dataTableOutput("rankings") ) #,
      # tabPanel("Predictions", 
      #          selectInput("homeTeam", 
      #                      label = "Select Team 1:", 
      #                      choices = team_list),
      #          selectInput("awayTeam", 
      #                      label = ("Select Team 2:"), 
      #                      choices = team_list),
      #          textOutput("prediction")),
      # tabPanel("Games", dataTableOutput("games"))
      # ,
      # tabPanel("D1 Championships", dataTableOutput("d1_output")),
      # tabPanel("D2 Championships", dataTableOutput("d2_output"))
    )
  )
))