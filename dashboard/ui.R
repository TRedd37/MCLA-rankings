library(shiny)


shinyUI(fluidPage(
  titlePanel("Redd's Rankings"),
  mainPanel(
    tabsetPanel(
      tabPanel("Rankings",  dataTableOutput("rankings") ),
      tabPanel("Predictions", 
               selectInput("homeTeam", 
                           label = "Select Home Team:", 
                           choices = team_list),
               selectInput("awayTeam", 
                           label = ("Select Away Team:"), 
                           choices = team_list),
               textOutput("prediction")),
      tabPanel("Games", dataTableOutput("games"))
    )
  )
))