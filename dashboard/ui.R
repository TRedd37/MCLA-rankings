library(shiny)


shinyUI(fluidPage(
  titlePanel(paste0("Redd's Rankings\nGames Through: :", games_through)),
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