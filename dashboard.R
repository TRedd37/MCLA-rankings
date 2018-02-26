library(shiny)

ui <- fluidPage(
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
)

server <- function(input, output) {
  output$rankings   <- renderDataTable({rankings})
  output$games      <- renderDataTable({results})
  output$prediction <- renderText({scales::percent(.555)})
}

# Run the app ----
shinyApp(ui = ui, server = server)