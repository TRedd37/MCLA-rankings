library(shiny)

setwd("ShinyApps/MCLA_rankings/")

load("backup.RData")

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
  output$prediction <- renderText({
    scales::percent(predictGameOutcome(as.character(input$homeTeam), as.character(input$awayTeam), output = model_output))
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)