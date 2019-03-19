library(shiny)

shinyUI(fluidPage(
  titlePanel("Redd Rankings"),
  mainPanel(
    tabsetPanel(
      tabPanel("Rankings",  dataTableOutput("rankings") )
    )
  )
))