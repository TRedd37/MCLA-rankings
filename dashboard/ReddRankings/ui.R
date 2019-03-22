library(shiny)
library(lubridate)

shinyUI(fluidPage(
  titlePanel("Redd Rankings"),
  mainPanel(
    dateInput("filterDate", "Rankings as of:", value = today(tz = "America/Denver")),
    tabsetPanel(
      tabPanel("Rankings", dataTableOutput("rankings") )
    )
  )
))