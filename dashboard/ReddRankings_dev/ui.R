
shinyUI(fluidPage(
  titlePanel("Redd Rankings"),
  mainPanel(
    dateInput("filterDate", "Rankings as of:", value = today(tz = "America/Denver")),
    checkboxGroupInput("conferences", "Conferences:", choices = all_conferences$Conference,
                       selected = all_conferences$Conference, inline = TRUE),
    checkboxGroupInput("divisions", "Divisions:", choices = c(1, 2),
                       selected = 1:2, inline = TRUE),
    tabsetPanel(
      tabPanel("Rankings", DTOutput("rankings") ),
      tabPanel("Rankings Over Time", plotlyOutput("over_time_plot"))
    )
  )
))