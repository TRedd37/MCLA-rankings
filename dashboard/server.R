library(shiny)

load("../../../../Personal projects/MCLA-rankings/backup.RData")

shinyServer(function(input, output) {
  # output$rankings   <- renderDataTable({rankings})
    # output$games      <- renderDataTable({results})
    # output$prediction <- renderText({
    #   scales::percent(predictGameOutcome(as.character(input$homeTeam), as.character(input$awayTeam), output = model_output))
    # })
  
  output$rankings   <- renderDataTable({as.data.table(matrix(0, 5, 5))})
  output$games      <- renderDataTable({as.data.table(matrix(0, 5, 5))})
  output$prediction <- renderText({"hello world"})
  
})