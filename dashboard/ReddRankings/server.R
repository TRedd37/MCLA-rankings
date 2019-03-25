

onStop(function() {
  poolClose(pool)
})


shinyServer(function(input, output) {
  
  input_time <- reactive({as.character(ymd(input$filterDate, 
                              tz = "America/Denver") + hours(8))})

  max_timestamp <- reactive({
    pool %>%
      tbl("ModelResults") %>%
      group_by(ModelID) %>%
      filter(Time < !!input_time()) %>%
      summarize(maxTimestamp = max(Time, na.rm = TRUE))})

  rankings_long <- reactive({
    validate(
      need(input$filterDate > ymd("2019-03-17"), 
           "No rankings before 2019-03-18. Select another date")
    )
    
    pool %>%
      tbl("ModelResults") %>%
      left_join(max_timestamp(), by = "ModelID") %>%
      left_join(tbl(pool, "ModelIDs"), by = c(ModelID = "ID")) %>%
      left_join(tbl(pool, "TeamIDs"), by = c(TeamID = "ID")) %>%
      filter(Active == 1) %>%
      filter(TeamName != "HFA") %>%
      filter(TIME == maxTimestamp) %>%
      select(TeamName, ModelRank, ModelScore, ModelName) %>%
      collect() %>%
      group_by(TeamName) %>%
      mutate(Average = mean(ModelRank),
             AveScore = mean(ModelScore)) %>%
      ungroup() %>%
      select(-ModelScore) %>%
      spread(ModelName, ModelRank) %>%
      arrange(Average, desc(AveScore)) %>%
      mutate(ReddRanking = 1:nrow(.)) %>%
      select(-AveScore) %>%
      rename(RR_v1 = "absolute_HFA")%>%
      select(TeamName, ReddRanking, RR_v1, everything(), -Average, Average)
  })
  output$rankings   <- renderDataTable({rankings_long()})
})