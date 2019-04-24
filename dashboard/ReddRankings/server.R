model_results <- pool %>%
  tbl("ModelResults") %>%
  collect()

max_times <- model_results %>%
  mutate(Time = ymd_hms(TIME, tz = 'America/Denver')) %>%
  mutate(Time1 = ymd_hms(TIME, tz = 'America/Denver') - hours(8)) %>%
  mutate(Time1 = ceiling_date(Time, unit = 'day')) %>% 
  group_by(Time1, ModelID) %>%
  summarize(maxTime = max(Time)) 

base_rankings <- model_results %>%
  mutate(Time = ymd_hms(TIME, tz = 'America/Denver')) %>%
  right_join(max_times, by = c("ModelID", Time = "maxTime")) %>% 
  left_join(tbl(pool, "ModelIDs"), by = c(ModelID = "ID"), copy = TRUE) %>%
  left_join(tbl(pool, "TeamIDs"), by = c(TeamID = "ID"), copy = TRUE) %>%
  filter(Active == 1) %>%
  filter(TeamName != "HFA") %>%
  select(TeamName, Division, Conference, ModelRank, ModelScore, ModelName, Time1) %>%
  filter(!is.na(Division )) %>%
  group_by(TeamName, Time1) %>%
  mutate(Average = mean(ModelRank),
         AveScore = mean(ModelScore)) %>%
  ungroup() %>%
  select(-ModelScore) %>%
  spread(ModelName, ModelRank) %>%
  arrange(Time1, Average, desc(AveScore)) %>%
  group_by(Time1) %>%
  mutate(ReddRanking = 1:n()) %>%
  ungroup() %>%
  select(-AveScore) %>%
  rename(RR_v1 = "absolute_HFA",
         School = "TeamName") %>%
  select(School, Conference, Division, Time1, ReddRanking, 
         RR_v1, everything(), -Average, Average)

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
  
  filtered_rankings <- reactive({
    base_rankings %>%
      filter(Division %in% input$divisions) %>%
      filter(Conference %in% input$conferences)
  })

  time_specific_rankings <- reactive({
    validate(
      need(input$filterDate > ymd("2019-03-17"), 
           "No rankings before 2019-03-18. Select another date")
    )
    filtered_rankings() %>%
      filter(Time1 == input$filterDate) %>% 
      select(-Time1)
  })
  
  output$over_time_plot <- renderPlotly({
    p <- ggplot( filtered_rankings(), aes( x = Time1, y = ReddRanking, color = School)) +
      geom_line() +
      theme(legend.position = "none",
            axis.title.x=element_blank()) + 
      scale_y_reverse()+
      theme()
    p %>%
      ggplotly() %>% 
      print()
  })

  output$rankings   <- renderDT(time_specific_rankings(), 
                                extensions = 'Buttons',
                                options = list(dom = 'Bfltip',
                                               pageLength = 25,
                                               columnDefs = list(list(targets = c(5:13), 
                                                                      visible = FALSE)),
                                               buttons = I('colvis'))
                                )
})