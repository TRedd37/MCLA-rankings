base_rankings %>%
  filter(Time1 == today(tz = "America/Denver")) %>%
  select(-Time1)


base_rankings$Time1 %>%
  unique()

teams <- pool %>%
  tbl("TeamIDs") %>%
  collect()


current_rankings <- pool %>%
  tbl("ModelResults") %>%
  collect() %>%
  group_by(ModelID) %>%
  filter(TIME == max(TIME)) %>%
  group_by(TeamID) %>%
  summarize(ranking_average = mean(ModelRank)) %>%
  arrange(ranking_average) %>%
  left_join(teams, by = c(TeamID = "ID"))

d1top25 <- current_rankings %>%
  filter(Division == 1) %>%
  ungroup() %>%
  slice(1:25)

d2top25 <- current_rankings %>%
  filter(Division == 2) %>%
  ungroup() %>%
  slice(1:25)
  

arrange(TIME)
  collect() %>%
  mutate(Time = ymd_hms(TIME, tz = 'America/Denver')) %>%
  filter(Time > floor_date(now(), 'year')) 