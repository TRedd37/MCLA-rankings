library(BradleyTerryScalable)
bt_model <- results %>%
  select(Home.Team, Away.Team, Winning.Team) %>%
  mutate(Home.Win = ifelse(Winning.Team == "Home", 1, 0)) %>%
  mutate(Away.Win = ifelse(Winning.Team == "Visiting", 1,0)) %>%
  select(-Winning.Team) %>%
  btdata() %>%
  btfit(1.1, FALSE) 

summary(bt_model)$item_summary %>%
  as.data.frame()