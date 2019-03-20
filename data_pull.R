library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(ReddRankings)

full_schedule <- getSchedule("2019")

venue_home_team <- getVenuesHomeTeam(full_schedule, FALSE)

full_schedule <- full_schedule %>% 
  dplyr::left_join(venue_home_team, by = "VenueURL") %>%
  dplyr::mutate(GameType = ifelse(Home == VenueHomeTeam, "Home", "Neutral"))

mcla2019todate <- full_schedule
