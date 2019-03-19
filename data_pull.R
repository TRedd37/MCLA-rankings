library(stringr)
library(XML)


teams_html <- htmlParse("http://mcla.us/teams")

d1teams <- readHTMLTable(teams_html, header=FALSE, which=1, stringsAsFactors=FALSE)$V1
d2teams <- readHTMLTable(teams_html, header=FALSE, which=2, stringsAsFactors=FALSE)$V1

raw_prior_means <- rep(c(0.75, -0.75), times = c(length(d1teams), length(d2teams)))
names(raw_prior_means) <- c(d1teams, d2teams)

full_schedule <- getSchedule(2019)

venue_home_team <- getVenuesHomeTeam(full_schedule, FALSE)

full_schedule <- full_schedule %>% 
  dplyr::left_join(venue_home_team, by = "VenueURL") %>%
  dplyr::mutate(GameType = ifelse(Home == VenueHomeTeam, "Home", "Neutral"))

mcla2019todate <- full_schedule
