library(stringr)
library(XML)


teams_html <- htmlParse("http://mcla.us/teams")

d1teams <- readHTMLTable(teams_html, header=FALSE, which=1, stringsAsFactors=FALSE)$V1
d2teams <- readHTMLTable(teams_html, header=FALSE, which=2, stringsAsFactors=FALSE)$V1

raw_prior_means <- rep(c(0.75, -0.75), times = c(length(d1teams ), length(d2teams)))
names(raw_prior_means) <- c(d1teams, d2teams)

full_schedule <- plyr::ldply(1:20, getSchedulePage)

web_venues <- unique(full_schedule$VenueURL)

fields_home_team <- "unknown"
for(i in 1:length(web_venues)){
  games_table <- readVenueTable(web_venues[i])
  if(is.numeric(games_table) || nrow(games_table) < 5 ){
    print(paste("unknown home team - ", web_venues[i]))
    fields_home_team[i] <- "unknown"
  } else {
    fields_home_team[i] <- names(which.max(table(games_table$Home)))
  }
}

venue_home_team <- data.frame(VenueURL = web_venues, 
                              VenueHomeTeam = fields_home_team, 
                              stringsAsFactors = FALSE)

full_schedule <- full_schedule %>% 
  dplyr::left_join(venue_home_team, by = "VenueURL") %>%
  dplyr::mutate(GameType = ifelse(Home == VenueHomeTeam, "Home", "Neutral"))

# handle some special cases where logic says Neutral but is really Homefield
full_schedule$GameType[full_schedule$Home=="Virginia Tech"
                   & full_schedule$Venue=="Virginia Tech - The Marching Virginians Center"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Texas Christian"
                   & full_schedule$Venue=="TCU Intramural Field"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Southeastern Louisiana"
                   & full_schedule$Venue=="Southeastern Soccer Complex (SELU)"] <- "Home"
full_schedule$GameType[full_schedule$Venue=="North Park Field"] <- "Neutral"
full_schedule$GameType[full_schedule$Home=="Southern Illinois"
                   & full_schedule$Venue=="SIU Carbondale"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Keiser University"
                   & full_schedule$Venue=="Keiser University"] <- "Home"
full_schedule$GameType[full_schedule$Home=="SUNY Purchase"
                   & full_schedule$Venue=="SUNY-Purchase"] <- "Home"
full_schedule$GameType[full_schedule$Home=="UC Colorado Springs"
                   & full_schedule$Venue=="University of Colorado (Colorado Springs)"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Southeastern Louisiana"
                   & full_schedule$Venue=="Strawberry Stadium"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Weber State"
                   & full_schedule$Venue=="Spence Eccles Ogden Community Sports Complex"] <- "Home"
full_schedule$GameType[full_schedule$Venue=="Vanderbilt - Rec Field 2"] <- "Neutral"
full_schedule$GameType[full_schedule$Home=="Auburn"
                   & full_schedule$Venue=="Duck Samford (Auburn)"] <- "Home"
full_schedule$GameType[full_schedule$Home=="West Virginia"
                   & full_schedule$Venue=="West Virginia - University HS"] <- "Home"
full_schedule$GameType[full_schedule$Venue=="South Barrington Park District"] <- "Neutral"
full_schedule$GameType[full_schedule$Home=="Ole Miss"
                   & full_schedule$Venue=="Lafayette High School"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Oklahoma"
                   & full_schedule$Venue=="Putnam City North Football Stadium"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Nebraska"
                   & full_schedule$Venue=="Fleming Fields"] <- "Home"
full_schedule$GameType[full_schedule$Home=="UC Santa Cruz"
                   & full_schedule$Venue=="Santa Cruz High School"] <- "Home"
full_schedule$GameType[full_schedule$Home=="Montana"
                   & full_schedule$Venue=="Fort Missoula Regional Park"] <- "Home"


mcla2019todate <- full_schedule
