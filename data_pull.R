
# MCLA 2018 Data (to date)

# webscraper

library(XML)
library(plyr)
library(stringr)
library(dplyr)


teams_html <- htmlParse("http://mcla.us/teams")

d1teams <- readHTMLTable(teams_html, header=FALSE, which=1, stringsAsFactors=FALSE)$V1
d2teams <- readHTMLTable(teams_html, header=FALSE, which=2, stringsAsFactors=FALSE)$V1

raw_prior_means <- rep(c(0.75, -0.75), times = c(length(d1teams ), length(d2teams)))
names(raw_prior_means) <- c(d1teams, d2teams)

# Games and Results

# http://mcla.us/schedule/20187?page=1



getSchedulePage <- function(i){
  schedule_url  <- paste0("http://mcla.us/schedule/2018?page=", i)
  schedule_html <- htmlParse(schedule_url)
  schedule <-readHTMLTable(schedule_html, header=TRUE, which=1, stringsAsFactors=FALSE)
  # is this a page of results or future games
  if(!"Score" %in% names(schedule)){
    schedule$Score <- "N/A"
  }
  return(schedule)
}

full_schedule <- ldply(1:22, getSchedulePage)

# parse score

parseScores <- function(unparsed_scores){
  scores <- str_split(unparsed_scores, "-", simplify = TRUE)
  scores[scores == "N/A"] <- 0
  scores[scores == ""] <- 0
  scores <- as.data.frame(scores, stringsAsFactors = FALSE)
  names(scores) <- c("AwayGoals", "HomeGoals")
  output <- scores %>% mutate_all(as.numeric)
  return(output)
}

scores <- parseScores(full_schedule$Score)


# identify home field or neutral location

#http://mcla.us/venue/montana_state

# correct some names to their venue webpage name
full_schedule$VenueWeb <- gsub(' ', '_', full_schedule$Venue)


full_schedule$VenueWeb[full_schedule$Venue=="Smithson Valley High School"] <- "smithson_high_school"
full_schedule$VenueWeb[full_schedule$Venue=="University of Texas - Dallas"] <- "university_of_texasdallas"
full_schedule$VenueWeb[full_schedule$Venue=="Instituto Politecnico Nacional"] <- "instituto-politecnico-nacional"
full_schedule$VenueWeb[full_schedule$Venue=="UC-Santa Barbara"] <- "ucsanta_barbara"
full_schedule$VenueWeb[full_schedule$Venue=="Joe Aillet Stadium"] <- "Aillet"
full_schedule$VenueWeb[full_schedule$Venue=="Veterans Park"] <- "veterans-park"
full_schedule$VenueWeb[full_schedule$Venue=="Florida Atlantic University"] <- "florida_atlantic"
full_schedule$VenueWeb[full_schedule$Venue=="University of Arkansas"] <- "rec_services_grass_field_fayetteville"
full_schedule$VenueWeb[full_schedule$Venue=="Allison North Stadium"] <- "allison-north-stadium"
full_schedule$VenueWeb[full_schedule$Venue=="E. Washington Field Complex"] <- "e-washington-field-complex"
full_schedule$VenueWeb[full_schedule$Venue=="UNC-Charlotte"] <- "unccharlotte"
full_schedule$VenueWeb[full_schedule$Venue=="University of California - Berkeley"] <- "university_of_california_berkeley"
full_schedule$VenueWeb[full_schedule$Venue=="Prestonwood Christian Academy"] <- "prestonwood-christian-academy"
full_schedule$VenueWeb[full_schedule$Venue=="Virginia Tech - The Marching Virginians Center"] <- "the-marching-virginians-center"
full_schedule$VenueWeb[full_schedule$Venue=="Cusabo Nation Lacrosse Complex"] <- "cusabo-nation-lacrosse-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Vanderbilt - Natchez Trace Field"] <- "natchez-field"
full_schedule$VenueWeb[full_schedule$Venue=="Wake Forest - Water Tower Field"] <- "water-tower-field"
full_schedule$VenueWeb[full_schedule$Venue=="Auburn Intramural Fields"] <- "auburn-intramural-fields"
full_schedule$VenueWeb[full_schedule$Venue=="Johnny Downs Sports Complex"] <- "johnny-downs-sports-complex"
full_schedule$VenueWeb[full_schedule$Venue=="South Carolina-Bluff Road Practice Fields"] <- "bluff-road-practice-fields"
full_schedule$VenueWeb[full_schedule$Venue=="T L Hanna High School"] <- "t-l-hanna-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Emory - Candler Park"] <- "candler"
full_schedule$VenueWeb[full_schedule$Venue=="University of Portland"] <- "portland"
full_schedule$VenueWeb[full_schedule$Venue=="UC-Irvine Field 2"] <- "ucirvine_field2"
full_schedule$VenueWeb[full_schedule$Venue=="Joint Venture Park"] <- "joint-venture-park"
full_schedule$VenueWeb[full_schedule$Venue=="Cal State-Long Beach"] <- "cal_statelong_beach"
full_schedule$VenueWeb[full_schedule$Venue=="Hidden Creek Polo Fields"] <- "hidden-creek-polo-fields"
full_schedule$VenueWeb[full_schedule$Venue=="Aurora Sports Park"] <- "aurora-sports-park"
full_schedule$VenueWeb[full_schedule$Venue=="Tennessee-Sutherland Fields"] <- "sutherland_fields"
full_schedule$VenueWeb[full_schedule$Venue=="Mulligan Field (Gonzaga)"] <- "mulligan_field_gonzaga"
full_schedule$VenueWeb[full_schedule$Venue=="Camas High School"] <- "camas-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Eastern Carver County Athletic Center"] <- "eastern-carver-county-athletic-center"
full_schedule$VenueWeb[full_schedule$Venue=="Concordia University"] <- "concordia_university_ca"
full_schedule$VenueWeb[full_schedule$Venue=="Texas A & M"] <- "texas_a__m"
full_schedule$VenueWeb[full_schedule$Venue=="North Florida"] <- "north-florida"
full_schedule$VenueWeb[full_schedule$Venue=="University of Washington IMA #1"] <- "university_of_washington_ima_1"
full_schedule$VenueWeb[full_schedule$Venue=="Judge Memorial High School"] <- "judge_memorial_catholic"
full_schedule$VenueWeb[full_schedule$Venue=="TCU Sports Club Field"] <- "tcu-sports-club-field"
full_schedule$VenueWeb[full_schedule$Venue=="CEFCU Stadium (SJSU)"] <- "CEFCU%20Stadium"
full_schedule$VenueWeb[full_schedule$Venue=="St Mary's College"] <- "st_marys"
full_schedule$VenueWeb[full_schedule$Venue=="Washington University - St. Louis"] <- "washington_university"
full_schedule$VenueWeb[full_schedule$Venue=="Georgia Tech Roe Stamps Field"] <- "ga_tech_roe_stamps_field"
full_schedule$VenueWeb[full_schedule$Venue=="Colorado State University-Pueblo"] <- "colorado_state_universitypueblo"
full_schedule$VenueWeb[full_schedule$Venue=="TCU Intramural Field"] <- "tcu-intramural-field"
full_schedule$VenueWeb[full_schedule$Venue=="St. Edwards"] <- "st_edwards"
full_schedule$VenueWeb[full_schedule$Venue=="University of Tennessee"] <- "university_tennessee"
full_schedule$VenueWeb[full_schedule$Venue=="Tarleton State University"] <- "tarleton_state"
full_schedule$VenueWeb[full_schedule$Venue=="Ford Center"] <- "ford-center"
full_schedule$VenueWeb[full_schedule$Venue=="Southeastern Soccer Complex (SELU)"] <- "southeastern-soccer-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Dacotah Field"] <- "north_dakota_state_university"
full_schedule$VenueWeb[full_schedule$Venue=="NC State - Method Road Soccer Field"] <- "method_road_soccer_field"
full_schedule$VenueWeb[full_schedule$Venue=="University of Texas - San Antonio"] <- "university_of_texassan_antonio"
full_schedule$VenueWeb[full_schedule$Venue=="University of Colorado-Denver"] <- "colorado_denver"
full_schedule$VenueWeb[full_schedule$Venue=="University of Puget Sound"] <- "puget_sound"
full_schedule$VenueWeb[full_schedule$Venue=="Clark Field"] <- "clark-field"
full_schedule$VenueWeb[full_schedule$Venue=="North Park Field"] <- "north_park_fields"
full_schedule$VenueWeb[full_schedule$Venue=="UC-Irvine"] <- "ucirvine"
full_schedule$VenueWeb[full_schedule$Venue=="Washington University - South Campus"] <- "washington_university_south"
full_schedule$VenueWeb[full_schedule$Venue=="SIU Carbondale"] <- "siu-carbondale"
full_schedule$VenueWeb[full_schedule$Venue=="Keiser University"] <- "keiser-university"
full_schedule$VenueWeb[full_schedule$Venue=="Oregon - Nex-Turf Field"] <- "oregon_nex_turf_field"
full_schedule$VenueWeb[full_schedule$Venue=="Gagliardi Dome (SJU)"] <- "gaglirdi-dome-sju-"
full_schedule$VenueWeb[full_schedule$Venue=="Terry Fox Field"] <- "terry-fox-field"
full_schedule$VenueWeb[full_schedule$Venue=="Park Tudor School"] <- "park-tudor"
full_schedule$VenueWeb[full_schedule$Venue=="Texas A&M Galveston"] <- "texas_a_m_galveston"
full_schedule$VenueWeb[full_schedule$Venue=="NAU South Field Recreation Complex"] <- "nau_south_intramural_fields"
full_schedule$VenueWeb[full_schedule$Venue=="Stonehill College - WB Mason Stadium"] <- "stonehill_college_wb_mason_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="University of Minnesota (bubble)"] <- "minnesota_bubble"
full_schedule$VenueWeb[full_schedule$Venue=="University Stadium - Chico"] <- "chico"
full_schedule$VenueWeb[full_schedule$Venue=="University of Missouri S&T"] <- "university_of_missouri_st"
full_schedule$VenueWeb[full_schedule$Venue=="Stephen F. Austin State University"] <- "stephen_f_austin_state_university"
full_schedule$VenueWeb[full_schedule$Venue=="Momentum Rec Field"] <- "momentum-rec-field"
full_schedule$VenueWeb[full_schedule$Venue=="University of California - Santa Cruz"] <- "university_of_california_santa_cruz"
full_schedule$VenueWeb[full_schedule$Venue=="Intramural Club Sports Field (Monmouth, OR)"] <- "intramural_club_sports_field"
full_schedule$VenueWeb[full_schedule$Venue=="Wade King Athletic Center"] <- "wade_king"
full_schedule$VenueWeb[full_schedule$Venue=="Skyline High School"] <- "skyline-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Sozo Sports Complex"] <- "sozo-sports-complex"
full_schedule$VenueWeb[full_schedule$Venue=="SUNY-Purchase"] <- "suny_purchase"
full_schedule$VenueWeb[full_schedule$Venue=="University of Colorado (Colorado Springs)"] <- "university_of_colorado_colorado_springs"
full_schedule$VenueWeb[full_schedule$Venue=="Swensen Field"] <- "bridgewater_state_university"
full_schedule$VenueWeb[full_schedule$Venue=="Strawberry Stadium"] <- "strawberry-stadium"
full_schedule$VenueWeb[full_schedule$Venue=="Chapman Stadium"] <- "chapman"
full_schedule$VenueWeb[full_schedule$Venue=="Arizona State University - West Campus"] <- "asu_west"
full_schedule$VenueWeb[full_schedule$Venue=="Cromwell Field"] <- "usc"
full_schedule$VenueWeb[full_schedule$Venue=="Segerstrom High School"] <- "segerstrom-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Johnson Hagood Stadium"] <- "johnson_hagood"
full_schedule$VenueWeb[full_schedule$Venue=="Argo Field"] <- "argo-field"
full_schedule$VenueWeb[full_schedule$Venue=="Liberty Men's Lacrosse Field"] <- "liberty_mens_lacrosse_field"
full_schedule$VenueWeb[full_schedule$Venue=="Oaks Christian High School"] <- "oaks-christian-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Spence Eccles Ogden Community Sports Complex"] <- "spence-eccles-ogden-community-sports-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Central Florida - RWC Park"] <- "rwc-park-"
full_schedule$VenueWeb[full_schedule$Venue=="Vanderbilt - Rec Field 2"] <- "vanderbilt-rec-field-2"
full_schedule$VenueWeb[full_schedule$Venue=="Alabama Intramural Fields FIeld #1"] <- "alabama_intramural_fields"
full_schedule$VenueWeb[full_schedule$Venue=="University of Utah"] <- "u_utah"
full_schedule$VenueWeb[full_schedule$Venue=="UC-San Diego"] <- "ucsan_diego"
full_schedule$VenueWeb[full_schedule$Venue=="Oconee County High School"] <- "oconee-county-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="USU Legacy Fields"] <- "usu_legacy"
full_schedule$VenueWeb[full_schedule$Venue=="Moakley Park"] <- "moakley-park"
full_schedule$VenueWeb[full_schedule$Venue=="Dick's Sporting Goods Park Field 8"] <- "dicks_sporting_goods_park_field_8"
full_schedule$VenueWeb[full_schedule$Venue=="University of Nevada - Reno"] <- "university_of_nevada__reno"
full_schedule$VenueWeb[full_schedule$Venue=="LSU Urec Fields"] <- "lsa_urec_fields"
full_schedule$VenueWeb[full_schedule$Venue=="Brigham Young University - North Field"] <- "brigham_young_university_north_field"
full_schedule$VenueWeb[full_schedule$Venue=="Regis Jesuit High School"] <- "regis-jesuit-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Colorado State University-Ft. Collins"] <- "colorado_state_universityft_collins"
full_schedule$VenueWeb[full_schedule$Venue=="University of Notre Dame"] <- "notre_dame"
full_schedule$VenueWeb[full_schedule$Venue=="Greenbrier East High School"] <- "greenbrier-east-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="DE Turf Complex"] <- "de-turf-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Indiana University - Evan Williams (Grass)"] <- "indiana-university-evan-williams--grass-"
full_schedule$VenueWeb[full_schedule$Venue=="Pacific Grove High School"] <- "pacific-grove-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="University of Dallas"] <- "university_dallas"
full_schedule$VenueWeb[full_schedule$Venue=="Indiana University - Rec Sports Complex (Turf)"] <- "indiana-university-rec-sports-complex--turf-"
full_schedule$VenueWeb[full_schedule$Venue=="John Coughlin Memorial Field"] <- "coughlin"
full_schedule$VenueWeb[full_schedule$Venue=="Maple Grove Sports Dome"] <- "maple-grove-sports-dome"
full_schedule$VenueWeb[full_schedule$Venue=="Eastern Florida State College"] <- "eastern-florida-state-college"
full_schedule$VenueWeb[full_schedule$Venue=="Georgia Club Sports Complex"] <- "georgia_rec_complex"
full_schedule$VenueWeb[full_schedule$Venue=="University of Buffalo"] <- "buffalo"
full_schedule$VenueWeb[full_schedule$Venue=="Sierra Nevada College"] <- "sierra_nevada"
full_schedule$VenueWeb[full_schedule$Venue=="Duck Samford (Auburn)"] <- "duck-samford-auburn-"
full_schedule$VenueWeb[full_schedule$Venue=="O'Shaughnessy Stadium"] <- "oshaughnessy_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="West Virginia - University HS"] <- "west-virginia-university-hs"
full_schedule$VenueWeb[full_schedule$Venue=="South Barrington Park District"] <- "south_barrington_park"
full_schedule$VenueWeb[full_schedule$Venue=="George Pierce Park"] <- "george-pierce-park"
full_schedule$VenueWeb[full_schedule$Venue=="TCF Bank Stadium"] <- "tcf_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="Ames - Field 1"] <- "ames_field_1"
full_schedule$VenueWeb[full_schedule$Venue=="Salt Lake City RAC"] <- "salt-lake-city-rac"
full_schedule$VenueWeb[full_schedule$Venue=="Foothill High School"] <- "foothill-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Robb Athletic Field"] <- "robb-athletic-field"
full_schedule$VenueWeb[full_schedule$Venue=="Lorene Rogers MIddle School"] <- "lorene-rogers-middle-school"
full_schedule$VenueWeb[full_schedule$Venue=="Lafayette High School"] <- "lafayette-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Prosper High School"] <- "prosper-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Putnam City North Football Stadium"] <- "putnam-city-north-football-stadium"
full_schedule$VenueWeb[full_schedule$Venue=="Wake Forest-Kentner Stadium"] <- "kentner_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="Seckman High School"] <- "seckman-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Fleming Fields"] <- "fleming-fields"
full_schedule$VenueWeb[full_schedule$Venue=="Prairie View High School"] <- "prairie-view-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Aloha High School"] <- "aloha-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Santa Cruz High School"] <- "aloha-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Fort Missoula Regional Park"] <- "fort-missoula-regional-park"
full_schedule$VenueWeb[full_schedule$Venue=="Willow Springs Middle School"] <- "willow-springs-middle-school"
full_schedule$VenueWeb[full_schedule$Venue=="Narragansett High School"] <- "narragansett-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Huntinton Beach High School"] <- "huntinton_high_school"
full_schedule$VenueWeb[full_schedule$Venue=="Morgan Hill Outdoor Sports Complex"] <- "morgan-hill-outdoor-sports-complex"



readVenueTable <- function(i){
  venue_URL <- paste0("http://mcla.us/venue/", full_schedule$VenueWeb[i])
  venue_webpage <-htmlParse(venue_URL)
  table <- readHTMLTable(venue_webpage, header=TRUE, which=1, stringsAsFactors=FALSE)
  return(table)
}

full_schedule$GameType <- "Home"
for(i in 1:nrow(full_schedule)){
  thispage <- tryCatch(readVenueTable(i), 
                       error = function(e) print(i) )
  if(is.numeric(thispage) || nrow(thispage) < 5 ){
    print(full_schedule$Venue[i])
    fields_home_team <- "unknown"
  } else {
    fields_home_team <- names(which.max(table(thispage$Home)))
  }
  
  if(full_schedule$Home[i] != fields_home_team  ){
    full_schedule$GameType[i] <- "Neutral"
  }
}

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


mcla2018todate <- cbind(full_schedule, scores)
