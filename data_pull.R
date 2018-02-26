
# MCLA 2018 Data (to date)

# webscraper

library(XML)
library(plyr)
library(stringr)
library(dplyr)

# Games and Results

# http://mcla.us/schedule/20187?page=1

full_schedule<-NULL

full_schedule <- ldply(1:22, getSchedulePage)

getSchedulePage <- function(i){
  # create the webpage url using paste
  schedule_url <- paste0("http://mcla.us/schedule/2018?page=", i)
  # read webpage and store in memory
  schedule_html <- htmlParse(schedule_url)
  # create R dataset from webpage contents
  schedule <-readHTMLTable(schedule_html, header=TRUE, which=1, stringsAsFactors=FALSE)
  # is this a page of results or future games
  if(!"Score" %in% names(schedule)){
    schedule$Score<-"N/A"
  }
  return(schedule)
}

# filter games not yet played
#full_schedule<-subset(full_schedule,Score!="N/A")

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
full_schedule$VenueWeb<-gsub(' ', '_', full_schedule$Venue)


full_schedule$VenueWeb[full_schedule$Venue=="Smithson Valley High School"]<-"smithson_high_school"
full_schedule$VenueWeb[full_schedule$Venue=="University of Texas - Dallas"]<-"university_of_texasdallas"
full_schedule$VenueWeb[full_schedule$Venue=="Instituto Politecnico Nacional"]<-"instituto-politecnico-nacional"
full_schedule$VenueWeb[full_schedule$Venue=="UC-Santa Barbara"]<-"ucsanta_barbara"
full_schedule$VenueWeb[full_schedule$Venue=="Joe Aillet Stadium"]<-"Aillet"
full_schedule$VenueWeb[full_schedule$Venue=="Veterans Park"]<-"veterans-park"
full_schedule$VenueWeb[full_schedule$Venue=="Florida Atlantic University"]<-"florida_atlantic"
full_schedule$VenueWeb[full_schedule$Venue=="University of Arkansas"]<-"rec_services_grass_field_fayetteville"
full_schedule$VenueWeb[full_schedule$Venue=="Allison North Stadium"]<-"allison-north-stadium"
full_schedule$VenueWeb[full_schedule$Venue=="E. Washington Field Complex"]<-"e-washington-field-complex"
full_schedule$VenueWeb[full_schedule$Venue=="UNC-Charlotte"]<-"unccharlotte"
full_schedule$VenueWeb[full_schedule$Venue=="University of California - Berkeley"]<-"university_of_california_berkeley"
full_schedule$VenueWeb[full_schedule$Venue=="Prestonwood Christian Academy"]<-"prestonwood-christian-academy"
full_schedule$VenueWeb[full_schedule$Venue=="Virginia Tech - The Marching Virginians Center"]<-"the-marching-virginians-center"
full_schedule$VenueWeb[full_schedule$Venue=="Cusabo Nation Lacrosse Complex"]<-"cusabo-nation-lacrosse-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Vanderbilt - Natchez Trace Field"]<-"natchez-field"
full_schedule$VenueWeb[full_schedule$Venue=="Wake Forest - Water Tower Field"]<-"water-tower-field"
full_schedule$VenueWeb[full_schedule$Venue=="Auburn Intramural Fields"]<-"auburn-intramural-fields"
full_schedule$VenueWeb[full_schedule$Venue=="Johnny Downs Sports Complex"]<-"johnny-downs-sports-complex"
full_schedule$VenueWeb[full_schedule$Venue=="South Carolina-Bluff Road Practice Fields"]<-"bluff-road-practice-fields"
full_schedule$VenueWeb[full_schedule$Venue=="T L Hanna High School"]<-"t-l-hanna-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Emory - Candler Park"]<-"candler"
full_schedule$VenueWeb[full_schedule$Venue=="University of Portland"]<-"portland"
full_schedule$VenueWeb[full_schedule$Venue=="UC-Irvine Field 2"]<-"ucirvine_field2"
full_schedule$VenueWeb[full_schedule$Venue=="Joint Venture Park"]<-"joint-venture-park"
full_schedule$VenueWeb[full_schedule$Venue=="Cal State-Long Beach"]<-"cal_statelong_beach"
full_schedule$VenueWeb[full_schedule$Venue=="Hidden Creek Polo Fields"]<-"hidden-creek-polo-fields"
full_schedule$VenueWeb[full_schedule$Venue=="Aurora Sports Park"]<-"aurora-sports-park"
full_schedule$VenueWeb[full_schedule$Venue=="Tennessee-Sutherland Fields"]<-"sutherland_fields"
full_schedule$VenueWeb[full_schedule$Venue=="Mulligan Field (Gonzaga)"]<-"mulligan_field_gonzaga"
full_schedule$VenueWeb[full_schedule$Venue=="Camas High School"]<-"camas-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Eastern Carver County Athletic Center"]<-"eastern-carver-county-athletic-center"
full_schedule$VenueWeb[full_schedule$Venue=="Concordia University"]<-"concordia_university_ca"
full_schedule$VenueWeb[full_schedule$Venue=="Texas A & M"]<-"texas_a__m"
full_schedule$VenueWeb[full_schedule$Venue=="North Florida"]<-"north-florida"
full_schedule$VenueWeb[full_schedule$Venue=="University of Washington IMA #1"]<-"university_of_washington_ima_1"
full_schedule$VenueWeb[full_schedule$Venue=="Judge Memorial High School"]<-"judge_memorial_catholic"
full_schedule$VenueWeb[full_schedule$Venue=="TCU Sports Club Field"]<-"tcu-sports-club-field"
full_schedule$VenueWeb[full_schedule$Venue=="CEFCU Stadium (SJSU)"]<-"CEFCU%20Stadium"
full_schedule$VenueWeb[full_schedule$Venue=="St Mary's College"]<-"st_marys"
full_schedule$VenueWeb[full_schedule$Venue=="Washington University - St. Louis"]<-"washington_university"
full_schedule$VenueWeb[full_schedule$Venue=="Georgia Tech Roe Stamps Field"]<-"ga_tech_roe_stamps_field"
full_schedule$VenueWeb[full_schedule$Venue=="Colorado State University-Pueblo"]<-"colorado_state_universitypueblo"
full_schedule$VenueWeb[full_schedule$Venue=="TCU Intramural Field"]<-"tcu-intramural-field"
full_schedule$VenueWeb[full_schedule$Venue=="St. Edwards"]<-"st_edwards"
full_schedule$VenueWeb[full_schedule$Venue=="University of Tennessee"]<-"university_tennessee"
full_schedule$VenueWeb[full_schedule$Venue=="Tarleton State University"]<-"tarleton_state"
full_schedule$VenueWeb[full_schedule$Venue=="Ford Center"]<-"ford-center"
full_schedule$VenueWeb[full_schedule$Venue=="Southeastern Soccer Complex (SELU)"]<-"southeastern-soccer-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Dacotah Field"]<-"north_dakota_state_university"
full_schedule$VenueWeb[full_schedule$Venue=="NC State - Method Road Soccer Field"]<-"method_road_soccer_field"
full_schedule$VenueWeb[full_schedule$Venue=="University of Texas - San Antonio"]<-"university_of_texassan_antonio"
full_schedule$VenueWeb[full_schedule$Venue=="University of Colorado-Denver"]<-"colorado_denver"
full_schedule$VenueWeb[full_schedule$Venue=="University of Puget Sound"]<-"puget_sound"
full_schedule$VenueWeb[full_schedule$Venue=="Clark Field"]<-"clark-field"
full_schedule$VenueWeb[full_schedule$Venue=="North Park Field"]<-"north_park_fields"
full_schedule$VenueWeb[full_schedule$Venue=="UC-Irvine"]<-"ucirvine"
full_schedule$VenueWeb[full_schedule$Venue=="Washington University - South Campus"]<-"washington_university_south"
full_schedule$VenueWeb[full_schedule$Venue=="SIU Carbondale"]<-"siu-carbondale"
full_schedule$VenueWeb[full_schedule$Venue=="Keiser University"]<-"keiser-university"
full_schedule$VenueWeb[full_schedule$Venue=="Oregon - Nex-Turf Field"]<-"oregon_nex_turf_field"
full_schedule$VenueWeb[full_schedule$Venue=="Gagliardi Dome (SJU)"]<-"gaglirdi-dome-sju-"
full_schedule$VenueWeb[full_schedule$Venue=="Terry Fox Field"]<-"terry-fox-field"
full_schedule$VenueWeb[full_schedule$Venue=="Park Tudor School"]<-"park-tudor"
full_schedule$VenueWeb[full_schedule$Venue=="Texas A&M Galveston"]<-"texas_a_m_galveston"
full_schedule$VenueWeb[full_schedule$Venue=="NAU South Field Recreation Complex"]<-"nau_south_intramural_fields"
full_schedule$VenueWeb[full_schedule$Venue=="Stonehill College - WB Mason Stadium"]<-"stonehill_college_wb_mason_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="University of Minnesota (bubble)"]<-"minnesota_bubble"
full_schedule$VenueWeb[full_schedule$Venue=="University Stadium - Chico"]<-"chico"
full_schedule$VenueWeb[full_schedule$Venue=="University of Missouri S&T"]<-"university_of_missouri_st"
full_schedule$VenueWeb[full_schedule$Venue=="Stephen F. Austin State University"]<-"stephen_f_austin_state_university"
full_schedule$VenueWeb[full_schedule$Venue=="Momentum Rec Field"]<-"momentum-rec-field"
full_schedule$VenueWeb[full_schedule$Venue=="University of California - Santa Cruz"]<-"university_of_california_santa_cruz"
full_schedule$VenueWeb[full_schedule$Venue=="Intramural Club Sports Field (Monmouth, OR)"]<-"intramural_club_sports_field"
full_schedule$VenueWeb[full_schedule$Venue=="Wade King Athletic Center"]<-"wade_king"
full_schedule$VenueWeb[full_schedule$Venue=="Skyline High School"]<-"skyline-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Sozo Sports Complex"]<-"sozo-sports-complex"
full_schedule$VenueWeb[full_schedule$Venue=="SUNY-Purchase"]<-"suny_purchase"
full_schedule$VenueWeb[full_schedule$Venue=="University of Colorado (Colorado Springs)"]<-"university_of_colorado_colorado_springs"
full_schedule$VenueWeb[full_schedule$Venue=="Swensen Field"]<-"bridgewater_state_university"
full_schedule$VenueWeb[full_schedule$Venue=="Strawberry Stadium"]<-"strawberry-stadium"
full_schedule$VenueWeb[full_schedule$Venue=="Chapman Stadium"]<-"chapman"
full_schedule$VenueWeb[full_schedule$Venue=="Arizona State University - West Campus"]<-"asu_west"
full_schedule$VenueWeb[full_schedule$Venue=="Cromwell Field"]<-"usc"
full_schedule$VenueWeb[full_schedule$Venue=="Segerstrom High School"]<-"segerstrom-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Johnson Hagood Stadium"]<-"johnson_hagood"
full_schedule$VenueWeb[full_schedule$Venue=="Argo Field"]<-"argo-field"
full_schedule$VenueWeb[full_schedule$Venue=="Liberty Men's Lacrosse Field"]<-"liberty_mens_lacrosse_field"
full_schedule$VenueWeb[full_schedule$Venue=="Oaks Christian High School"]<-"oaks-christian-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Spence Eccles Ogden Community Sports Complex"]<-"spence-eccles-ogden-community-sports-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Central Florida - RWC Park"]<-"rwc-park-"
full_schedule$VenueWeb[full_schedule$Venue=="Vanderbilt - Rec Field 2"]<-"vanderbilt-rec-field-2"
full_schedule$VenueWeb[full_schedule$Venue=="Alabama Intramural Fields FIeld #1"]<-"alabama_intramural_fields"
full_schedule$VenueWeb[full_schedule$Venue=="University of Utah"]<-"u_utah"
full_schedule$VenueWeb[full_schedule$Venue=="UC-San Diego"]<-"ucsan_diego"
full_schedule$VenueWeb[full_schedule$Venue=="Oconee County High School"]<-"oconee-county-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="USU Legacy Fields"]<-"usu_legacy"
full_schedule$VenueWeb[full_schedule$Venue=="Moakley Park"]<-"moakley-park"
full_schedule$VenueWeb[full_schedule$Venue=="Dick's Sporting Goods Park Field 8"]<-"dicks_sporting_goods_park_field_8"
full_schedule$VenueWeb[full_schedule$Venue=="University of Nevada - Reno"]<-"university_of_nevada__reno"
full_schedule$VenueWeb[full_schedule$Venue=="LSU Urec Fields"]<-"lsa_urec_fields"
full_schedule$VenueWeb[full_schedule$Venue=="Brigham Young University - North Field"]<-"brigham_young_university_north_field"
full_schedule$VenueWeb[full_schedule$Venue=="Regis Jesuit High School"]<-"regis-jesuit-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Colorado State University-Ft. Collins"]<-"colorado_state_universityft_collins"
full_schedule$VenueWeb[full_schedule$Venue=="University of Notre Dame"]<-"notre_dame"
full_schedule$VenueWeb[full_schedule$Venue=="Greenbrier East High School"]<-"greenbrier-east-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="DE Turf Complex"]<-"de-turf-complex"
full_schedule$VenueWeb[full_schedule$Venue=="Indiana University - Evan Williams (Grass)"]<-"indiana-university-evan-williams--grass-"
full_schedule$VenueWeb[full_schedule$Venue=="Pacific Grove High School"]<-"pacific-grove-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="University of Dallas"]<-"university_dallas"
full_schedule$VenueWeb[full_schedule$Venue=="Indiana University - Rec Sports Complex (Turf)"]<-"indiana-university-rec-sports-complex--turf-"
full_schedule$VenueWeb[full_schedule$Venue=="John Coughlin Memorial Field"]<-"coughlin"
full_schedule$VenueWeb[full_schedule$Venue=="Maple Grove Sports Dome"]<-"maple-grove-sports-dome"
full_schedule$VenueWeb[full_schedule$Venue=="Eastern Florida State College"]<-"eastern-florida-state-college"
full_schedule$VenueWeb[full_schedule$Venue=="Georgia Club Sports Complex"]<-"georgia_rec_complex"
full_schedule$VenueWeb[full_schedule$Venue=="University of Buffalo"]<-"buffalo"
full_schedule$VenueWeb[full_schedule$Venue=="Sierra Nevada College"]<-"sierra_nevada"
full_schedule$VenueWeb[full_schedule$Venue=="Duck Samford (Auburn)"]<-"duck-samford-auburn-"
full_schedule$VenueWeb[full_schedule$Venue=="O'Shaughnessy Stadium"]<-"oshaughnessy_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="West Virginia - University HS"]<-"west-virginia-university-hs"
full_schedule$VenueWeb[full_schedule$Venue=="South Barrington Park District"]<-"south_barrington_park"
full_schedule$VenueWeb[full_schedule$Venue=="George Pierce Park"]<-"george-pierce-park"
full_schedule$VenueWeb[full_schedule$Venue=="TCF Bank Stadium"]<-"tcf_stadium"
full_schedule$VenueWeb[full_schedule$Venue=="Ames - Field 1"]<-"ames_field_1"
full_schedule$VenueWeb[full_schedule$Venue=="Salt Lake City RAC"]<-"salt-lake-city-rac"
full_schedule$VenueWeb[full_schedule$Venue=="Foothill High School"]<-"foothill-high-school"
full_schedule$VenueWeb[full_schedule$Venue=="Robb Athletic Field"]<-"robb-athletic-field"
full_schedule$VenueWeb[full_schedule$Venue=="Lorene Rogers MIddle School"]<-"lorene-rogers-middle-school"


readVenueTable <- function(i){
  venue_URL <- paste0("http://mcla.us/venue/", full_schedule$VenueWeb[i])
  venue_webpage <-htmlParse(venue_URL)
  table <- readHTMLTable(venue_webpage, header=TRUE, which=1, stringsAsFactors=FALSE)
  return(table)
}

full_schedule$GameType<-"Home"
for(i in 1:nrow(full_schedule)){
  thispage <- tryCatch(readVenueTable(i), error = function(e) print(i))
  
  if(is.numeric(thispage)){
    fields_home_team <- "unknown"
  } else {
    fields_home_team <- names(which.max(table(thispage$Home)))
  }
  
  if(full_schedule$Home[i] != fields_home_team
     | nrow(thispage) < 5 ){full_schedule$GameType[i] <- "Neutral"}
}




# handle some special cases where logic says Neutral but is really Homefield
full_schedule$GameType[full_schedule$Home=="Virginia Tech"
                   & full_schedule$Venue=="Virginia Tech - The Marching Virginians Center"]<-"Home"
full_schedule$GameType[full_schedule$Home=="Texas Christian"
                   & full_schedule$Venue=="TCU Intramural Field"]<-"Home"
full_schedule$GameType[full_schedule$Home=="Southeastern Louisiana"
                   & full_schedule$Venue=="Southeastern Soccer Complex (SELU)"]<-"Home"
full_schedule$GameType[full_schedule$Venue=="North Park Field"]<-"Neutral"
full_schedule$GameType[full_schedule$Home=="Southern Illinois"
                   & full_schedule$Venue=="SIU Carbondale"]<-"Home"
full_schedule$GameType[full_schedule$Home=="Keiser University"
                   & full_schedule$Venue=="Keiser University"]<-"Home"
full_schedule$GameType[full_schedule$Home=="SUNY Purchase"
                   & full_schedule$Venue=="SUNY-Purchase"]<-"Home"
full_schedule$GameType[full_schedule$Home=="UC Colorado Springs"
                   & full_schedule$Venue=="University of Colorado (Colorado Springs)"]<-"Home"
full_schedule$GameType[full_schedule$Home=="Southeastern Louisiana"
                   & full_schedule$Venue=="Strawberry Stadium"]<-"Home"
full_schedule$GameType[full_schedule$Home=="Weber State"
                   & full_schedule$Venue=="Spence Eccles Ogden Community Sports Complex"]<-"Home"
full_schedule$GameType[full_schedule$Venue=="Vanderbilt - Rec Field 2"]<-"Neutral"
full_schedule$GameType[full_schedule$Home=="Auburn"
                   & full_schedule$Venue=="Duck Samford (Auburn)"]<-"Home"
full_schedule$GameType[full_schedule$Home=="West Virginia"
                   & full_schedule$Venue=="West Virginia - University HS"]<-"Home"
full_schedule$GameType[full_schedule$Venue=="South Barrington Park District"]<-"Neutral"


mcla2018todate<-full_schedule

# data to investigate
# 0-0
# 1-0 or 0-1

# how to identify home/away/neutral field

# how to distinguish Div 1 & Div 2

# how to distinguish conf games and non-conf games


# can we get the scoring by quarter?

# is there a sequence that includes all 2017 games?
# ? 19240 ? (first game in 2017 list: 20007) (first BYU game 18873) (18693 but 18704 has no page)
#   to 19992 (championship game)
#   BYU-CU RMLC game: 20062

all.game.qtrs<-NULL

for(i in 20141:21400){   
  # create the webpage url using paste
  thispage.url<-paste("http://mcla.us/game/",i,sep="")
  # read webpage and store in memory
  thispage.webpage<-htmlParse(thispage.url)
  # check to see if this is a game ID that has a table of statistics
  thispage<-readHTMLTable(thispage.webpage)
  if(length(thispage)>1){
    print(i)
    # create R dataset from webpage contents
    thispage<-readHTMLTable(thispage.webpage, 
                            header=TRUE,which=1,stringsAsFactors=FALSE)
    # away team is first row, home team is second row
    all.game.qtrs<-rbind(all.game.qtrs,
                         data.frame(Away=thispage[1,1],
                                    Away.q1=as.numeric(thispage[1,2]),
                                    Away.q2=as.numeric(thispage[1,3]),
                                    Away.q3=as.numeric(thispage[1,4]),
                                    Away.q4=as.numeric(thispage[1,5]),Away.ot=as.numeric(thispage[1,6]),
                                    AwayGoals=sum(as.numeric(thispage[1,2:6])),
                                    Home=thispage[2,1],
                                    Home.q1=as.numeric(thispage[2,2]),
                                    Home.q2=as.numeric(thispage[2,3]),
                                    Home.q3=as.numeric(thispage[2,4]),
                                    Home.q4=as.numeric(thispage[2,5]),Home.ot=as.numeric(thispage[2,6]),
                                    HomeGoals=sum(as.numeric(thispage[2,2:6])) ) )
    
  }
  
}

# convert Home and Away to character
all.game.qtrs$Away<-as.character(all.game.qtrs$Away)
all.game.qtrs$Home<-as.character(all.game.qtrs$Home)

mcla2018todate.qtrs<-all.game.qtrs



# merge quarterly detail

test1<-mcla2018todate[with(mcla2018todate,order(Home,Away,HomeGoals,AwayGoals)),]

test2<-mcla2018todate.qtrs

# Inconsistencies: "BYU in game webpages but "Brigham Young" in schedule webpages
test2$Home[test2$Home=="BYU"]<-"Brigham Young"
test2$Away[test2$Away=="BYU"]<-"Brigham Young"
test2$Home[test2$Home=="Bridgewater"]<-"Bridgewater State"
test2$Away[test2$Away=="Bridgewater"]<-"Bridgewater State"
test2$Home[test2$Home=="Channel Islands"]<-"Cal State Channel Islands"
test2$Away[test2$Away=="Channel Islands"]<-"Cal State Channel Islands"
test2$Home[test2$Home=="Fullerton"]<-"Cal State Fullerton"
test2$Away[test2$Away=="Fullerton"]<-"Cal State Fullerton"
test2$Home[test2$Home=="Central Connecticut"]<-"Central Conn. State"
test2$Away[test2$Away=="Central Connecticut"]<-"Central Conn. State"
test2$Home[test2$Home=="The Citadel"]<-"Citadel"
test2$Away[test2$Away=="The Citadel"]<-"Citadel"
test2$Home[test2$Home=="CU-Denver"]<-"Colorado Denver"
test2$Away[test2$Away=="CU-Denver"]<-"Colorado Denver"
test2$Home[test2$Home=="Columbus St"]<-"Columbus State"
test2$Away[test2$Away=="Columbus St"]<-"Columbus State"
test2$Home[test2$Home=="Concordia"]<-"Concordia-Irvine"
test2$Away[test2$Away=="Concordia"]<-"Concordia-Irvine"
test2$Home[test2$Home=="FAU"]<-"Florida Atlantic"
test2$Away[test2$Away=="FAU"]<-"Florida Atlantic"
test2$Home[test2$Home=="FGCU"]<-"Florida Gulf Coast"
test2$Away[test2$Away=="FGCU"]<-"Florida Gulf Coast"
test2$Home[test2$Home=="Framingham"]<-"Framingham State"
test2$Away[test2$Away=="Framingham"]<-"Framingham State"
test2$Home[test2$Home=="GVSU"]<-"Grand Valley State"
test2$Away[test2$Away=="GVSU"]<-"Grand Valley State"
test2$Home[test2$Home=="JWU-Denver"]<-"Johnson & Wales"
test2$Away[test2$Away=="JWU-Denver"]<-"Johnson & Wales"
test2$Home[test2$Home=="La. Tech"]<-"Louisiana Tech"
test2$Away[test2$Away=="La. Tech"]<-"Louisiana Tech"
test2$Home[test2$Home=="Miami (FL)"]<-"Miami"
test2$Away[test2$Away=="Miami (FL)"]<-"Miami"
test2$Home[test2$Home=="Minn-Duluth"]<-"Minn.-Duluth"
test2$Away[test2$Away=="Minn-Duluth"]<-"Minn.-Duluth"
test2$Home[test2$Home=="Mo. State"]<-"Missouri State"
test2$Away[test2$Away=="Mo. State"]<-"Missouri State"
test2$Home[test2$Home=="MSUD"]<-"MSU Denver"
test2$Away[test2$Away=="MSUD"]<-"MSU Denver"
test2$Home[test2$Home=="NDSU"]<-"North Dakota State"
test2$Away[test2$Away=="NDSU"]<-"North Dakota State"
test2$Home[test2$Home=="RHIT"]<-"Rose-Hulman"
test2$Away[test2$Away=="RHIT"]<-"Rose-Hulman"
test2$Home[test2$Home=="PBA"]<-"Palm Beach Atlantic"
test2$Away[test2$Away=="PBA"]<-"Palm Beach Atlantic"
test2$Home[test2$Home=="Southern Conn."]<-"Southern Conn. State"
test2$Away[test2$Away=="Southern Conn."]<-"Southern Conn. State"
#test2$Home[test2$Home==""]<-"Southeastern Louisiana"
#test2$Away[test2$Away==""]<-"Southeastern Louisiana"
test2$Home[test2$Home=="SIUC"]<-"Southern Illinois"
test2$Away[test2$Away=="SIUC"]<-"Southern Illinois"
test2$Home[test2$Home=="SMU"]<-"Southern Methodist"
test2$Away[test2$Away=="SMU"]<-"Southern Methodist"
test2$Home[test2$Home=="St. Johns"]<-"St. John's"
test2$Away[test2$Away=="St. Johns"]<-"St. John's"
test2$Home[test2$Home=="St. Thomas"]<-"St. Thomas"
test2$Away[test2$Away=="St. Thomas"]<-"St. Thomas"
test2$Home[test2$Home=="Tarleton St"]<-"Tarleton State"
test2$Away[test2$Away=="Tarleton St"]<-"Tarleton State"
test2$Home[test2$Home=="TAMU-CC"]<-"TAMU-Corpus Christi"
test2$Away[test2$Away=="TAMU-CC"]<-"TAMU-Corpus Christi"
test2$Home[test2$Home=="WashU"]<-"Washington (Mo.)"
test2$Away[test2$Away=="WashU"]<-"Washington (Mo.)"
test2$Home[test2$Home=="Weber St"]<-"Weber State"
test2$Away[test2$Away=="Weber St"]<-"Weber State"
test2$Home[test2$Home=="Winona"]<-"Winona State"
test2$Away[test2$Away=="Winona"]<-"Winona State"
test2$Home[test2$Home=="Wofford"]<-"Wofford College"
test2$Away[test2$Away=="Wofford"]<-"Wofford College"

test2<-test2[with(mcla2018todate.qtrs,order(Home,Away,HomeGoals,AwayGoals)),]

test3<-merge(test1,test2,by=c("Home","Away","HomeGoals","AwayGoals"),all.x=TRUE)

# clean up ... replace missing quarterly scoring with zeros?

#table(subset(test3,is.na(Home.q1),Home))

#subset(test3,Home=="Brigham Young")
#subset(test3,Home=="Palm Beach Atlantic")
#subset(test3,Home=="Southeastern Louisiana")

# Division 1 or 2?

# create the webpage url using paste
thispage.url<-"http://mcla.us/teams"
# read webpage and store in memory
thispage.webpage<-htmlParse(thispage.url)
# create R dataset from webpage contents
thispage<-readHTMLTable(thispage.webpage,stringsAsFactors=FALSE)
names.div1<-thispage[[1]]
names.div1<-data.frame(Home=names.div1[,1])
names.div1$Home<-as.character(names.div1$Home)
names.div1$Division<-1
names.div2<-thispage[[2]]
names.div2<-data.frame(Home=names.div2[,1])
names.div2$Home<-as.character(names.div2$Home)
names.div2$Division<-2

div.names<-rbind(names.div1,names.div2)
div.names<-div.names[order(div.names$Home),]

# identify home team division
test4<-merge(test3,div.names,by="Home",all.x=TRUE)
# schools with missing division
test4$Division[is.na(test4$Division)]<-0
names(test4)[22]<-"Home.Division"
# identify away team division
names(div.names)[1]<-"Away"
test5<-merge(test4,div.names,by="Away",all.x=TRUE)
# schools with missing division
test5$Division[is.na(test5$Division)]<-0
names(test5)[23]<-"Away.Division"


# final dataset
mcla2018todate<-test5[,c(1:4,6:9,11:23)]




