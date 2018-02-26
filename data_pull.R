
# MCLA 2018 Data (to date)

# webscraper

library(XML)

# Games and Results

# http://mcla.us/schedule/20187?page=1

all.pages<-NULL
for(i in 1:22){
  print(i)
  # create the webpage url using paste
  thispage.url<-paste("http://mcla.us/schedule/2018?page=",i,sep="")
  # read webpage and store in memory
  thispage.webpage<-htmlParse(thispage.url)
  # create R dataset from webpage contents
  thispage<-readHTMLTable(thispage.webpage,
                          header=TRUE,which=1,stringsAsFactors=FALSE)
  # is this a page of results or future games
  if(dim(thispage)[2]==6){
    thispage$Score<-"N/A"
  }
  # combine this page's data to the all.pages dataframe
  all.pages<-rbind(all.pages,thispage)
}

# filter games not yet played
#all.pages<-subset(all.pages,Score!="N/A")

# parse score
temp.scores<-as.numeric(unlist(strsplit(all.pages$Score,"-")))
all.pages$AwayGoals<-temp.scores[seq(1,2*length(all.pages$Score),by=2)]
all.pages$HomeGoals<-temp.scores[seq(2,2*length(all.pages$Score),by=2)]



# identify home field or neutral location

#http://mcla.us/venue/montana_state

# correct some names to their venue webpage name
all.pages$VenueWeb<-gsub(' ','_',all.pages$Venue)
all.pages$VenueWeb[all.pages$Venue=="Smithson Valley High School"]<-"smithson_high_school"
all.pages$VenueWeb[all.pages$Venue=="University of Texas - Dallas"]<-"university_of_texasdallas"
all.pages$VenueWeb[all.pages$Venue=="Instituto Politecnico Nacional"]<-"instituto-politecnico-nacional"
all.pages$VenueWeb[all.pages$Venue=="UC-Santa Barbara"]<-"ucsanta_barbara"
all.pages$VenueWeb[all.pages$Venue=="Joe Aillet Stadium"]<-"Aillet"
all.pages$VenueWeb[all.pages$Venue=="Veterans Park"]<-"veterans-park"
all.pages$VenueWeb[all.pages$Venue=="Florida Atlantic University"]<-"florida_atlantic"
all.pages$VenueWeb[all.pages$Venue=="University of Arkansas"]<-"rec_services_grass_field_fayetteville"
all.pages$VenueWeb[all.pages$Venue=="Allison North Stadium"]<-"allison-north-stadium"
all.pages$VenueWeb[all.pages$Venue=="E. Washington Field Complex"]<-"e-washington-field-complex"
all.pages$VenueWeb[all.pages$Venue=="UNC-Charlotte"]<-"unccharlotte"
all.pages$VenueWeb[all.pages$Venue=="University of California - Berkeley"]<-"university_of_california_berkeley"
all.pages$VenueWeb[all.pages$Venue=="Prestonwood Christian Academy"]<-"prestonwood-christian-academy"
all.pages$VenueWeb[all.pages$Venue=="Virginia Tech - The Marching Virginians Center"]<-"the-marching-virginians-center"
all.pages$VenueWeb[all.pages$Venue=="Cusabo Nation Lacrosse Complex"]<-"cusabo-nation-lacrosse-complex"
all.pages$VenueWeb[all.pages$Venue=="Vanderbilt - Natchez Trace Field"]<-"natchez-field"
all.pages$VenueWeb[all.pages$Venue=="Wake Forest - Water Tower Field"]<-"water-tower-field"
all.pages$VenueWeb[all.pages$Venue=="Auburn Intramural Fields"]<-"auburn-intramural-fields"
all.pages$VenueWeb[all.pages$Venue=="Johnny Downs Sports Complex"]<-"johnny-downs-sports-complex"
all.pages$VenueWeb[all.pages$Venue=="South Carolina-Bluff Road Practice Fields"]<-"bluff-road-practice-fields"
all.pages$VenueWeb[all.pages$Venue=="T L Hanna High School"]<-"t-l-hanna-high-school"
all.pages$VenueWeb[all.pages$Venue=="Emory - Candler Park"]<-"candler"
all.pages$VenueWeb[all.pages$Venue=="University of Portland"]<-"portland"
all.pages$VenueWeb[all.pages$Venue=="UC-Irvine Field 2"]<-"ucirvine_field2"
all.pages$VenueWeb[all.pages$Venue=="Joint Venture Park"]<-"joint-venture-park"
all.pages$VenueWeb[all.pages$Venue=="Cal State-Long Beach"]<-"cal_statelong_beach"
all.pages$VenueWeb[all.pages$Venue=="Hidden Creek Polo Fields"]<-"hidden-creek-polo-fields"
all.pages$VenueWeb[all.pages$Venue=="Aurora Sports Park"]<-"aurora-sports-park"
all.pages$VenueWeb[all.pages$Venue=="Tennessee-Sutherland Fields"]<-"sutherland_fields"
all.pages$VenueWeb[all.pages$Venue=="Mulligan Field (Gonzaga)"]<-"mulligan_field_gonzaga"
all.pages$VenueWeb[all.pages$Venue=="Camas High School"]<-"camas-high-school"
all.pages$VenueWeb[all.pages$Venue=="Eastern Carver County Athletic Center"]<-"eastern-carver-county-athletic-center"
all.pages$VenueWeb[all.pages$Venue=="Concordia University"]<-"concordia_university_ca"
all.pages$VenueWeb[all.pages$Venue=="Texas A & M"]<-"texas_a__m"
all.pages$VenueWeb[all.pages$Venue=="North Florida"]<-"north-florida"
all.pages$VenueWeb[all.pages$Venue=="University of Washington IMA #1"]<-"university_of_washington_ima_1"
all.pages$VenueWeb[all.pages$Venue=="Judge Memorial High School"]<-"judge_memorial_catholic"
all.pages$VenueWeb[all.pages$Venue=="TCU Sports Club Field"]<-"tcu-sports-club-field"
all.pages$VenueWeb[all.pages$Venue=="CEFCU Stadium (SJSU)"]<-"CEFCU%20Stadium"
all.pages$VenueWeb[all.pages$Venue=="St Mary's College"]<-"st_marys"
all.pages$VenueWeb[all.pages$Venue=="Washington University - St. Louis"]<-"washington_university"
all.pages$VenueWeb[all.pages$Venue=="Georgia Tech Roe Stamps Field"]<-"ga_tech_roe_stamps_field"
all.pages$VenueWeb[all.pages$Venue=="Colorado State University-Pueblo"]<-"colorado_state_universitypueblo"
all.pages$VenueWeb[all.pages$Venue=="TCU Intramural Field"]<-"tcu-intramural-field"
all.pages$VenueWeb[all.pages$Venue=="St. Edwards"]<-"st_edwards"
all.pages$VenueWeb[all.pages$Venue=="University of Tennessee"]<-"university_tennessee"
all.pages$VenueWeb[all.pages$Venue=="Tarleton State University"]<-"tarleton_state"
all.pages$VenueWeb[all.pages$Venue=="Ford Center"]<-"ford-center"
all.pages$VenueWeb[all.pages$Venue=="Southeastern Soccer Complex (SELU)"]<-"southeastern-soccer-complex"
all.pages$VenueWeb[all.pages$Venue=="Dacotah Field"]<-"north_dakota_state_university"
all.pages$VenueWeb[all.pages$Venue=="NC State - Method Road Soccer Field"]<-"method_road_soccer_field"
all.pages$VenueWeb[all.pages$Venue=="University of Texas - San Antonio"]<-"university_of_texassan_antonio"
all.pages$VenueWeb[all.pages$Venue=="University of Colorado-Denver"]<-"colorado_denver"
all.pages$VenueWeb[all.pages$Venue=="University of Puget Sound"]<-"puget_sound"
all.pages$VenueWeb[all.pages$Venue=="Clark Field"]<-"clark-field"
all.pages$VenueWeb[all.pages$Venue=="North Park Field"]<-"north_park_fields"
all.pages$VenueWeb[all.pages$Venue=="UC-Irvine"]<-"ucirvine"
all.pages$VenueWeb[all.pages$Venue=="Washington University - South Campus"]<-"washington_university_south"
all.pages$VenueWeb[all.pages$Venue=="SIU Carbondale"]<-"siu-carbondale"
all.pages$VenueWeb[all.pages$Venue=="Keiser University"]<-"keiser-university"
all.pages$VenueWeb[all.pages$Venue=="Oregon - Nex-Turf Field"]<-"oregon_nex_turf_field"
all.pages$VenueWeb[all.pages$Venue=="Gagliardi Dome (SJU)"]<-"gaglirdi-dome-sju-"
all.pages$VenueWeb[all.pages$Venue=="Terry Fox Field"]<-"terry-fox-field"
all.pages$VenueWeb[all.pages$Venue=="Park Tudor School"]<-"park-tudor"
all.pages$VenueWeb[all.pages$Venue=="Texas A&M Galveston"]<-"texas_a_m_galveston"
all.pages$VenueWeb[all.pages$Venue=="NAU South Field Recreation Complex"]<-"nau_south_intramural_fields"
all.pages$VenueWeb[all.pages$Venue=="Stonehill College - WB Mason Stadium"]<-"stonehill_college_wb_mason_stadium"
all.pages$VenueWeb[all.pages$Venue=="University of Minnesota (bubble)"]<-"minnesota_bubble"
all.pages$VenueWeb[all.pages$Venue=="University Stadium - Chico"]<-"chico"
all.pages$VenueWeb[all.pages$Venue=="University of Missouri S&T"]<-"university_of_missouri_st"
all.pages$VenueWeb[all.pages$Venue=="Stephen F. Austin State University"]<-"stephen_f_austin_state_university"
all.pages$VenueWeb[all.pages$Venue=="Momentum Rec Field"]<-"momentum-rec-field"
all.pages$VenueWeb[all.pages$Venue=="University of California - Santa Cruz"]<-"university_of_california_santa_cruz"
all.pages$VenueWeb[all.pages$Venue=="Intramural Club Sports Field (Monmouth, OR)"]<-"intramural_club_sports_field"
all.pages$VenueWeb[all.pages$Venue=="Wade King Athletic Center"]<-"wade_king"
all.pages$VenueWeb[all.pages$Venue=="Skyline High School"]<-"skyline-high-school"
all.pages$VenueWeb[all.pages$Venue=="Sozo Sports Complex"]<-"sozo-sports-complex"
all.pages$VenueWeb[all.pages$Venue=="SUNY-Purchase"]<-"suny_purchase"
all.pages$VenueWeb[all.pages$Venue=="University of Colorado (Colorado Springs)"]<-"university_of_colorado_colorado_springs"
all.pages$VenueWeb[all.pages$Venue=="Swensen Field"]<-"bridgewater_state_university"
all.pages$VenueWeb[all.pages$Venue=="Strawberry Stadium"]<-"strawberry-stadium"
all.pages$VenueWeb[all.pages$Venue=="Chapman Stadium"]<-"chapman"
all.pages$VenueWeb[all.pages$Venue=="Arizona State University - West Campus"]<-"asu_west"
all.pages$VenueWeb[all.pages$Venue=="Cromwell Field"]<-"usc"
all.pages$VenueWeb[all.pages$Venue=="Segerstrom High School"]<-"segerstrom-high-school"
all.pages$VenueWeb[all.pages$Venue=="Johnson Hagood Stadium"]<-"johnson_hagood"
all.pages$VenueWeb[all.pages$Venue=="Argo Field"]<-"argo-field"
all.pages$VenueWeb[all.pages$Venue=="Liberty Men's Lacrosse Field"]<-"liberty_mens_lacrosse_field"
all.pages$VenueWeb[all.pages$Venue=="Oaks Christian High School"]<-"oaks-christian-high-school"
all.pages$VenueWeb[all.pages$Venue=="Spence Eccles Ogden Community Sports Complex"]<-"spence-eccles-ogden-community-sports-complex"
all.pages$VenueWeb[all.pages$Venue=="Central Florida - RWC Park"]<-"rwc-park-"
all.pages$VenueWeb[all.pages$Venue=="Vanderbilt - Rec Field 2"]<-"vanderbilt-rec-field-2"
all.pages$VenueWeb[all.pages$Venue=="Alabama Intramural Fields FIeld #1"]<-"alabama_intramural_fields"
all.pages$VenueWeb[all.pages$Venue=="University of Utah"]<-"u_utah"
all.pages$VenueWeb[all.pages$Venue=="UC-San Diego"]<-"ucsan_diego"
all.pages$VenueWeb[all.pages$Venue=="Oconee County High School"]<-"oconee-county-high-school"
all.pages$VenueWeb[all.pages$Venue=="USU Legacy Fields"]<-"usu_legacy"
all.pages$VenueWeb[all.pages$Venue=="Moakley Park"]<-"moakley-park"
all.pages$VenueWeb[all.pages$Venue=="Dick's Sporting Goods Park Field 8"]<-"dicks_sporting_goods_park_field_8"
all.pages$VenueWeb[all.pages$Venue=="University of Nevada - Reno"]<-"university_of_nevada__reno"
all.pages$VenueWeb[all.pages$Venue=="LSU Urec Fields"]<-"lsa_urec_fields"
all.pages$VenueWeb[all.pages$Venue=="Brigham Young University - North Field"]<-"brigham_young_university_north_field"
all.pages$VenueWeb[all.pages$Venue=="Regis Jesuit High School"]<-"regis-jesuit-high-school"
all.pages$VenueWeb[all.pages$Venue=="Colorado State University-Ft. Collins"]<-"colorado_state_universityft_collins"
all.pages$VenueWeb[all.pages$Venue=="University of Notre Dame"]<-"notre_dame"
all.pages$VenueWeb[all.pages$Venue=="Greenbrier East High School"]<-"greenbrier-east-high-school"
all.pages$VenueWeb[all.pages$Venue=="DE Turf Complex"]<-"de-turf-complex"
all.pages$VenueWeb[all.pages$Venue=="Indiana University - Evan Williams (Grass)"]<-"indiana-university-evan-williams--grass-"
all.pages$VenueWeb[all.pages$Venue=="Pacific Grove High School"]<-"pacific-grove-high-school"
all.pages$VenueWeb[all.pages$Venue=="University of Dallas"]<-"university_dallas"
all.pages$VenueWeb[all.pages$Venue=="Indiana University - Rec Sports Complex (Turf)"]<-"indiana-university-rec-sports-complex--turf-"
all.pages$VenueWeb[all.pages$Venue=="John Coughlin Memorial Field"]<-"coughlin"
all.pages$VenueWeb[all.pages$Venue=="Maple Grove Sports Dome"]<-"maple-grove-sports-dome"
all.pages$VenueWeb[all.pages$Venue=="Eastern Florida State College"]<-"eastern-florida-state-college"
all.pages$VenueWeb[all.pages$Venue=="Georgia Club Sports Complex"]<-"georgia_rec_complex"
all.pages$VenueWeb[all.pages$Venue=="University of Buffalo"]<-"buffalo"
all.pages$VenueWeb[all.pages$Venue=="Sierra Nevada College"]<-"sierra_nevada"
all.pages$VenueWeb[all.pages$Venue=="Duck Samford (Auburn)"]<-"duck-samford-auburn-"
all.pages$VenueWeb[all.pages$Venue=="O'Shaughnessy Stadium"]<-"oshaughnessy_stadium"
all.pages$VenueWeb[all.pages$Venue=="West Virginia - University HS"]<-"west-virginia-university-hs"
all.pages$VenueWeb[all.pages$Venue=="South Barrington Park District"]<-"south_barrington_park"
all.pages$VenueWeb[all.pages$Venue=="George Pierce Park"]<-"george-pierce-park"
all.pages$VenueWeb[all.pages$Venue=="TCF Bank Stadium"]<-"tcf_stadium"
all.pages$VenueWeb[all.pages$Venue=="Ames - Field 1"]<-"ames_field_1"
all.pages$VenueWeb[all.pages$Venue=="Salt Lake City RAC"]<-"salt-lake-city-rac"
all.pages$VenueWeb[all.pages$Venue=="Foothill High School"]<-"foothill-high-school"
all.pages$VenueWeb[all.pages$Venue=="Robb Athletic Field"]<-"robb-athletic-field"


all.pages$GameType<-"Home"
for(i in 671:length(all.pages$Venue)){
  print(i)
  # create the webpage url using paste
  thispage.url<-paste("http://mcla.us/venue/",all.pages$VenueWeb[i],sep="")
  # read webpage and store in memory
  thispage.webpage<-htmlParse(thispage.url)
  # create R dataset from webpage contents
  thispage<-readHTMLTable(thispage.webpage,
                          header=TRUE,which=1,stringsAsFactors=FALSE)
  if(all.pages$Home[i]!=names(which.max(table(thispage$Home)))
     | dim(thispage)[1]<5 ){all.pages$GameType[i]<-"Neutral"}
}




# handle some special cases where logic says Neutral but is really Homefield
all.pages$GameType[all.pages$Home=="Virginia Tech"
                   & all.pages$Venue=="Virginia Tech - The Marching Virginians Center"]<-"Home"
all.pages$GameType[all.pages$Home=="Texas Christian"
                   & all.pages$Venue=="TCU Intramural Field"]<-"Home"
all.pages$GameType[all.pages$Home=="Southeastern Louisiana"
                   & all.pages$Venue=="Southeastern Soccer Complex (SELU)"]<-"Home"
all.pages$GameType[all.pages$Venue=="North Park Field"]<-"Neutral"
all.pages$GameType[all.pages$Home=="Southern Illinois"
                   & all.pages$Venue=="SIU Carbondale"]<-"Home"
all.pages$GameType[all.pages$Home=="Keiser University"
                   & all.pages$Venue=="Keiser University"]<-"Home"
all.pages$GameType[all.pages$Home=="SUNY Purchase"
                   & all.pages$Venue=="SUNY-Purchase"]<-"Home"
all.pages$GameType[all.pages$Home=="UC Colorado Springs"
                   & all.pages$Venue=="University of Colorado (Colorado Springs)"]<-"Home"
all.pages$GameType[all.pages$Home=="Southeastern Louisiana"
                   & all.pages$Venue=="Strawberry Stadium"]<-"Home"
all.pages$GameType[all.pages$Home=="Weber State"
                   & all.pages$Venue=="Spence Eccles Ogden Community Sports Complex"]<-"Home"
all.pages$GameType[all.pages$Venue=="Vanderbilt - Rec Field 2"]<-"Neutral"
all.pages$GameType[all.pages$Home=="Auburn"
                   & all.pages$Venue=="Duck Samford (Auburn)"]<-"Home"
all.pages$GameType[all.pages$Home=="West Virginia"
                   & all.pages$Venue=="West Virginia - University HS"]<-"Home"
all.pages$GameType[all.pages$Venue=="South Barrington Park District"]<-"Neutral"


mcla2018todate<-all.pages

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




