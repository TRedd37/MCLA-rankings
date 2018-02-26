
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




