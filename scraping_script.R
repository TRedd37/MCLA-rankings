library(rvest)
library(XML)

# Games and Results

all.pages<-NULL
for(i in 1:26){
  # create the webpage url using paste
  thispage.url<-paste("http://mcla.us/schedule/2017?page=",i,sep="")
  # read webpage and store in memory
  thispage.webpage<-htmlParse(thispage.url)
  # create R dataset from webpage contents
  thispage<-readHTMLTable(thispage.webpage,
                          header=TRUE,which=1,stringsAsFactors=FALSE)
  # combine this page's data to the all.pages dataframe
  all.pages<-rbind(all.pages,thispage)
}

# parse score
all.pages$AwayGoals<-temp.scores[seq(1,2*length(all.pages$Score),by=2)]
all.pages$HomeGoals<-temp.scores[seq(2,2*length(all.pages$Score),by=2)]

page <- read_html(thispage.url)
game_urls <- page %>% html_nodes("td:nth-child(2) a") %>% html_attr("href")
game_urls[1] %>% readHTMLTable(header=TRUE, which=1, stringsAsFactors=FALSE)
