library(plyr)
scores <- read.csv("~/Dropbox/Lacrosse/MCLA scores.csv", stringsAsFactors = FALSE)

rpis <- calculateRPIs(scores)



cbind(rpis[order(rpis$V1, decreasing = TRUE) , ], 1:nrow(rpis))

calculateRPIs <- function(scores){
  all_teams <- unique(c(scores$Home.Team, scores$Away.Team))
  names(all_teams) <- all_teams
  RPIs <- ldply(all_teams, calculateIndividualRPI, scores)
  return(RPIs)
}

calculateIndividualRPI <- function(specific_team, scores){
  
  team_wp <- calculateWP(specific_team, scores)
  
  opponents <- findOpponents(specific_team, scores)
  OO <- setdiff(unique(unlist(llply(opponents, findOpponents, scores ))), specific_team)
  
  OWP <- mean(ldply(opponents, calculateWP, scores = scores)$V1)
  OOWP <- mean(ldply(OO, calculateWP, scores = scores)$V1)
  
  RPI <- 0.25 * team_wp + 0.5 * OWP + 0.25 * OOWP
  return(RPI)
}

calculateWP <- function(specific_team, scores){
  specific_team_games <- scores[specific_team == scores$Home.Team | 
                                  specific_team == scores$Away.Team, ]
  
  return(mean(specific_team_games$Winning.Team == specific_team))
}

findOpponents <- function(specific_team, scores){
  specific_team_games <- scores[specific_team == scores$Home.Team | 
                                  specific_team == scores$Away.Team, ]
  opponents <- setdiff(unique(c(specific_team_games$Home.Team, specific_team_games$Away.Team)), specific_team)
  return(opponents)
}
