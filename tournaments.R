library(plyr)
calculateProbabilityToSurvive <- function(match_ups){
  if(length(unlist(match_ups)) == 2){
    home_win_prop <- predictGameOutcome(names(match_ups)[1], names(match_ups)[2], 
                                        model_output, neutral = TRUE)
    match_ups[[1]] = match_ups[[1]] * home_win_prop
    match_ups[[2]] = match_ups[[2]] * (1 - home_win_prop)
  } else {
    match_ups <- llply(match_ups, calculateProbabilityToSurvive)
    output <- match_ups
    prob1over2 <-  predictGameOutcome(names(match_ups[[1]])[1], 
                                      names(match_ups[[2]])[1], 
                                      model_output, neutral = TRUE)
    prob1over3 <- predictGameOutcome(names(match_ups[[1]])[1], 
                                     names(match_ups[[2]])[2], 
                                     model_output, neutral = TRUE)
    prob4over2 <- predictGameOutcome(names(match_ups[[1]])[2], 
                                     names(match_ups[[2]])[1], 
                                     model_output, neutral = TRUE)
    prob4over3 <- predictGameOutcome(names(match_ups[[1]])[2], 
                                     names(match_ups[[2]])[2], 
                                     model_output, neutral = TRUE)
    output[[1]][1] <- match_ups[[1]][1] * (
        match_ups[[2]][1] * prob1over2 + 
        match_ups[[2]][2] * prob1over3)
    output[[1]][2] <- match_ups[[1]][2] * (
        match_ups[[2]][1] * prob4over2 + 
        match_ups[[2]][2] * prob4over3)
    output[[2]][1] <- match_ups[[2]][1] * (
      match_ups[[1]][1] * (1-prob1over2) + 
        match_ups[[1]][2] * (1-prob4over2))
    output[[2]][2] <- match_ups[[2]][2] * (
      match_ups[[1]][2] * (1-prob4over3) + 
        match_ups[[1]][1] * (1-prob1over3))
    match_ups <- output
  }
  return(match_ups)
}

calculateCombination <- function(side1, side2){
  tournament <- c(side1, side2)
  for(team in names(side1)){
    lse <- 0
    for(team2 in names(side2)){
      lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * side2[team2])
    }
    tournament[team] <- side1[team] * lse
  }
  for(team in names(side2)){
    lse <- 0
    for(team2 in names(side1)){
      lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * side1[team2])
    }
    tournament[team] <- side2[team] * lse
  }
  return(tournament)
  
}
# 
# 
# tournament <- list(c("South Carolina" = 1, "Florida" = 1), c("Georgia Tech" = 1, "Liberty" = 1))
# selc_east <- unlist(calculateProbabilityToSurvive(tournament))
# 
# tournament <- list(c("Virginia Tech" = 1, "Georgia" = 1), c("Florida State" = 1, "Clemson" = 1))
# selc_west <- unlist(calculateProbabilityToSurvive(tournament))
# 
# selc_tournament <- c(selc_east, selc_west)
# 
# for(team in names(selc_east)){
#   lse <- 0
#   for(team2 in names(selc_west)){
#     lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * selc_west[team2])
#   }
#   selc_tournament[team] <- selc_east[team] * lse
# }
# for(team in names(selc_west)){
#   lse <- 0
#   for(team2 in names(selc_east)){
#     lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * selc_east[team2])
#   }
#   selc_tournament[team] <- selc_west[team] * lse
# }
# 
# tournament <- list(c("South Carolina" = 1, "Liberty" = 1), c("Virginia Tech" = 1, "Clemson" = 1))
# selc_west <- unlist(calculateProbabilityToSurvive(tournament))
# 
# predictGameOutcome("South Carolina", "Virginia Tech", model_output, neutral = TRUE)
# 
# ############# LSA ################
# 
# tournament <- list(c("Southern Methodist" = 1, "Texas A&M" = 1), c("Oklahoma" = 1, "Texas" = 1))
# unlist(calculateProbabilityToSurvive(tournament))
# 
# predictGameOutcome("Oklahoma", "Texas A&M", model_output, neutral = TRUE)
# 
# ############ PCLL ###############
# 
# pcll_tournament <- list(c("Northeastern" = 1, "New Hampshire" = 1), c("Boston College" = 1, "Buffalo" = 1))
# unlist(calculateProbabilityToSurvive(pcll_tournament))
# 
# predictGameOutcome("Northeastern", "New Hampshire", model_output, neutral = TRUE)
# predictGameOutcome("Boston College", "Buffalo", model_output, neutral = TRUE)
# 
# predictGameOutcome("Boston College", "Northeastern", model_output, neutral = TRUE)
# 
# ############ PNCLL ###############
# 
# pncll_tournament <- list(c("Simon Fraser" = 1, "Oregon State" = 1), c("Oregon" = 1, "Washington" = 1))
# unlist(calculateProbabilityToSurvive(pncll_tournament))
# 
# predictGameOutcome("Simon Fraser", "Oregon State", model_output, neutral = TRUE)
# predictGameOutcome("Washington", "Oregon", model_output, neutral = TRUE)
# 
# predictGameOutcome("Simon Fraser", "Oregon", model_output, neutral = TRUE)
# 
# 
# ####################### SLC ###############
# 
# CIU_win1 <- predictGameOutcome("Concordia-Irvine", 
#                                "UC Santa Barbara", 
#                                model_output, neutral = TRUE)
# UCSB_win1 = 1 - CIU_win1
# 
# gcu_CIU <- predictGameOutcome("Grand Canyon", "Concordia-Irvine", 
#                                model_output, neutral = TRUE)
# gcu_ucsb <- predictGameOutcome("Grand Canyon", "UC Santa Barbara", 
#                                 model_output, neutral = TRUE)
# 
# north_slc <- c( "Grand Canyon" = (CIU_win1 *  gcu_CIU+
#                              UCSB_win1 *  gcu_ucsb),
#                 "Concordia-Irvine" = CIU_win1 * (1-gcu_CIU),
#                 "UC Santa Barbara" = UCSB_win1 *  (1-gcu_ucsb)
# )
# 
# 
# ASU_win1 <- predictGameOutcome("Arizona State", 
#                                "San Diego State", 
#                                model_output, neutral = TRUE)
# SDSU_win1 = 1 - ASU_win1
# 
# chap_ASU <- predictGameOutcome("Chapman", "Arizona State", 
#                                model_output, neutral = TRUE)
# chap_SDSU <- predictGameOutcome("Chapman", "San Diego State", 
#                                 model_output, neutral = TRUE)
# 
# south_slc <- c( Chapman = (ASU_win1 *  chap_ASU+
#                              SDSU_win1 *  chap_SDSU),
#                 "Arizona State" = ASU_win1 * (1-chap_ASU),
#                 "San Diego State" = SDSU_win1 *  (1-chap_SDSU)
# )
# 
# slc_tournament <- c(north_slc, south_slc)
# 
# for(team in names(north_slc)){
#   lse <- 0
#   for(team2 in names(south_slc)){
#     lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * south_slc[team2])
#   }
#   slc_tournament[team] <- north_slc[team] * lse
# }
# for(team in names(south_slc)){
#   lse <- 0
#   for(team2 in names(north_slc)){
#     lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * north_slc[team2])
#   }
#   slc_tournament[team] <- south_slc[team] * lse
# }
# 
# slc_tournament <- list(c("Grand Canyon" = 1, "Concordia-Irvine" = 1), c("Chapman" = 1, "Arizona State" = 1))
# unlist(calculateProbabilityToSurvive(slc_tournament))
# 
# predictGameOutcome("Chapman", "Concordia-Irvine", 
#                    model_output, neutral = TRUE)
# 
# 
# ############### RMLC ##############
# 
# rmlc_tournament <- list(c("Utah" = 1, "Colorado State" = 1), c("Colorado" = 1, "Brigham Young" = 1))
# calculateProbabilityToSurvive(rmlc_tournament)
# 
# 
# predictGameOutcome("Utah", "Colorado", 
#                    model_output, neutral = TRUE)
# 
# ############# CCLA ################
# 
# predictGameOutcome("Minnesota", "Michigan State", model_output, neutral = TRUE)
# 
# ############ UMLC #################
# 
# umlc_west <- list(c("Illinois State" = 1, "Iowa State" = 1), c("Purdue" = 1, "MSU-Mankato" = 1))
# west_umlc <- unlist(calculateProbabilityToSurvive(umlc_west))
# 
# Mizz_win1 <- predictGameOutcome("Missouri", 
#                                "Oakland", 
#                                model_output, neutral = TRUE)
# Oak_win1 = 1 - Mizz_win1
# 
# Ind_Mizz <- predictGameOutcome("Indiana", "Missouri", 
#                                model_output, neutral = TRUE)
# Ind_Oak <- predictGameOutcome("Indiana", "Oakland", 
#                                 model_output, neutral = TRUE)
# 
# east_umlc <- c( Indiana = (Mizz_win1 *  Ind_Mizz +
#                          Oak_win1 *  Ind_Oak),
#                 "Missouri" = Mizz_win1 * (1-Ind_Mizz),
#                 "Oakland" = Oak_win1 *  (1-Ind_Oak)
# )
# 
# umlc_tournament <- c(west_umlc, east_umlc)
# 
# for(team in names(west_umlc)){
#   lse <- 0
#   for(team2 in names(east_umlc)){
#     lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * east_umlc[team2])
#   }
#   umlc_tournament[team] <- west_umlc[team] * lse
# }
# for(team in names(east_umlc)){
#   lse <- 0
#   for(team2 in names(west_umlc)){
#     lse = lse + (predictGameOutcome(team, team2, model_output, neutral = TRUE) * west_umlc[team2])
#   }
#   umlc_tournament[team] <- east_umlc[team] * lse
# }
# 
# umlc_tournament <- list(c("Illinois State" = 1, "Purdue" = 1), c("Oakland" = 1, "Indiana" = 1))
# umlc <- unlist(calculateProbabilityToSurvive(umlc_tournament))
# 
# predictGameOutcome("Illinois State", "Purdue", model_output, neutral = TRUE)
# predictGameOutcome("Indiana", "Oakland", model_output, neutral = TRUE)
# 
# 
# 
# ############# WCLL ################
# 
# wcll_tournament <- list(c("California" = 1, "Sonoma State" = 1), c("Santa Clara" = 1, "Cal Poly" = 1))
# calculateProbabilityToSurvive(wcll_tournament)
# 
# predictGameOutcome("California", "Cal Poly", model_output, neutral = TRUE)


######### D1 Nationals ##############

northwest_set <- list(c("Chapman" = 1, "Indiana" = 0), c("Concordia-Irvine" = 0, "Liberty" = 1))
northwest <- unlist(calculateProbabilityToSurvive(northwest_set))
northeast_set <- list(c("Utah" = 1, "Texas A&M" = 0), c("Grand Canyon" = 0, "Brigham Young" = 1))
northeast <- unlist(calculateProbabilityToSurvive(northeast_set))
southwest_set <- list(c("California" = 1, "Colorado" = 0), c("Georgia Tech" = 0, "South Carolina" = 1))
southwest <- unlist(calculateProbabilityToSurvive(southwest_set))
southeast_set <- list(c("Oregon" = 0, "Michigan State" = 1), c("Boston College" = 0, "Virginia Tech" = 1))
southeast <- unlist(calculateProbabilityToSurvive(southeast_set))

west_set <- list(c("Chapman" = 1, "Liberty" = 1), c("South Carolina" = 1, "California" = 1))
west_semis <- unlist(llply(west_set, calculateProbabilityToSurvive))
west <- unlist(calculateProbabilityToSurvive(west_set))

east_set <- list(c("Utah" = 1, "Brigham Young" = 1), c("Michigan State" = 1, "Virginia Tech" = 1))
east <- unlist(calculateProbabilityToSurvive(east_set))
east_semis <- unlist(llply(east_set, calculateProbabilityToSurvive))

d1_semi_finals <- c(west_semis, east_semis)

d1_finals <- c(west, east)

champs <- calculateCombination(west, east)

display_order <- order(d1_semi_finals, decreasing = TRUE)

semi_final_set <- list(c("Michigan State" = 1, "Brigham Young" = 1), c("Chapman" = 1, "South Carolina" = 1))
finals <- unlist(llply(semi_final_set, calculateProbabilityToSurvive))
champs <- unlist(calculateProbabilityToSurvive(semi_final_set))

display_order <- order(finals, decreasing = TRUE)

d1_output <- data.frame(Team = names(finals)[display_order],
           "Prob to make Finals" = scales::percent(finals[names(finals)[display_order]]),
           "Prob to win Championship" = scales::percent(champs[names(finals)[display_order]]),
           check.names = FALSE
           )



sort(champs, decreasing = TRUE)
######### D2 Nationals ##############

d2_northwest_set <- list(c("St. Thomas" = 1, "Missouri State" = 0), c("Sierra Nevada" = 0, "Florida Gulf Coast" = 1))
d2_northwest <- unlist(calculateProbabilityToSurvive(d2_northwest_set))
d2_northeast_set <- list(c("North Dakota State" = 1, "Central Conn. State" = 0), c("Dayton" = 0, "Montana State" = 1))
d2_northeast <- unlist(calculateProbabilityToSurvive(d2_northeast_set))
d2_southwest_set <- list(c("UC Davis" = 0, "St. John's" = 1), c("Cal State San Marcos" = 0, "Grand Valley State" = 1))
d2_southwest <- unlist(calculateProbabilityToSurvive(d2_southwest_set))
d2_southeast_set <- list(c("College of Idaho" = 0, "Minn.-Duluth" = 1), c("Cal State Fullerton" = 0, "Kennesaw State" = 1))
d2_southeast <- unlist(calculateProbabilityToSurvive(d2_southeast_set))


d2_west_set <- list(c("St. Thomas" = 1, "Florida Gulf Coast" = 1), c("St. John's" = 1, "Grand Valley State" = 1))
d2_west_semis <- unlist(llply(d2_west_set, calculateProbabilityToSurvive))
d2_west <- unlist(calculateProbabilityToSurvive(d2_west_set))

d2_east_set <- list(c("North Dakota State" = 1, "Montana State" = 1), c("Minn.-Duluth" = 1, "Kennesaw State" = 1))
d2_east <- unlist(calculateProbabilityToSurvive(d2_east_set))
d2_east_semis <- unlist(llply(d2_east_set, calculateProbabilityToSurvive))

d2_semi_finals <- c(d2_west_semis, d2_east_semis)

d2_finals <- c(d2_west, d2_east)

d2_champs <- calculateCombination(d2_west, d2_east)

display_order_d2 <- order(d2_semi_finals, decreasing = TRUE)

d2_output <- data.frame(Team = names(d2_semi_finals)[display_order_d2],
                        "Prob to make Semis" = scales::percent(d2_semi_finals[display_order_d2]),
                        "Prob to make Finals" = scales::percent(d2_finals[names(d2_semi_finals)[display_order_d2]]),
                        "Prob to win Championship" = scales::percent(d2_champs[names(d2_semi_finals)[display_order_d2]]),
                        check.names = FALSE
)

d2_semi_final_set <- list(c("St. Thomas" = 1, "Grand Valley State" = 1), c("North Dakota State" = 1, "Minn.-Duluth" = 1))
d2_finals <- unlist(llply(d2_semi_final_set, calculateProbabilityToSurvive))
d2_champs <- unlist(calculateProbabilityToSurvive(d2_semi_final_set))

d2_display_order <- order(d2_finals, decreasing = TRUE)

d2_output <- data.frame(Team = names(d2_finals)[display_order],
                        "Prob to make Finals" = scales::percent(d2_finals[names(d2_finals)[d2_display_order]]),
                        "Prob to win Championship" = scales::percent(d2_champs[names(d2_finals)[d2_display_order]]),
                        check.names = FALSE
)

