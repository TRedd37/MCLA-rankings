library(ReddRankings)
library(elo)


results <- plyr::llply(2015:2018, getResults)

games <- results[[4]]
games <- getResults(2019)

HFA <- 0:20
base_k <- 1:150
multiplier <- seq(0,1, by= .1)
all_model_params <- expand.grid( multiplier = multiplier, HFA = HFA, base_k = base_k)

divisions <- getDivisions()

all_teams <- union(games$Home.Team, games$Away.Team)

initial_elos <- rep(1450, length(all_teams))
names(initial_elos) <- all_teams
initial_elos[names(initial_elos) %in% divisions$D1] <- 1500
initial_elos[names(initial_elos) %in% divisions$D2] <- 1100

games_d1 <- games %>%
  filter(Home.Team %in% union(divisions$D1, "Utah") & Away.Team %in% union(divisions$D1, "Utah"))

auc = 0
mse = 0
pb <- txtProgressBar(min = 1, max = nrow(all_model_params), style = 3)
for(i in 1:nrow(all_model_params)){
  setTxtProgressBar(pb, i)
  model = elo.run(score(HomeGoals, AwayGoals) ~ 
                    adjust(Home.Team, (all_model_params$HFA[i] * (1-Neutral))) + Away.Team + 
                    k(all_model_params$base_k[i] * 
                        abs(HomeGoals - AwayGoals)^all_model_params$multiplier[i]), 
                  data = games)
  auc[i] <- pROC::auc(model)
  mse[i] <- summary(model)$mse
}

model = elo.run(score(HomeGoals, AwayGoals) ~ 
                  adjust(Home.Team, (15 * (1-Neutral))) + Away.Team + 
                  k(15 * 
                      abs(HomeGoals - AwayGoals)^.3), 
                data = games,
                initial.elos = initial_elos)

new_games <- data.frame(Home.Team = c("Utah Valley", "Brigham Young", "UC Santa Barbara"), 
           Away.Team = c("Brigham Young", "Utah Valley", "Michigan State"),
           Neutral = FALSE)

predict(model, newdata = new_games)

all_model_params[which.max(auc), ]
auc[3153]

summary(model)
elo_ratings <- sort(final.elos(model), decreasing = TRUE)

data.frame(School = names(elo_ratings), 
           ELO = elo_ratings, 
           Ranking = 1:length(elo_ratings))





