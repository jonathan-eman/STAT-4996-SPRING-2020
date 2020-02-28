# indicate if home = away, etc.
# filter home != away
# separate home.away -> map off_score and def_score
# create variable for winner

# win prob model - time left, field position, score differential
# see how win probability changes with aggressive play vs. non-aggressive play (coarse events: short yard, long yard, field goal, punt)
# calculate from last 5 minutes of game
# run play on 4th down aggressive in "aggressive scenario" - short yardage

setwd("~/R/STAT 4996")

library(tidyverse)
library(ggcorrplot)
library(ROCR)
library(MASS)
library(klaR)
library(ICS)
library(ISLR)

# Read-in data
data_2019 <- read.csv("2019 PFF All Plays.csv")

# Filter for only plays where team was tied or losing
data_2019 %>% 
   separate(pff_SCORE, c("HOME_SCORE", "AWAY_SCORE")) %>%
   separate(pff_CLOCK, c("MINS", "SECS")) %>%
   mutate(
      HOME_SCORE = as.numeric(HOME_SCORE),
      AWAY_SCORE = as.numeric(AWAY_SCORE),
      pff_OFFSUCCESS = case_when(
         pff_OFFSUCCESS == "1G" ~ 1,
         pff_OFFSUCCESS == "2A" ~ 0,
         pff_OFFSUCCESS == "3R" ~ 0),
      HOME_SCORE = case_when(
         is.na(HOME_SCORE) ~ 0,
         TRUE ~ HOME_SCORE
      ),
      AWAY_SCORE = case_when(
         is.na(AWAY_SCORE) ~ 0,
         abs(HOME_SCORE - AWAY_SCORE) != abs(pff_SCOREDIFFERENTIAL) ~ 
            as.numeric(paste0(AWAY_SCORE, 0)),
         TRUE ~ AWAY_SCORE
      ),
      OFF_TEAM = ifelse(HOME_SCORE == pff_OFFSCORE, "Home", "Away"),
      DEF_TEAM = ifelse(HOME_SCORE == pff_DEFSCORE, "Home", "Away"),
      TIME_REMAINING = round(as.numeric(MINS) + as.numeric(SECS)/60, 2),
      pff_DOWN = as.factor(pff_DOWN),
      pff_QUARTER = as.factor(pff_QUARTER),
      YDS_TO_END_ZONE = ifelse(pff_FIELDPOSITION < 0,
                              abs(pff_FIELDPOSITION) + 50,
                              pff_FIELDPOSITION)
   ) %>%
   filter(pff_SCOREDIFFERENTIAL < 0,
          pff_QUARTER = 4,
          pff_DOWN != 0,
          TIME_REMAINING <= 10) -> subset_2019

subset_2019 %>%
   group_by(pff_GAMEID) %>%
   slice(n()) %>%
   mutate(WINNER = ifelse(HOME_SCORE > AWAY_SCORE, "Home", "Away")) %>%
   dplyr::select(pff_GAMEID, HOME_SCORE, AWAY_SCORE, WINNER) -> winners_2019

subset_2019 %>%
   select(pff_GAMEID, pff_QUARTER, TIME_REMAINING, pff_SCOREDIFFERENTIAL, 
          pff_DISTANCE, pff_FIELDPOSITION, OFF_TEAM, DEF_TEAM, pff_OFFSCORE,
          pff_DEFSCORE) %>% head()

subset_2019 %>%
   left_join(winners_2019, by = "pff_GAMEID") %>% 
   mutate(TEAM_WINS = as.factor(ifelse(
      OFF_TEAM == WINNER, 1, 0))) -> join_2019

set.seed(12211999)

# split data
sample.data <- sample.int(nrow(join_2019), floor(.50*nrow(join_2019)),
                          replace = F)
train <- join_2019[sample.data, ]
test <- join_2019[-sample.data, ] 


### Win probability model

glm(data = train, formula = TEAM_WINS ~ pff_QUARTER + pff_DOWN +
    TIME_REMAINING + pff_DISTANCE + YDS_TO_END_ZONE + pff_SCOREDIFFERENTIAL,
    family = "binomial") -> win_prob_model

summary(win_prob_model)

# get predictions and create ROC curve
preds_logistic <- predict(win_prob_model, newdata=test, type = "response")

rates_logistic <- prediction(preds_logistic, test$TEAM_WINS)

roc_logistic <- performance(rates_logistic, measure = "tpr", x.measure = "fpr")

plot(roc_logistic, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

# confusion matrix and overall error rate
confusion.mat.logistic <- table(test$TEAM_WINS, preds_logistic > 0.5)

overall.error.logistic <- (confusion.mat.logistic[1,2] + 
                              confusion.mat.logistic[2,1]) / 
   sum(confusion.mat.logistic) 

# Overall error rate with logistic regression is 13.7%



