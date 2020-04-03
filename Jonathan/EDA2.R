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
library(fitdistrplus)
library(emg)

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
      START_YDS_TO_END_ZONE = ifelse(pff_FIELDPOSITION < 0,
                              pff_FIELDPOSITION + 100,
                              pff_FIELDPOSITION),
      END_YDS_TO_END_ZONE = ifelse(pff_PLAYENDFIELDPOSITION < 0,
                                        pff_PLAYENDFIELDPOSITION + 100,
                                        pff_PLAYENDFIELDPOSITION),
      YDS_GAINED = START_YDS_TO_END_ZONE - END_YDS_TO_END_ZONE
      #AGGRESSIVE_PLAY = case_when(
         
      #)
      ) %>%
  #filter(pff_SCOREDIFFERENTIAL < 0,
   #       pff_QUARTER == 4,
    #      pff_DOWN != 0) %>%
   dplyr::select(pff_GAMEID, pff_QUARTER, TIME_REMAINING,
                 pff_SCOREDIFFERENTIAL, pff_DISTANCE, pff_FIELDPOSITION,
                 OFF_TEAM, DEF_TEAM, HOME_SCORE, AWAY_SCORE, pff_OFFSCORE,
                 pff_DEFSCORE, START_YDS_TO_END_ZONE, END_YDS_TO_END_ZONE,
                 YDS_GAINED, pff_DRIVEENDEVENT, pff_RUNPASS,
                 pff_TRICKPLAY, pff_DEEPPASS, pff_DRIVEPLAY,
                 pff_DRIVEENDPLAYNUMBER) -> subset_2019

### All passes
subset_2019 %>%
   filter(pff_RUNPASS == "P", pff_DEEPPASS == 0) %>%
   dplyr::select(YDS_GAINED) -> passes

passes %>%
   filter(YDS_GAINED >= 0) -> positive_passes

passes %>%
   filter(YDS_GAINED < 0) %>%
   mutate(YDS_LOST = abs(YDS_GAINED)) -> negative_passes

ggplot(positive_passes) +
   geom_histogram(aes(x=YDS_GAINED, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_GAINED))

ggplot(negative_passes) +
   geom_histogram(aes(x=YDS_LOST, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_LOST))

### All runs 
subset_2019 %>%
   filter(pff_RUNPASS == "R") %>%
   dplyr::select(YDS_GAINED) -> runs

runs %>%
   filter(YDS_GAINED >= 0) -> positive_runs

runs %>%
   filter(YDS_GAINED < 0) %>%
   mutate(YDS_LOST = abs(YDS_GAINED)) -> negative_runs

ggplot(positive_runs) +
   geom_histogram(aes(x=YDS_GAINED, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_GAINED))

ggplot(negative_runs) +
   geom_histogram(aes(x=YDS_LOST, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_LOST))

### Deep passes 
subset_2019 %>%
   filter(pff_DEEPPASS == 1) %>%
   dplyr::select(YDS_GAINED) -> deep_passes

deep_passes %>%
   filter(YDS_GAINED >= 0) -> positive_deep_passes

deep_passes %>%
   filter(YDS_GAINED < 0) %>%
   mutate(YDS_LOST = abs(YDS_GAINED)) -> negative_deep_passes

ggplot(positive_deep_passes) +
   geom_histogram(aes(x=YDS_GAINED, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_GAINED))

ggplot(negative_deep_passes) +
   geom_histogram(aes(x=YDS_LOST, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_LOST))

### Trick plays runs
subset_2019 %>%
   filter(pff_TRICKPLAY == 1, pff_RUNPASS == "R") %>%
   dplyr::select(YDS_GAINED) -> trick_plays

trick_plays %>%
   filter(YDS_GAINED >= 0) -> positive_trick_plays
 
trick_plays %>%
   filter(YDS_GAINED < 0) %>%
   mutate(YDS_LOST = abs(YDS_GAINED)) -> negative_trick_plays

ggplot(positive_trick_plays) +
   geom_histogram(aes(x=YDS_GAINED, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_GAINED))

ggplot(negative_trick_plays) +
   geom_histogram(aes(x=YDS_LOST, y=..density..), binwidth = 3) +
   geom_density(aes(x=YDS_LOST))

### 

subset_2019 %>%
   group_by(pff_GAMEID) %>%
   slice(n()) %>%
   mutate(WINNER = ifelse(HOME_SCORE > AWAY_SCORE, "Home", "Away")) %>%
   dplyr::select(pff_GAMEID, HOME_SCORE, AWAY_SCORE, WINNER) -> winners_2019

subset_2019 %>%
   dplyr::select(pff_GAMEID, pff_QUARTER, TIME_REMAINING,
                 pff_SCOREDIFFERENTIAL, 
          pff_DISTANCE, pff_FIELDPOSITION, OFF_TEAM, DEF_TEAM, pff_OFFSCORE,
          pff_DEFSCORE) %>% head()

subset_2019 %>%
   left_join(winners_2019, by = "pff_GAMEID") %>% 
   mutate(TEAM_WINS = as.factor(ifelse(
      OFF_TEAM == WINNER, 1, 0))) -> join_2019

subset_2019 %>%
   filter(pff_RUNPASS %in% c("R", "P")) %>%
   group_by(pff_RUNPASS) %>%
   summarize(prop = n()/nrow(subset_2019 %>%
                                filter(pff_RUNPASS %in% c("R", "P"))))

# for normal play, prob of pass is .528, run is .472
subset_2019 %>%
   filter(pff_RUNPASS %in% c("R", "P")) %>%
   group_by(pff_RUNPASS, pff_TRICKPLAY) %>%
   summarize(prop = n()/nrow(subset_2019 %>% 
                                filter(pff_RUNPASS %in% c("R", "P"))))

# Field goals ----
subset_2019 %>% 
   filter(pff_DRIVEENDEVENT %in% c("FIELD GOAL", "MISSED FG"),
          pff_DRIVEENDPLAYNUMBER == pff_DRIVEPLAY) %>%
   dplyr::select(START_YDS_TO_END_ZONE, pff_DRIVEENDEVENT) %>%
   mutate(FIELD_GOAL_MADE = as.numeric(pff_DRIVEENDEVENT == "FIELD GOAL")) ->
   field_goals

glm(FIELD_GOAL_MADE ~ START_YDS_TO_END_ZONE, data = field_goals,
    family = "binomial") -> field_goal_probability_model

summary(field_goal_probability_model)

field_goal_probability <- function(x) {
   exp(2.410322 - 0.064872*x) / (1 + exp(2.410322 - 0.064872*x))
}
