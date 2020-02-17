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
      TIME_REMAINING = round(as.numeric(MINS) + as.numeric(SECS)/60, 2)
   ) %>%
   filter(pff_SCOREDIFFERENTIAL < 0,
          pff_QUARTER == 4,
          TIME_REMAINING <= 5) -> subset_2019

subset_2019 %>%
   group_by(pff_GAMEID) %>%
   slice(n()) %>%
   mutate(WINNER = ifelse(HOME_SCORE > AWAY_SCORE, "Home", "Away")) %>%
   select(pff_GAMEID, HOME_SCORE, AWAY_SCORE, WINNER) -> winners_2019

subset_2019 %>%
   select(pff_GAMEID, pff_QUARTER, TIME_REMAINING, pff_SCOREDIFFERENTIAL, 
          pff_DISTANCE, pff_FIELDPOSITION, OFF_TEAM, DEF_TEAM, pff_OFFSCORE,
          pff_DEFSCORE) %>% head()

### Win probability model
subset_2019 %>%
   left_join(winners_2019, by = "pff_GAMEID") %>% 
   mutate(TEAM_WINS = ifelse(OFF_TEAM == WINNER, 1, 0))



