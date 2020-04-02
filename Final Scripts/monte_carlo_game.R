setwd("~/R/STAT 4996/STAT-4996-SPRING-2020/Final Scripts")
source('run_full_drive.R')
source('game_simulator.R')

library(tidyverse)
library(emg)

# We need to simulate a large number of games and calculate the win
# probabilities for the following situations:

# Losing by >14, between 7 and 14, between 3 and 7, <3
# Starting aggressive with 10 mins, 5 mins, 2 mins, and not at all

#  states = (A, B, C, D, E, F)

#  Probability breakdown:
#  P(A_{t+1}, B_{t+1}, C_{t+1} | A_t, B_t, C_t) 
#          = P(A_{t+1} | C_{t+1}, C_t, A_t) 
#              P(B_{t+1} | C_{t+1}, C_t, B_t) 
#              P(C_{t+1} | A_t, B_t, C_t)

# A is starting field position of play (ranges from 0 to 100 (so your own 25 would be 75. 0 would be a touchdown))
# B is time remaining in quarter
# C is yards to first down
# D is down number
# E is starting score differential
# F is aggressive play indication

# Losing by 3, non-aggressive
monte_carlo <- replicate(100,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-3, 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 > Team_1) %>%
   summarize(win_percentage = mean(win))
