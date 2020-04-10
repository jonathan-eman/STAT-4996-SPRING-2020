setwd("~/R/STAT 4996/STAT-4996-SPRING-2020/Final Scripts")
source('run_full_drive.R')
source('game_simulator.R')
source('field_goal_probability.R')

library(tidyverse)
library(fitdistrplus)
library(actuar)

#  states = (A, B, C, D, E, F)

# A is starting field position of play (ranges from 0 to 100 (so your own 25 would be 75. 0 would be a touchdown))
# B is time remaining in quarter
# C is yards to first down
# D is down number
# E is starting score differential
# F is aggressive play indication

set.seed(04082020)

### Whole game aggressive ----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(1:3, 1), 
                                        F=1))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(4:7, 1), 
                                        F=1))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(8:13, 1), 
                                        F=1))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(14:20, 1), 
                                        F=1))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.14to20

### Whole game non-aggressive----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(1:3, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> nonag.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(4:7, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> nonag.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(8:13, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> nonag.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(14:20, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> nonag.14to20

### Aggressive at 10 mins----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator_10(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(1:3, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.10.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator_10(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(4:7, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.10.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator_10(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(8:13, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.10.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator_10(A=floor(rnorm(1, 60, 10)), 
                                        B=15, 
                                        C=10, 
                                        D=1, 
                                        E=-sample(14:20, 1), 
                                        F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.10.14to20


### Aggressive at 5 mins----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator_5(A=floor(rnorm(1, 60, 10)), 
                                           B=15, 
                                           C=10, 
                                           D=1, 
                                           E=-sample(1:3, 1), 
                                           F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.5.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator_5(A=floor(rnorm(1, 60, 10)), 
                                           B=15, 
                                           C=10, 
                                           D=1, 
                                           E=-sample(4:7, 1), 
                                           F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.5.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator_5(A=floor(rnorm(1, 60, 10)), 
                                           B=15, 
                                           C=10, 
                                           D=1, 
                                           E=-sample(8:13, 1), 
                                           F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.5.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator_5(A=floor(rnorm(1, 60, 10)), 
                                           B=15, 
                                           C=10, 
                                           D=1, 
                                           E=-sample(14:20, 1), 
                                           F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.5.14to20


### Aggressive at 7.5 mins----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator_7.5(A=floor(rnorm(1, 60, 10)), 
                                          B=15, 
                                          C=10, 
                                          D=1, 
                                          E=-sample(1:3, 1), 
                                          F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.7.5.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator_7.5(A=floor(rnorm(1, 60, 10)), 
                                          B=15, 
                                          C=10, 
                                          D=1, 
                                          E=-sample(4:7, 1), 
                                          F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.7.5.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator_7.5(A=floor(rnorm(1, 60, 10)), 
                                          B=15, 
                                          C=10, 
                                          D=1, 
                                          E=-sample(8:13, 1), 
                                          F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.7.5.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator_7.5(A=floor(rnorm(1, 60, 10)), 
                                          B=15, 
                                          C=10, 
                                          D=1, 
                                          E=-sample(14:20, 1), 
                                          F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.7.5.14to20

### Aggressive at 2.5 mins----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator_2.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(1:3, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.2.5.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator_2.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(4:7, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.2.5.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator_2.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(8:13, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.2.5.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator_2.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(14:20, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.2.5.14to20



### Aggressive at 12.5 mins----
# 1-3
monte_carlo <- replicate(2000,
                         game_simulator_12.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(1:3, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.12.5.1to3

# 4-7
monte_carlo <- replicate(2000,
                         game_simulator_12.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(4:7, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.12.5.4to7

# 8-13
monte_carlo <- replicate(2000,
                         game_simulator_12.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(8:13, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.12.5.8to13

# 14-20
monte_carlo <- replicate(2000,
                         game_simulator_12.5(A=floor(rnorm(1, 60, 10)), 
                                            B=15, 
                                            C=10, 
                                            D=1, 
                                            E=-sample(14:20, 1), 
                                            F=0))

as.data.frame(t(monte_carlo)) %>%
   mutate(win = Team_0 >= Team_1) %>%
   summarize(win_percentage = mean(win)) -> ag.12.5.14to20


### Results matrix ----
matrix(c(nonag.1to3, nonag.4to7, nonag.8to13, nonag.14to20,
         ag.2.5.1to3, ag.2.5.4to7, ag.2.5.8to13, ag.2.5.14to20,
         ag.5.1to3, ag.5.4to7, ag.5.8to13, ag.5.14to20,
         ag.7.5.1to3, ag.7.5.4to7, ag.7.5.8to13, ag.7.5.14to20,
         ag.10.1to3, ag.10.4to7, ag.10.8to13, ag.10.14to20,
         ag.12.5.1to3, ag.12.5.4to7, ag.12.5.8to13, ag.12.5.14to20,
         ag.1to3, ag.4to7, ag.8to13, ag.14to20),
       byrow = TRUE, nrow = 7) -> results

colnames(results) <- c("1-3", "4-7", "8-13", "14+")
rownames(results) <- c("Non-Aggressive", "Aggressive @ 2.5",
                       "Aggressive @ 5", "Aggressive @ 7.5",
                       "Aggressive @ 10", "Aggressive @ 12.5",
                       "Full Quarter Aggressive")

results
 


# ### test ----
# monte_carlo_15 <- replicate(1000,
#                          game_simulator(A=floor(rnorm(1, 60, 10)), 
#                                         B=15, 
#                                         C=10, 
#                                         D=1, 
#                                         E=-sample(1:3, 1), 
#                                         F=1))
# 
# as.data.frame(t(monte_carlo_15)) %>%
#    mutate(win = Team_0 >= Team_1) %>%
#    summarize(win_percentage = mean(win)) -> results_15
# 
# monte_carlo_10 <- replicate(1000,
#                          game_simulator_10(A=floor(rnorm(1, 60, 10)), 
#                                         B=15, 
#                                         C=10, 
#                                         D=1, 
#                                         E=-sample(1:3, 1), 
#                                         F=0))
# 
# as.data.frame(t(monte_carlo_10)) %>%
#    mutate(win = Team_0 >= Team_1) %>%
#    summarize(win_percentage = mean(win)) -> results_10
# 
# monte_carlo_5 <- replicate(1000,
#                             game_simulator_5(A=floor(rnorm(1, 60, 10)), 
#                                               B=15, 
#                                               C=10, 
#                                               D=1, 
#                                               E=-sample(1:3, 1), 
#                                               F=0))
# 
# as.data.frame(t(monte_carlo_5)) %>%
#    mutate(win = Team_0 >= Team_1) %>%
#    summarize(win_percentage = mean(win)) -> results_5
# 
# monte_carlo_0 <- replicate(1000,
#                             game_simulator(A=floor(rnorm(1, 60, 10)), 
#                                            B=15, 
#                                            C=10, 
#                                            D=1, 
#                                            E=-sample(1:3, 1), 
#                                            F=0))
# 
# as.data.frame(t(monte_carlo_0)) %>%
#    mutate(win = Team_0 >= Team_1) %>%
#    summarize(win_percentage = mean(win)) -> results_0
# 
# c(results_15, results_10, results_5, results_0)
