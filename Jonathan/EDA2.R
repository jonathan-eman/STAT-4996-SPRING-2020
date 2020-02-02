setwd("~/R/STAT 4996")

library(tidyverse)

# Read-in data
data_2019 <- read.csv("2019 PFF All Plays.csv")

# Filter for only plays where team was tied or losing
data_2019 %>%
   filter(pff_SCOREDIFFERENTIAL <= 0) %>%
   mutate(
      pff_OFFSUCCESS = case_when(
         pff_OFFSUCCESS == "1G" ~ 1,
         pff_OFFSUCCESS == "2A" ~ 0,
         pff_OFFSUCCESS == "3R" ~ 0),
      TIMEREMAINING = case_when(
         pff_QUARTER == 1 
      )
   ) -> subset_2019

subset_2019 %>%
   ggplot(aes(x = pff_SCOREDIFFERENTIAL)) +
   geom_histogram(binwidth = 5)
   
subset_2019 %>%
   filter(!is.na(pff_OFFSUCCESS)) %>%
   group_by(pff_QUARTER, pff_DOWN) %>%
   summarize(prob_success = mean(pff_OFFSUCCESS))

