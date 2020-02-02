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

#### Cleaning and EDA ####
data_2019 %>%
   dplyr::select(pff_PLAYID, pff_GAMEID, pff_DOWN, pff_QUARTER, pff_SHOTGUN,
                 pff_PISTOL, pff_GAINLOSS, pff_OFFFORMATION, pff_OFFSUCCESS,
                 pff_DEEPPASS, pff_FIELDPOSITION, pff_DISTANCE,
                 pff_RUNPASSOPTION, pff_OPTION) %>%
   mutate(
      pff_SHOTGUN = case_when(
         pff_SHOTGUN == "S" ~ 1,
         TRUE ~ 0),
      pff_PISTOL = case_when(
         pff_PISTOL == "P" ~ 1,
         TRUE ~ 0),
      pff_OFFSUCCESS = case_when(
         pff_OFFSUCCESS == "1G" ~ 1,
         pff_OFFSUCCESS == "2A" ~ 0,
         pff_OFFSUCCESS == "3R" ~ 0)
      ) %>%
   filter(!is.na(pff_OFFSUCCESS)) -> subset_2019

# Histogram for yard gain/loss 
subset_2019 %>%
   ggplot() + 
   geom_histogram(aes(x=pff_GAINLOSS), binwidth = 5)

# Correlations of quantitative variables
data_2019 %>%
   select_if(is.numeric) -> subset_2019_quant

subset_2019_quant %>%
   filter(!is.na(pff_GAINLOSS)) %>%
   cor() %>%
   as.data.frame() -> cor_2019

#### Logistic Regression ####
set.seed(12211999)

# split data
sample.data <- sample.int(nrow(subset_2019), floor(.50*nrow(subset_2019)),
                          replace = F)
train <- subset_2019[sample.data, ]
test <- subset_2019[-sample.data, ] 


# build model
logistic_train <- glm(pff_OFFSUCCESS ~ pff_SHOTGUN + pff_PISTOL + pff_DEEPPASS +
                    pff_FIELDPOSITION + pff_DISTANCE + pff_RUNPASSOPTION +
                    pff_OPTION, data = subset_2019, family = "binomial")

summary(logistic_train)

# get predictions and create ROC curve
preds_logistic <- predict(logistic_train, newdata=test, type = "response")

rates_logistic <- prediction(preds_logistic, test$pff_OFFSUCCESS)

roc_logistic <- performance(rates_logistic, measure = "tpr", x.measure = "fpr")

plot(roc_logistic, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

# confusion matrix and overall error rate
confusion.mat.logistic <- table(test$pff_OFFSUCCESS, preds_logistic > 0.5)

overall.error.logistic <- (confusion.mat.logistic[1,2] + 
                              confusion.mat.logistic[2,1]) / 
   sum(confusion.mat.logistic) 
# Overall error rate with logistic regression is 32.98%

#### Linear Discriminant Analysis ####

# build model w/ same test and training data
lda_train <- lda(pff_OFFSUCCESS ~ pff_SHOTGUN + pff_PISTOL + pff_DEEPPASS +
                         pff_FIELDPOSITION + pff_DISTANCE + pff_RUNPASSOPTION +
                         pff_OPTION, data = subset_2019)


# get predictions and create ROC curve
preds_lda <- predict(lda_train, newdata=test)

rates_logistic <- predict(lda_train, test)

roc_logistic <- performance(rates, measure = "tpr", x.measure = "fpr")

plot(roc_logistic, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

# confusion matrix and overall error rate
confusion.mat.logistic <- table(test$pff_OFFSUCCESS, preds > 0.5)

overall.error.logistic <- (confusion.mat[1,2] + confusion.mat[2,1]) /
   sum(confusion.mat) # Overall error rate with logistic regression is 32.98%
