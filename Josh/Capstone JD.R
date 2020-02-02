setwd("/Users/jcdavis64/Downloads") 
data2019 <- read.csv(file="2019 PFF All Plays.csv", header = TRUE)
library(tidyverse)

# Question 1 = what passing formation will lead to to the most success 
# on any given first down play?
# variables to filter by: formation type, down, garbage time, yards gained/lost
# trickplay, 

shotgun <- data2019[data2019$pff_SHOTGUN == 'S',]
shotgun <- shotgun[shotgun$pff_GARBAGETIME == 0,]
s <- group_by(shotgun,pff_DOWN) %>%
   summarize(mean(pff_GAINLOSSNET))

pistol <- data2019[data2019$pff_PISTOL == 'P',]
pistol <- pistol[pistol$pff_GARBAGETIME == 0,]
p <- group_by(pistol,pff_DOWN) %>%
   summarize(mean(pff_GAINLOSSNET))

rpo <-  data2019[data2019$pff_RUNPASSOPTION == 1,]
rpo <- rpo[rpo$pff_GARBAGETIME == 0,]
r <- group_by(rpo,pff_DOWN) %>%
   summarize(mean(pff_GAINLOSSNET))

# make a table of values and maybe run hypothesis tests for difference in 
# means to see if there's a significance
# Creating a table for data
class(r)
dataframe <- data.frame(r,s,p)
dataframe[[1,4]]
table <- matrix(c(dataframe[[2]],dataframe[[4]],dataframe[[6]]),ncol=3)  
rownames(table) <- c("N/A","1st","2nd","3rd","4th") 
colnames(table) <- c("RPO","Shotgun","Pistol")
table

# which formation type leads to the highest touchdown percentage inside the 
# redzone/10 yard line?

shotgun1 <- data2019[data2019$pff_SHOTGUN == 'S',]
shotgun1 <- shotgun1[which(shotgun1$pff_FIELDPOSITION > 0 
                           & shotgun1$pff_FIELDPOSITION < 11),]
shotgun2 <- shotgun1[shotgun1$pff_TOUCHDOWN != "",]
TDshotgun <- nrow(shotgun2) / nrow(shotgun1)

pistol1 <- data2019[data2019$pff_PISTOL == 'P',]
pistol1 <- pistol1[which(pistol1$pff_FIELDPOSITION > 0 
                         & pistol1$pff_FIELDPOSITION < 11),]
pistol2 <- pistol1[pistol1$pff_TOUCHDOWN != "",]
TDpistol <- nrow(pistol2) / nrow(pistol1)

rpo1 <- data2019[data2019$pff_RUNPASSOPTION == 1,]
rpo1 <- rpo1[which(rpo1$pff_FIELDPOSITION > 0 
                   & rpo1$pff_FIELDPOSITION < 11),]
rpo2 <- rpo1[rpo1$pff_TOUCHDOWN != "",]
TDrpo <- nrow(rpo2) / nrow(rpo1)
TDtable <- matrix(c(TDshotgun,TDpistol,TDrpo))
TDtable 

# Deeppass, distance, drivestart field position, field position, 
# fourth down attempt rate, nohuddle, passdepth

# New question: When should teams start being more aggressive when down
# later in the game?

newdata <- data2019[data2019$pff_QUARTER != 1,]

