setwd("C:/Users/student/Documents/UVA/STAT 4996")
PFF2019 <- read.csv("2019 PFF All Plays.csv", header = TRUE)
head(PFF2019)

fourth <- PFF2019[, c("pff_GAMEID", "pff_QUARTER", "pff_CLOCK", 
                      "pff_SCOREDIFFERENTIAL", "pff_DOWN", "pff_DISTANCE", 
                      "pff_RUNPASS", "pff_SCORE", "pff_OFFSCORE", "pff_DEFSCORE", "pff_DRIVEENDEVENT")]
fourth <- fourth[fourth$pff_SCOREDIFFERENTIAL < 0, ]
fourth <- fourth[fourth$pff_DOWN == 4, ]
fourth <- fourth[fourth$pff_RUNPASS != "", ]
fourth <- fourth[fourth$pff_RUNPASS != "X", ]
fourth

last_drive <- PFF2019[, c("pff_GAMEID", "pff_QUARTER", "pff_CLOCK", 
                          "pff_SCOREDIFFERENTIAL", "pff_DOWN", "pff_DISTANCE", 
                          "pff_RUNPASS", "pff_SCORE", "pff_OFFSCORE", "pff_DEFSCORE", "pff_DRIVEENDEVENT")]
last_drive <- last_drive[last_drive$pff_DRIVEENDEVENT == "END OF GAME", ]
last_drive

table(fourth$pff_DOWN, fourth$pff_QUARTER)


