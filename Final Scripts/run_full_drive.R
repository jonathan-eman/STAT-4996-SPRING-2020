#this function runs a full drive


#full drive function takes in a starting state and simulates a full drive that either: ends in a "touchdown"
# worth 7 points, or ends in a loss of possession on downs.

full_drive <- function(A, B, C, D, E, F){
  
  is_not_done <- TRUE
  k <- 0 #this allows us to gracefully kick out of a bad while loop. Not needed in production
  #but in case you miscode something while running a while loop, it's always good to not have to kill the kernel.
  
  drive_result <- list(event = NA, score = NA, end_yard = NA, end_time = NA)
  
  #this is the meat of the drive.  Basically, we either score or run out of downs.  NOthing else.
  while(is_not_done) {
    if(B <= 0) {
       print("Time is up")
       drive_result$score <- NA
       drive_result$end_yard <- NA
       drive_result$end_time <- 0
       drive_result$event <- "End of Game"
       is_not_done <- FALSE
    }
     
    if(F == 0) {
       if (D != 4 & A > 0) { # if not 4th down and >0 yards to end zone
          play_type_num <- sample.int(2, size = 1, prob = c(.528, .472))
          play_type <- c("P", "R")[play_type_num]
          
          if (play_type == "P") {
             int <- sample.int(3, size = 1, 
                               prob = c(.540, .370, .090))
             pos_neg <- c("POS", "0", "NEG")[int]
             if (pos_neg == "POS") {
                yards_gained = rlnorm(1, meanlog = 2.04, sdlog = .79) 
             } else if (pos_neg == "NEG") {
                yards_gained = -rweibull(1, shape = 1.580, scale = 5.935)
             } else {
                yards_gained = 0 
             }
             
             time_elapsed = rnorm(1, 20/60, 8/60) 
             
          } else if (play_type == "R") {
             int <- sample.int(2, size = 1, 
                               prob = c(.789, .211))
             pos_neg <- c("POS", "NEG")[int]
             if (pos_neg == "POS") {
                yards_gained = rlnorm(1, meanlog = 1.454, sdlog = .916) 
             } else if (pos_neg == "NEG") {
                yards_gained = -rlnorm(1, meanlog = .874, sdlog = .872)
             }
             
             time_elapsed = rnorm(1, 12/60, 6/60) 
          }
          
          A = A - yards_gained
          B = B - round(time_elapsed, 2)
          C = C - yards_gained
          D = ifelse(C <= 0, 1, D + 1)
          
          if (D == 1) { #if first down, reset YTG to 10
             C = 10
          }
          
          if(play_type == "P") {
             turnover <- sample.int(2, size = 1, 
                                    prob = c(.96, .04)) - 1
             D = ifelse(turnover == 1, 0, D)
          }
          
          if (D == 0) { # interception
             drive_result$end_yard <- A
             drive_result$end_time <- B
             drive_result$event <- "Interception"
             print("Interception")
             is_not_done <- FALSE
          }
          
          k <- k + 1
          
       } else if (D == 4 & A > 0) {
          if (A <= 30) { #if within 30 yards of end zone, simulate field goal
             prob <- field_goal_probability(A)
             field_goal <- sample.int(2, size = 1, prob = c(prob, 1 - prob))
             if (field_goal == 1) {
                drive_result$score <- 3
                drive_result$event <- "FG"
             } else {
                drive_result$end_yard <- A
                drive_result$event <- "Missed FG"
             }
             drive_result$end_time <- B - (5/60)
             is_not_done <- FALSE
          } else { #or else simulate punt
             punt_yards <- rnorm(1, 40.5, 9.7)
             drive_result$end_yard <- ifelse(A - punt_yards <= 0,
                                             25,
                                             A - punt_yards)
             drive_result$end_time <- B - (10/60)
             drive_result$event <- "Punt"
             is_not_done <- FALSE
          }
      
       } else if (A <= 0) {
          drive_result$score <- 7
          drive_result$end_time <- B - (10/60)
          drive_result$event <- "Touchdown"
          is_not_done <- FALSE
       } 
          
       if(k > 100){
         drive_result$end_yard <- C
         drive_result$end_time <- B - (10/60)
         is_not_done <- FALSE
       }
       
    } else if (F == 1) {
       if (A > 0) { # if >0 yards to end zone
          play_type_num <- sample.int(4, size = 1,
                                      prob = c(.4, .25, .25, .1))
          play_type <- c("P", "R", "DP", "TPR")[play_type_num]
          
          if (play_type == "P") {
             int <- sample.int(3, size = 1, 
                               prob = c(.540, .370, .090))
             pos_neg <- c("POS", "0", "NEG")[int]
             if (pos_neg == "POS") {
                yards_gained = rlnorm(1, meanlog = 2.04, sdlog = .79) 
             } else if (pos_neg == "NEG") {
                yards_gained = -rweibull(1, shape = 1.580, scale = 5.935)
             } else {
                yards_gained = 0 
             }
             
             time_elapsed = rnorm(1, 15/60, 4/60) 
             
         } else if (play_type == "R") {
            int <- sample.int(2, size = 1, 
                              prob = c(.789, .211))
            pos_neg <- c("POS", "NEG")[int]
            if (pos_neg == "POS") {
               yards_gained = rlnorm(1, meanlog = 1.454, sdlog = .916) 
            } else if (pos_neg == "NEG") {
               yards_gained = -rlnorm(1, meanlog = .874, sdlog = .872)
            }
            
             time_elapsed = rnorm(1, 10/60, 2/60) 
             
         } else if (play_type == "DP") {
            int <- sample.int(3, size = 1, 
                              prob = c(.424, .56, .016))
            pos_neg <- c("POS", "0", "NEG")[int]
            if (pos_neg == "POS") {
               yards_gained = rburr(1, shape1 = 2.7031, shape2 = 2.6202,
                                    rate = .0225) 
            } else if (pos_neg == "NEG") {
               yards_gained = -rweibull(1, shape = 2.3326, scale = 11.1046) 
            } else {
               yards_gained = 0 
            }
            
             time_elapsed = rnorm(1, 20/60, 6/60) 
            
         } else if (play_type == "TPR") {
             int <- sample.int(2, size = 1, 
                                   prob = c(.90, .10))
             pos_neg <- c("POS", "NEG")[int]
             if (pos_neg == "POS") {
                yards_gained = rlnorm(1, meanlog = 1.4656, sdlog = .9465) 
            } else if (pos_neg == "NEG") {
                yards_gained = -rlnorm(1, meanlog = 1.0617, sdlog = .8847) 
            } else {
                yards_gained = 0 
            }
             
             time_elapsed = rnorm(1, 13/60, 7/60)
          }
          
          A = A - yards_gained
          B = B - round(time_elapsed, 2)
          C = C - yards_gained
          D = ifelse(C <= 0, 1, D + 1)
          
          if(play_type == "DP") {
             turnover <- sample.int(2, size = 1, 
                                    prob = c(.88, .12)) - 1
             D = ifelse(turnover == 1, 0, D)
          } else if(play_type == "P") {
             turnover <- sample.int(2, size = 1, 
                                    prob = c(.96, .04)) - 1
             D = ifelse(turnover == 1, 0, D)
          }
          
          if (D == 1) { #if first down, reset YTG to 10
             C = 10
          }
          
          k <- k + 1
          
         if (D == 5) { # turnover on downs
          drive_result$end_yard <- A
          drive_result$end_time <- B
          drive_result$event <- "Turnover on Downs"
          print("TOD")
          is_not_done <- FALSE
       } 
          
          if (D == 0) { # interception
          drive_result$end_yard <- A
          drive_result$end_time <- B
          drive_result$event <- "Interception"
          print("Interception")
          is_not_done <- FALSE
       }
          
       } else if (A <= 0) {
          drive_result$score <- 7
          drive_result$end_time <- B
          drive_result$event <- "Touchdown"
          print("Touchdown")
          is_not_done <- FALSE
       }
       
       if(k > 100){
          drive_result$end_yard <- C
          drive_result$end_time <- B - (10/60)
          is_not_done <- FALSE
       }
   }
  }
  
  print(drive_result)
}
