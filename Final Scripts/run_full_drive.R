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
          
          if(play_type == "P") {
             yards_gained = remg(1, -2, 3, .1)
             time_elapsed = rnorm(1, 20/60, 8/60)
          } else {
             yards_gained = remg(1, -2.5, 2, .05)
             time_elapsed = rnorm(1, 12/60, 6/60)
          }
          
          A = A - yards_gained
          B = B - round(time_elapsed, 2)
          C = C - yards_gained
          D = ifelse(C <= 0, 1, C + 1)
          
          if (D == 1) { #if first down, reset YTG to 10
             C = 10
          }
          
          k <- k + 1
          
       } else if (D == 4 & A > 0) {
          if (A <= 30) { #if within 30 yards of end zone, simulate field goal
             field_goal <- sample.int(2, size = 1, prob = c(.7, .3))
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
          play_type_num <- sample.int(4, size = 1, prob = c(.4, .25, .25, .1))
          play_type <- c("P", "R", "DP", "TPR")[play_type_num]
          
          if (play_type == "P") {
             yards_gained = remg(1, -2, 3, .1)
             time_elapsed = rnorm(1, 20/60, 8/60) } 
          else if (play_type == "R") {
             yards_gained = remg(1, -2, 3, .1)
             time_elapsed = rnorm(1, 20/60, 8/60) } 
          else if (play_type == "DP") {
             yards_gained = remg(1, -2, 3, .1)
             time_elapsed = rnorm(1, 20/60, 8/60) }
          else {
             yards_gained = remg(1, -2.5, 2, .05)
             time_elapsed = rnorm(1, 12/60, 6/60)
          }
          
          A = A - yards_gained
          B = B - round(time_elapsed, 2)
          C = C - yards_gained
          D = ifelse(C <= 0, 1, C+1)
          
          if (D == 1) { #if first down, reset YTG to 10
             C = 10
          }
          
          k <- k+1
          
       } else if (A <= 0) {
          drive_result$score <- 7
          drive_result$end_time <- B
          drive_result$event <- "Touchdown"
          is_not_done <- FALSE
       } else if (D == 5) { # turnover on downs
          drive_result$end_yard <- A
          drive_result$end_time <- B
          drive_result$event <- "Turnover on Downs"
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

# #helper function that prints current state in pretty way
# print_status <- function(S1, y){
#   if(S1$C>50){
#     print(paste0("YTG: ", S1$A, " Down: ", S1$B, " LOS: own ", 100-S1$C, " Yards gained: ", y)) #the states are not saved!
#   }else if(S1$C==50){
#     print(paste0("YTG: ", S1$A, " Down: ", S1$B, " LOS: ", S1$C, " Yards gained: ", y))
#   }else if(S1$C<50 & S1$C>20){
#     print(paste0("YTG: ", S1$A, " Down: ", S1$B, " LOS: opponent's ", S1$C, " Yards gained: ", y))
#   }else{
#     print(paste0("RED ZONE ALERT!!! YTG: ", S1$A, " Down: ", S1$B, " LOS: opponent's ", S1$C, " Yards gained: ", y))
#   }
# }
# 
# turnover_on_downs_print_status <- function(y){
#   #y is in terms of your team's last possession.  So to get it in terms of your opponents position
#   #we need to take 100-y
#   x <- 100-y
#   if(x>50){
#     print(paste0("Sorry, you have lost possession on downs.  Your opponent gets the ball on their own ", 100-x, " yard line."))
#   }else if(x==50){
#     print(paste0("Sorry, you have lost possession on downs.  Your opponent gets the ball on the ", x, " yard line."))
#   }else{
#     print(paste0("Sorry, you have lost possession on downs.  Your opponent gets the ball on your ", x, " yard line."))
#   }
# }