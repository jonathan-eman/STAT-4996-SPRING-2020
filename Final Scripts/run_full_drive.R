#this function runs a full drive


#full drive function takes in a starting state and simulates a full drive that either: ends in a "touchdown"
# worth 7 points, or ends in a loss of possession on downs.

full_drive <- function(S1){
  
  is_not_done <- TRUE
  k <- 0 #this allows us to gracefully kick out of a bad while loop. Not needed in production
  #but in case you miscode something while running a while loop, it's always good to not have to kill the kernel.
  
  drive_result <- list(score = NA, end_yard = NA, end_time = NA)
  
  #this is the meat of the drive.  Basically, we either score or run out of downs.  NOthing else.
  while(is_not_done) {
    if(F = 0) {
       if(C != 4 & A > 0) { # if not 4th down and >0 yards to end zone
          play_type_num <- sample.int(2, size = 1, prob = c(.528, .472))
          play_type <- c("P", "R")[play_type_num]
          
          if(play_type == "P") {
             yards_gained = remg(1, -2, 3, .1)
             time_elapsed = rnorm(1, 20/60, 8/60)
          } else {
             yards_gained = remg(1, -2.5, 2, .05)
             time_elapsed = rnorm(1, 12/60, 6/60)
          }
          
          A_new = A-yards_gained
          B_new = B-round(time_elapsed, 2)
          C_new = C-yards_gained
          D_new = ifelse(C_new <= 0, 1, C+1)
          
          if (D_new == 1) { #if first down, reset YTG to 10
             C_new = 10
          }
          
          k <- k+1
          S1$A <- A_new
          S1$B <- B_new
          S1$C <- C_new
          S1$D <- D_new
          
       } else if (C == 4 & A > 0) {
          if (A <= 30) { #if within 30 yards of end zone, simulate field goal
             field_goal <- sample.int(2, size = 1, prob = c(.7, .3))
             if (field_goal == 1) {
                is_not_done <- FALSE
                drive_result$score <- 3
             } else {
                is_not_done <- FALSE
                drive_result$end_yard <- A
             }
             drive_result$end_time <- B-(5/60)
          } else { #or else simulate punt
             punt_yards <- rnorm(1, 35, 3)
             is_not_done <- FALSE
             drive_result$end_yard <- A + punt_yards
             drive_result$end_time <- B-(10/60)
          }
      
       } else if (A <= 0) {
          is_not_done <- FALSE
          drive_result$score <- 7
       } 
          
       if(k > 100){
         is_not_done <- FALSE
         drive_result$end_yard <- C_new
       }
       }
  }
  
  drive_result #return drive result which is (score, end_yard).  One of those will always be NA.
}

#helper function that prints current state in pretty way
print_status <- function(S1, y){
  if(S1$C>50){
    print(paste0("YTG: ", S1$A, " Down: ", S1$B, " LOS: own ", 100-S1$C, " Yards gained: ", y)) #the states are not saved!
  }else if(S1$C==50){
    print(paste0("YTG: ", S1$A, " Down: ", S1$B, " LOS: ", S1$C, " Yards gained: ", y))
  }else if(S1$C<50 & S1$C>20){
    print(paste0("YTG: ", S1$A, " Down: ", S1$B, " LOS: opponent's ", S1$C, " Yards gained: ", y))
  }else{
    print(paste0("RED ZONE ALERT!!! YTG: ", S1$A, " Down: ", S1$B, " LOS: opponent's ", S1$C, " Yards gained: ", y))
  }
}

turnover_on_downs_print_status <- function(y){
  #y is in terms of your team's last possession.  So to get it in terms of your opponents position
  #we need to take 100-y
  x <- 100-y
  if(x>50){
    print(paste0("Sorry, you have lost possession on downs.  Your opponent gets the ball on their own ", 100-x, " yard line."))
  }else if(x==50){
    print(paste0("Sorry, you have lost possession on downs.  Your opponent gets the ball on the ", x, " yard line."))
  }else{
    print(paste0("Sorry, you have lost possession on downs.  Your opponent gets the ball on your ", x, " yard line."))
  }
}