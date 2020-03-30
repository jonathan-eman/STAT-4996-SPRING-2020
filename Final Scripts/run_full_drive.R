#this function runs a full drive


#full drive function takes in a starting state and simulates a full drive that either: ends in a "touchdown"
# worth 7 points, or ends in a loss of possession on downs.

full_drive <- function(S1){
  
  is_not_done <- TRUE
  k <- 0 #this allows us to gracefully kick out of a bad while loop. Not needed in production
  #but in case you miscode something while running a while loop, it's always good to not have to kill the kernel.
  
  drive_result <- list(score = NA, end_yard = NA) #we don't really need to declare this here, I'm just being inefficient
  
  #this is the meat of the drive.  Basically, we either score or run out of downs.  NOthing else.
  while(is_not_done){
    
    y <- floor(rnorm(1, mean=3, sd=1)) #how many yards do you gain.  This is a silly way to sample yards.  Should
    #definitely be replaced with a smarter sampling function, and one that is
    #estimated from the data!

    #print current state
    print_status(S1, y)
    
    C_new <- S1$C - y
    A_new <- S1$A - y
    if(A_new <= 0){
      B_new <- 1
      A_new <- 10
    }else{
      B_new <- S1$B + 1
    }
    if(C_new <=0){
      is_not_done <- FALSE
      drive_result$score <- 7
    }else if(B_new == 5){
      is_not_done <- FALSE
      drive_result$end_yard <- C_new
      turnover_on_downs_print_status(C_new)
    }else{
      k <- k+1
      S1$A <- A_new
      S1$B <- B_new
      S1$C <- C_new
      
    }
    if(k > 100){
      is_not_done <- FALSE
      drive_result$end_yard <- C_new
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