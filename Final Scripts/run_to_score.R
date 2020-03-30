
#This function loops and repeatedly calls the function full_drive until someone scores.
#One of the main things this function does is keep track of which team has the ball.
run_to_score <- function(S1)
{
  team <- 0 #start with team 0 (chosen so that we can work with modular arithmetic)
  k <- 0 #again, if there is something crazy we want to kick out of the while loop.  in production time we
  #would prefer this to be a boolean and not a counter, but this will work for now.
  while(k<101){
    result_drive <- full_drive(S) #team 0 runs their drive
    if(!is.na(result_drive$score))
    {
      print(paste0("team ", team, " scored!")) #if they score, this function is done!
      k <- 10000 #set k to be super large so the while loop stops.
    }else{
      S <- list(A=10, B=1, C=100-result_drive$end_yard) #if they don't score, give the other team the ball
      # where the previous team left off.
      team <- (team+1)%%2 #this is modular arithmetic. Gives us a graceful way to switch between teams
      k <- k+1 #play counter in case things get out of hand.
    }
  }
  #nothing is returned (yet!)
}
