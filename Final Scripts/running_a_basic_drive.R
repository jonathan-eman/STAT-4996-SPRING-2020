source('run_full_drive.R')
source('run_to_score.R')

#  states = (A, B, C, D, E, F)

#  Probability breakdown:
#  P(A_{t+1}, B_{t+1}, C_{t+1} | A_t, B_t, C_t) 
#          = P(A_{t+1} | C_{t+1}, C_t, A_t) 
#              P(B_{t+1} | C_{t+1}, C_t, B_t) 
#              P(C_{t+1} | A_t, B_t, C_t)

# A is starting field position of play (ranges from 0 to 100 (so your own 25 would be 75. 0 would be a touchdown))
# B is time remaining in quarter
# C is Down number
# D is yards to first down
# E is score differential
# F is aggressive play indication

# Assume that YG comes from the same simple distribution- this is one of the major changes you would want to make!

S <- list(A=10, B=1, C=75) #starting state- better: simulate a kickoff, but this will do for now

#now we run our main function.  This function isn't necessarily the one you will use to simulate a game
#eventually, because it's not necessarily the best "unit" to loop over, but it's something for now!
#in the very least, you can use it to work out things like punts, and different kinds of scoring opportunities.

run_to_score(S)

#to simulate more, you only need to run the function again.  S does not change, so you don't need to
#re run the line that defines S.
