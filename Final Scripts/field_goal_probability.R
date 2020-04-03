field_goal_probability <- function(x) {
   exp(2.410322 - 0.064872*x) / (1 + exp(2.410322 - 0.064872*x))
}