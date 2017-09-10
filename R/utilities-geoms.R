# This functions are used in the gglot2's geoms.

# Given a y_value, it returns y_value + increment, where the increment is
# calculated taking into account how ggplot2 renders the y's values on the plot
# for n different points.
next_y_cut <- function(y_value, n, percentage = 2 / 3){
  if (n == 1) return(0.5 + 0.5 * percentage)
  # I have deduced this initial value from how ggplot2 calculates the y's values
  # for discrete variables.
  initial_value <- 3 / (1 + 5 * n)
  cuts <- seq(initial_value, 1 - initial_value, length.out = n)
  index <- 1
  while(!isTRUE(all.equal(y_value, cuts[index], tolerance = 0.00000001)) &
        index <= n)
    index <- index + 1
  # If the function does not find the y_value, it will return NA.
  if (index == n + 1) return(NA)
  if(index == n){
    return(cuts[index] + (1 - cuts[index]) * percentage)
  } else {
    return(cuts[index] + (cuts[index + 1] - cuts[index]) * percentage)
  }
}

# I have taken this function from the ggplot2 package, see:
# https://raw.githubusercontent.com/tidyverse/ggplot2/master/R/utilities-grid.r
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
