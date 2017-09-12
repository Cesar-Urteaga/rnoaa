# This function creates the quake's dates.
approximate_date <- function(year, month, day){
  # Constant that represents the days from January 01, 0000 to January 01, 1970.
  days_from_1970 <- 719528
  # We set up the midpoint of the period when the month or/and day is/are
  # missing.
  if(is.na(month)){
    # July 2 is the midpoint of a common year, please see:
    #   https://en.wikipedia.org/wiki/July_2
    month <- 7
    #   Adjustment for negative years.
    if(year < 0 &
       (((abs(year + 1)%% 4 == 0) & (abs(year + 1)%% 100 != 0)) |
        (abs(year + 1) %% 400 == 0)
        )
       ){
      day <- 1
    } else
    day   <- 2
  }
  if(is.na(day)) day <- ifelse(month == 2, 14, 15)
  # Stores the given date considering that the given year is positive.
  temporal_date <- as.Date(paste(sprintf("%04d", abs(year)),
                                 sprintf("%02d", month),
                                 sprintf("%02d", day),
                                 sep = "-")
                           )
  # Since R does not handle dates B.C.E., we need to calculate them.
  if(year < 0){
    # For dates B.C.E., we need to calculate the lengths A-00 and C-B:
    #
    # 00                  A    B      C
    # |+++++++++++++++++++|----|------|
    #
    # where
    #   +: Previous whole years.
    #   -: Last year of the given date.
    #
    # N.B.: B is the actual date and C is the last day of the year of the given
    # date.
    date_C <- as.Date(paste(sprintf("%04d", abs(year)),
                            "12",
                            "31",
                            sep = "-")
                      )
    date_B <- temporal_date
    CB_length <- as.numeric(difftime(date_C, date_B))
    if(year == -1){
      # i.e., there are not whole years.
      as.Date(-(CB_length + days_from_1970 + 1),
              origin = "1970-01-01")
    } else {
      date_A <- as.Date(paste(sprintf("%04d", abs(year)- 1),
                              "01",
                              "01",
                              sep = "-")
                        )
      A0_length <- as.numeric(difftime(date_A, "0000-01-01"))
      as.Date(-(CB_length + A0_length + days_from_1970),
              origin = "1970-01-01")
    }
  } else
    temporal_date
}

#-------------------------------------------------------------------------------
# These functions are used in the gglot2's geoms.

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
