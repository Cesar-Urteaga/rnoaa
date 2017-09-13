context("ggplot2's functions")

# We test the three ggplot2's functions: geom_timeline, geom_timeline_label, and
# theme_timeline.

library(ggplot2)
library(dplyr)

test_that("Class is correct once we have used the three ggplot2's functions", {
  raw_data <- get_earthquake_data()
  clean_data <- eq_clean_data(raw_data)
  clean_data <- eq_location_clean(clean_data)
  ggplot2_plot <- clean_data %>%
    filter(COUNTRY == "USA",
           !is.na(EQ_PRIMARY),
           YEAR %in% 2000:2016) %>%
    ggplot(mapping = aes(x = DATE,
                         size = EQ_PRIMARY,
                         color = TOTAL_DEATHS / 1000,
                         label = LOCATION_NAME)
           ) +
    geom_timeline() +
    # We label the five biggest quakes in size.
    geom_timeline_label(n_max = 5,
                        line_height = 1 / 5) +
    labs(size  = "Richter scale value",
         color = "# deaths",
         y = "") +
    theme_timeline()

  expect_s3_class(ggplot2_plot, c("gg", "ggplot"))
  }
  )
