context("ggplot2's functions")

# We test the three ggplot2's functions: geom_timeline, geom_timeline_label, and
# theme_timeline.

library(ggplot2)
library(dplyr)
library(grid)

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

test_that("Class of auxiliary functions are correct", {
  expect_s3_class(ggname("Circle", circleGrob()),
                  "grob"
                  )
  }
  )

test_that("Y-coordinates for each level are correctly set up", {
  expect_identical(next_y_cut(0.5, 1), 0.5 + 0.5 * 2 / 3)
  expect_identical(next_y_cut(0.5, 2), NA)
  expect_equal(next_y_cut(3 / (1 + 5 * 2), 2, 0), 0.2727273,
               tolerance = 0.000001)
  expect_equal(next_y_cut(1 - 3 / (1 + 5 * 2), 2, 0), 0.7272727,
               tolerance = 0.000001)
  }
  )
