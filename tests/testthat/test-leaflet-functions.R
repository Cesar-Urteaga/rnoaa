context("leaflet's functions")

# We test the two R's leaflet map functions: eq_map and eq_create_label.

library(dplyr)
library(lubridate)

test_that("Class is correct once we have used the two R's leaflet map functions", {
  raw_data <- get_earthquake_data()
  clean_data <- eq_clean_data(raw_data)
  clean_data <- eq_location_clean(clean_data)
  leaflet_map <- clean_data %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")

  expect_s3_class(leaflet_map, c("leaflet", "htmlwidget"))
  }
  )

test_that("The R leaflet maps' popup text labels are correctly created", {
    expect_identical(HTML_text_labels("Guerrero", 6.4, 2),
                     "<b>Location: </b>Guerrero</br><b>Magnitude: </b>6.4</br><b>Total deaths: </b>2</br>")
    expect_identical(HTML_text_labels(NA, 6.4, 2),
                     "<b>Magnitude: </b>6.4</br><b>Total deaths: </b>2</br>")
    expect_identical(HTML_text_labels("Guerrero", NA, 2),
                     "<b>Location: </b>Guerrero</br><b>Total deaths: </b>2</br>")
    expect_identical(HTML_text_labels("Guerrero", 6.4, NA),
                     "<b>Location: </b>Guerrero</br><b>Magnitude: </b>6.4</br>")
    expect_identical(HTML_text_labels("Guerrero", NA, NA),
                     "<b>Location: </b>Guerrero</br>")
    expect_identical(HTML_text_labels(NA, 6.4, NA),
                     "<b>Magnitude: </b>6.4</br>")
    expect_identical(HTML_text_labels(NA, NA, 2),
                     "<b>Total deaths: </b>2</br>")
    expect_identical(HTML_text_labels(NA, NA, NA), character(0))
  }
  )
