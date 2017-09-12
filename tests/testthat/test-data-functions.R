context("data functions")

test_that("class is correct", {
  skip("Skipped when we do not have internet access")

  # We expect that downloaded data is a tibble or data frame object and is of
  # the same class without regard whether the data was downloaded or loaded.
  downloaded_data <- download_earthquake_data()
  loaded_data <- get_earthquake_data()

  # Also, we hope that the class will not change after the processing has
  # carried out.
  clean_data <- eq_clean_data(loaded_data)
  clean_location_data <- eq_location_clean(loaded_data)

  expect_s3_class(downloaded_data, c("tbl_df", "tbl", "data.frame"))
  expect_identical(class(downloaded_data), class(loaded_data))
  expect_s3_class(clean_data, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(clean_location_data, c("tbl_df", "tbl", "data.frame"))
  }
  )

# test_that("All the required variables are present", {
#     required_variables <- c("DAY", "MONTH", "YEAR")
#   }
#   )
