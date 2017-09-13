context("data functions")

# We test the four functions for data processing: download_earthquake_data,
# get_earthquake_data, eq_clean_data, and eq_location_clean.

# Functions for get the data:
downloaded_data <- download_earthquake_data()
loaded_data <- get_earthquake_data()

# Functions for tidy the data up:
clean_data <- eq_clean_data(downloaded_data)
location_clean_data <- eq_location_clean(downloaded_data)

test_that("class is correct", {
  # skip("Skipped for lack of internet access")

  # We expect that downloaded data is a tibble or data frame object and is of
  # the same class without regard whether the data was downloaded or loaded.
  expect_s3_class(downloaded_data, c("tbl_df", "tbl", "data.frame"))
  expect_identical(class(downloaded_data), class(loaded_data))

  # Also, we hope that the class will not change after the processing has
  # carried out.
  expect_s3_class(clean_data, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(location_clean_data, c("tbl_df", "tbl", "data.frame"))
  }
  )

test_that("All the required variables are present in the dataset", {
    required_variables <- c("DAY", "MONTH", "YEAR",
                            "COUNTRY", "LOCATION_NAME",
                            "EQ_PRIMARY", "TOTAL_DEATHS")

    names_dd <- names(downloaded_data)
    names_ld <- names(loaded_data)

    # We check that the key variables are in the NOAA's quake dataset.
    expect_identical(required_variables,
                     intersect(required_variables, names_dd))
    expect_identical(required_variables,
                     intersect(required_variables, names_ld))
  }
  )

# Once the above tests have already passed, we can assume that the
# downloaded_data and loaded_data are equivalent; so, henceforth we will use the
# downloaded_data.  In addition, this enforces to work with the latest changes.

test_that("Before the processing, the variables are of the expected type", {
          with(downloaded_data, {
               expect_type(YEAR, "integer")
               expect_type(MONTH, "integer")
               expect_type(DAY, "integer")
               expect_type(LATITUDE, "character")
               expect_type(LONGITUDE, "character")
               expect_type(EQ_PRIMARY, "character")
               expect_type(TOTAL_DEATHS, "character")
               expect_type(COUNTRY, "character")
               expect_type(LOCATION_NAME, "character")
               }
               )
  }
  )

test_that("After the processing, the variables are of the expected class/type",
          {
           with(clean_data, {
                expect_s3_class(DATE, "Date")
                expect_type(LATITUDE, "double")
                expect_type(LONGITUDE, "double")
                expect_type(EQ_PRIMARY, "double")
                expect_type(TOTAL_DEATHS, "double")
                }
                )
           with(location_clean_data, {
                expect_type(LOCATION_NAME, "character")
                }
                )
           }
  )

test_that("The dates of occurrence are correctly transformed/approximated", {
    # We set up a tolerance level in advance to check equality.
    chk_equality <- function(x, y) expect_equal(x, y, tolerance = 0.000001)

    # For dates in Common Era (C.E.), that is, when YEAR >= 0.
    chk_equality(create_date(   0,  1,  1), as.Date("0000-01-01"))
    chk_equality(create_date(2010, 10, 10), as.Date("2010-10-10"))
    #   When the month or/and day is/are missing:
    chk_equality(create_date(   1, 10, NA), as.Date("0001-10-15"))
    chk_equality(create_date(   1,  2, NA), as.Date("0001-02-14"))
    chk_equality(create_date(   1, NA, NA), as.Date("0001-07-02"))
    chk_equality(create_date(   1, NA, 10), as.Date("0001-07-02"))

    # For dates Before Common Era (B.C.E.), that is, when YEAR < 0.
    origin <- "1970-01-01"
    chk_equality(create_date(-2010, 10, 10), as.Date(-1453383, origin = origin))
    #   When the month or/and day is/are missing:
    chk_equality(create_date(-   1, 10, NA), as.Date( -719606, origin = origin))
    chk_equality(create_date(-   1,  2, NA), as.Date( -719849, origin = origin))
    chk_equality(create_date(-   1, NA, NA), as.Date( -719711, origin = origin))
    chk_equality(create_date(-   1, NA, 10), as.Date( -719711, origin = origin))
  }
  )

test_that("The quakes' location names are correctly cleaned", {
  # We test a few cases:
  expect_identical(remove_country_from_location("JORDAN", "JORDAN: BAB-A-DARAA,AL-KARAK"),
                   "Bab-A-Daraa, Al-Karak")
  expect_identical(remove_country_from_location("CHINA", "CHINA:  YUNNAN:  LUDIAN:   LONGTOUSHAN, ZHAOTONG"),
                   "Yunnan: Ludian: Longtoushan, Zhaotong")
  expect_identical(remove_country_from_location("SWITZERLAND", "SWITZERLAND"),
                   "Switzerland")
  expect_identical(remove_country_from_location("AFGHANISTAN", "AFGHANISTAN; PAKISTAN:  MANSEHRA, SHANGLA"),
                   "Pakistan: Mansehra, Shangla")
  }
  )
