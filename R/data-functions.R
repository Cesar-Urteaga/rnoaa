#' Loads the NOAA's Significant Earthquake dataset.
#'
#' It reads the \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{Significant Earthquake database}
#' from a plain file downloaded on September 10, 2017 from the NOAA's Webpage
#' (i.e., no internet connection is needed); in case you want the latest
#' information, you can use the \code{\link{download_earthquake_data}} function.
#'
#' This dataset contains information about destructive earthquakes from 2150
#' B.C. to the present that meet the following criterion: Moderate damage
#' (approximately $1 million or more), 10 or more deaths, Magnitude 7.5 or
#' greater, Modified Mercalli Intensity X or greater, or the earthquake
#' generated a tsunami.
#'
#' Please refer to \href{https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225}{this link}
#' for more information about the description of each variable.
#'
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom readr read_delim
#' @seealso \code{\link{eq_clean_data}} and eq_location_clean for data tidying.
#' @examples
#' raw_data <- get_earthquake_data()
get_earthquake_data <- function(){
  data_file <- system.file("extdata", "noaa.txt", package = "rnoaa")
  readr::read_delim(data_file, delim = "\t")
}

#' Downloads the latest NOAA's Significant Earthquake dataset (requires
#' internet access).
#'
#' The function \code{download_earthquake_data} gets the most recent earthquake
#' data from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{the NOAA's Webpage}.
#'
#' For more information about this dataset, please read the documentation of
#' \code{\link{get_earthquake_data}}.
#'
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom readr read_delim
#' @seealso \code{\link{eq_clean_data}} and \code{\link{eq_location_clean}} for
#' data tidying.
#' @examples
#' raw_data <- download_earthquake_data()
download_earthquake_data <- function(){
  url <- "https://www.ngdc.noaa.gov/nndc/struts/results?bt_0=&st_0=&type_17=EXACT&query_17=None+Selected&op_12=eq&v_12=&type_12=Or&query_14=None+Selected&type_3=Like&query_3=&st_1=&bt_2=&st_2=&bt_1=&bt_4=&st_4=&bt_5=&st_5=&bt_6=&st_6=&bt_7=&st_7=&bt_8=&st_8=&bt_9=&st_9=&bt_10=&st_10=&type_11=Exact&query_11=&type_16=Exact&query_16=&bt_18=&st_18=&ge_19=&le_19=&type_20=Like&query_20=&display_look=189&t=101650&s=1&submit_all=Search+Database"
  temporal_file <- tempfile("NOOA", fileext = "txt")
  download.file(url, temporal_file)
  readr::read_delim(temporal_file, delim = "\t")
  }

#' Creates the earthquakes' date of occurrence and prepares some key variables
#' for data analysis.
#'
#' \code{eq_clean_data} processes the following variables in the NOAA's raw
#' database in order to create the date of each reported earthquake: YEAR,
#' MONTH, and DAY.  So too, it processes the variables with the latitude
#' (LATITUDE), longitude (LONGITUDE), magnitude (EQ_PRIMARY), and total deaths
#' (TOTAL_DEATHS) to make easier the data analysis.
#'
#' @param data A data frame or tibble object with the dates of occurrence of the
#' earthquakes (\code{DATE}) and the variables with the latitude, longitude,
#' magnitude, and total deaths changed to the \code{\link{numeric}} type.
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object with the processed data.
#' @export
#' @importFrom tidyr unite
#' @importFrom dplyr %>% rowwise mutate ungroup
#' @section Notes:
#' One of the benefits of using this function is that it converts dates B.C.E.
#' into the \code{Date} class; futhermore, when the month or/and day is/are
#' missing, the date is approximated at the midpoint of the period.
#' @seealso \code{\link{eq_location_clean}} for process the location name
#' variable.
#' @examples
#' raw_data <- download_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
eq_clean_data <- function(data){
  data %>%
    # If we want to use the approximate_date function by row, we need this.
    dplyr::rowwise() %>%
    dplyr::mutate(DATE         = approximate_date(YEAR, MONTH, DAY),
                  LATITUDE     = as.numeric(LATITUDE),
                  LONGITUDE    = as.numeric(LONGITUDE),
                  EQ_PRIMARY   = as.numeric(EQ_PRIMARY),
                  TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)
                  ) %>%
    dplyr::ungroup()
  }

#' Tidies the earthquakes' location name variable up.
#'
#' With \code{eq_location_clean} you can get rid of the country in the \code{LOCATION_NAME}
#' variable of the NOAA's quake database because this already exists in another
#' variable (\code{COUNTRY}) and convert it into title case.
#'
#' @param data A data frame or tibble object with the \code{LOCATION_NAME}
#' variable processed.
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom purrr map2_chr
#' @importFrom stringr str_to_title str_trim
#' @seealso \code{\link{eq_clean_data}} for process other key variables.
#' @examples
#' raw_data <- download_earthquake_data()
#' clean_data <- eq_location_clean(raw_data)
eq_location_clean <- function(data){
  data <- data %>%
  dplyr::mutate(
    LOCATION_NAME = purrr::map2_chr(COUNTRY, LOCATION_NAME,
      function(country, location){
        gsub(paste0("^",country, "(:|;) "), "", location)
        }),
    LOCATION_NAME = stringr::str_to_title(LOCATION_NAME),
    LOCATION_NAME = stringr::str_trim(LOCATION_NAME),
    LOCATION_NAME = gsub(",", ", ", LOCATION_NAME),
    LOCATION_NAME = gsub(";", "; ", LOCATION_NAME),
    LOCATION_NAME = gsub("[ ]{2,}", " ", LOCATION_NAME)
    )
  data
  }
