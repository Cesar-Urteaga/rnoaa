#' Obtains the latest earthquake data from the NOOA
#'
#' The function \code{download_earthquake_data} gets the most recent earthquake
#' data from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{the NOOA's Webpage}.
#'
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom readr read_delim
download_earthquake_data <- function(data_frame_name) {
  url <- "https://www.ngdc.noaa.gov/nndc/struts/results?bt_0=&st_0=&type_17=EXACT&query_17=None+Selected&op_12=eq&v_12=&type_12=Or&query_14=None+Selected&type_3=Like&query_3=&st_1=&bt_2=&st_2=&bt_1=&bt_4=&st_4=&bt_5=&st_5=&bt_6=&st_6=&bt_7=&st_7=&bt_8=&st_8=&bt_9=&st_9=&bt_10=&st_10=&type_11=Exact&query_11=&type_16=Exact&query_16=&bt_18=&st_18=&ge_19=&le_19=&type_20=Like&query_20=&display_look=189&t=101650&s=1&submit_all=Search+Database"
  temporal_file <- tempfile("NOOA", fileext = "txt")
  download.file(url, temporal_file)
  readr::read_delim(temporal_file, delim = "\t")
}

#' Creates the earthquake's time-date and process the geospatial data
#'
#' \code{eq_clean_data} creates a variable with the date of the quake and
#' converts the latitude and longitude variables into a numeric variable.
#'
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object with the cleaned data.
#' @export
#' @importFrom tidyr unite
#' @importFrom dplyr %>% mutate
eq_clean_data <- function(data){
  data %>%
    tidyr::unite(date, YEAR, MONTH, DAY, HOUR, remove = FALSE) %>%
    dplyr::mutate(date = lubridate::ymd_h(date),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE))
}

#' Cleans the location name of the earthquake
#'
#' With \code{eq_location_clean} you can get rid of the country for the variable
#' \code{LOCATION_NAME} and convert the location name into title case.
#'
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom stringr str_to_title
eq_location_clean <- function(data){
  dplyr::mutate(LOCATION_NAME = stringr::str_to_title(LOCATION_NAME),
                LOCATION_NAME = gsub("^.+:", "", LOCATION_NAME),
                LOCATION_NAME = gsub(",", ", ", LOCATION_NAME),
                LOCATION_NAME = gsub("[ ]{2,}", " ", LOCATION_NAME)
                )
  }
