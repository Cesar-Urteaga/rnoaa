#' Obtains the latest earthquake data.
#'
#' The function \code{download_earthquake_data} gets the most recent earthquake
#' data from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{the NOAA's Webpage}.
#'
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom readr read_delim
#' @examples
#' raw_data <- download_earthquake_data()
download_earthquake_data <- function(){
  url <- "https://www.ngdc.noaa.gov/nndc/struts/results?bt_0=&st_0=&type_17=EXACT&query_17=None+Selected&op_12=eq&v_12=&type_12=Or&query_14=None+Selected&type_3=Like&query_3=&st_1=&bt_2=&st_2=&bt_1=&bt_4=&st_4=&bt_5=&st_5=&bt_6=&st_6=&bt_7=&st_7=&bt_8=&st_8=&bt_9=&st_9=&bt_10=&st_10=&type_11=Exact&query_11=&type_16=Exact&query_16=&bt_18=&st_18=&ge_19=&le_19=&type_20=Like&query_20=&display_look=189&t=101650&s=1&submit_all=Search+Database"
  temporal_file <- tempfile("NOOA", fileext = "txt")
  download.file(url, temporal_file)
  readr::read_delim(temporal_file, delim = "\t")
  }

#' Prepares some earthquake's key variables.
#'
#' \code{eq_clean_data} processes the earthquake's variables with the date,
#' latitude (LATITUDE), longitude (LONGITUDE), magnitude (EQ_PRIMARY), and total
#' deaths (TOTAL_DEATHS) to make easier the data analysis.
#'
#' @param data A data frame or tibble object with the above variables.
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object with the processed data.
#' @export
#' @importFrom tidyr unite
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate ymd_h
#' @examples
#' raw_data <- download_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
eq_clean_data <- function(data){
  data %>%
    tidyr::unite(DATE, YEAR, MONTH, DAY, HOUR, remove = FALSE) %>%
    dplyr::mutate(DATE         = lubridate::ymd_h(DATE),
                  LATITUDE     = as.numeric(LATITUDE),
                  LONGITUDE    = as.numeric(LONGITUDE),
                  EQ_PRIMARY   = as.numeric(EQ_PRIMARY),
                  TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)
                  )
  }

#' Cleans the location name of the earthquake.
#'
#' With \code{eq_location_clean} you can get rid of the country for the variable
#' \code{LOCATION_NAME} and convert the location name into title case.
#'
#' @param data A data frame or tibble object with the above variable.
#' @return Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#' object.
#' @export
#' @importFrom stringr str_to_title
#' raw_data <- download_earthquake_data()
#' clean_data <- eq_location_clean(raw_data)
eq_location_clean <- function(data){
  data %>%
  dplyr::mutate(LOCATION_NAME = stringr::str_to_title(LOCATION_NAME),
                LOCATION_NAME = gsub("^.+:", "", LOCATION_NAME),
                LOCATION_NAME = gsub(",", ", ", LOCATION_NAME),
                LOCATION_NAME = gsub("[ ]{2,}", " ", LOCATION_NAME)
                )
  }
