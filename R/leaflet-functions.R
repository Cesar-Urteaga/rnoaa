#' List of available R's leaflet map providers.
#'
#' Please refer to the documentation of \code{\link[leaflet]{providers}}.
#'
#' @format NULL
#' @usage NULL
#' @export
#' @section Details:
#' This object was exported from the leaflet package so as to use different
#' types of maps in the \code{\link{eq_map}} function.
providers <- leaflet::providers

#' Displays an R leaflet map with the quake's epicenters (requires internet
#' access).
#'
#' \code{eq_map} shows an R leaflet map with the quakes' epicenters; it takes
#' into account the earthquake's magnitude to display the radii.  This function
#' allows to include popup labels based on the contents of a variable (e.g., the
#' quake's occurrence).
#'
#' @param data A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the earthquake epicenters.
#' @param annot_col A character vector in HTML format with the text content to
#' be displayed in the popup text labels of the R leaflet map.
#'
#' @export
#' @importFrom grDevices chull
#' @importFrom dplyr %>% mutate
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addPolygons
#' @importFrom leaflet addLayersControl layersControlOptions hideGroup
#' @importFrom leaflet addMeasure addMiniMap
#' @section Warning:
#' In order to work, the following variables must exist in the \code{data}
#' object: LONGITUDE, LATITUDE, and EQ_PRIMARY (magnitude).
#' @examples
#' require(dplyr)
#' require(lubridate)
#'
#' # Before showing the interactive map, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- eq_location_clean(clean_data)
#'
#' # Displays an R's leaflet map with the epicenters of the earthquakes that
#' # have occurred in Mexico as of 2000.  The interactive map has popup text
#' # labels with the date of occurrence.
#' clean_data %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
eq_map <- function(data, annot_col){
  # Calculates the outline of the observed quake's epicenters.
  outline <- data[grDevices::chull(data$LONGITUDE, data$LATITUDE),]
  # Renders the epicenters in an R's leaflet map, please refer to:
  # https://rstudio.github.io/leaflet/
  map <- data %>%
    dplyr::mutate_(popup_text = as.name(annot_col)) %>%
    leaflet::leaflet() %>%
    leaflet::addProviderTiles("CartoDB.DarkMatter",
                              group = "Dark") %>%
    leaflet::addProviderTiles("OpenStreetMap",
                              group = "OSM") %>%
    leaflet::addProviderTiles("CartoDB.Positron",
                              group = "Gray") %>%
    leaflet::addCircleMarkers(lng = ~LONGITUDE,
                              lat = ~LATITUDE,
                              radius = ~EQ_PRIMARY,
                              popup = ~popup_text,
                              color = "red",
                              opacity = 1,
                              fillOpacity = 0.4,
                              weight = 1,
                              group = "Earthquakes") %>%
    leaflet::addPolygons(data = outline, lng = ~LONGITUDE, lat = ~LATITUDE,
                         fill = F, weight = 2, color = "red",
                         group = "Perimeter") %>%
    leaflet::addLayersControl(
                      baseGroups = c("Dark", "OSM", "Gray"),
                      overlayGroups = c("Earthquakes", "Perimeter"),
                      options = leaflet::layersControlOptions(collapsed = FALSE)
                      ) %>%
    leaflet::hideGroup("Perimeter") %>%
    leaflet::addMeasure(position = "bottomleft",
                        primaryLengthUnit = "miles",
                        secondaryLengthUnit = "kilometers",
                        primaryAreaUnit = "sqmiles",
                        secondaryAreaUnit = "sqmeters") %>%
    leaflet::addMiniMap(tiles = "CartoDB.DarkMatter",
                        toggleDisplay = TRUE,
                        minimized = TRUE)
  map
}

#' Creates R leaflet maps' popup text labels with the quake's traits.
#'
#' \code{eq_create_label} is useful to create the information that will be
#' displayed in the text popup labels of the \code{\link{eq_map}} function.
#'
#' @param data A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the earthquake traits (i.e., location name, magnitude, and
#'    total deaths).
#'
#' @export
#' @importFrom purrr pmap_chr
#' @importFrom scales comma
#' @section Warning:
#' In order to work, the following variables must exist in the \code{data}
#' object: LOCATION_NAME (location name), EQ_PRIMARY (magnitude), and
#' TOTAL_DEATHS (total deaths).
#' @examples
#' require(dplyr)
#' require(lubridate)
#'
#' # Before showing the interactive map, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- eq_location_clean(clean_data)
#'
#' # Displays an R's leaflet map with the epicenters of the earthquakes that
#' # have occurred in Mexico as of 2000.  The interactive map has popup text
#' # labels with the location, magnitude, and total deaths.
#' clean_data %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
eq_create_label <- function(data){
  with(data,
       # Creates the HTML labels with the earthquake characteristics.
       purrr::pmap_chr(list(LOCATION_NAME,
                            EQ_PRIMARY,
                            TOTAL_DEATHS),
                       HTML_text_labels)
       )
  }

