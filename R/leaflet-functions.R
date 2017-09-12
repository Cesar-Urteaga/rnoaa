#' @export
# Exports the list of leaflet's providers; it is necessary so as to the other
# functions work.
providers <- leaflet::providers

#' Displays an R leaflet map with the quake's epicenters (requires internet
#' access).
#'
#' The epicenters take into account the quake's magnitude to display the radii.
#' It allows to include popup labels based on the contents of a variable.
#'
#' @param data A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the earthquake epicenters.
#' @param annot_col A character vector in HTML format with text content to be
#' displayed in popup text labels (i.e., text shown in click-interactive points).
#'
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addPolygons
#' @importFrom leaflet addLayersControl layersControlOptions hideGroup
#' @importFrom leaflet addMeasure addMiniMap
#' @section Warning:
#' In order to work, the following variables must exist in the \code{data}
#' object: LONGITUDE, LATITUDE, and EQ_PRIMARY (magnitude).
#' @examples
#' require(dplyr)
#'
#' # Before showing the interactive map, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- eq_location_clean(clean_data)
#'
#' # Displays an R's leaflet map with the epicenters of earthquakes occurred in
#' # Mexico as of 2000.  The interactive map has popup text labels with the
#' # quake's date.
#' clean_data %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
eq_map <- function(data, annot_col){
  # Calculates the outline of the observed quake's epicenters.
  outline <- data[chull(data$LONGITUDE, data$LATITUDE),]
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
#'
#' # Before showing the interactive map, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- eq_location_clean(clean_data)
#'
#' # Displays an R's leaflet map with the epicenters of earthquakes occurred in
#' # Mexico as of 2000.  The interactive map has popup text labels with the
#' # location, magnitude, and total deaths.
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
                       function(ln, mag, td)
                         paste0("if"(!is.na(ln),
                                     paste0("<b>Location: </b>",
                                            trimws(ln),
                                            "</br>")
                                     ),
                                "if"(!is.na(mag),
                                     paste0("<b>Magnitude: </b>",
                                            trimws(mag),
                                            "</br>")
                                     ),
                                "if"(!is.na(td),
                                     paste0("<b>Total deaths: </b>",
                                            scales::comma(as.numeric(td)),
                                            "</br>")
                                     )
                                )
                       )
       )
}

