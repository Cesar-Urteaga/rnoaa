#' Displays an R Leaflet map with the quake's epicenters.
#'
#' @param data A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the earthquake epicenters.
#'
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addPolygons
#' @importFrom leaflet addLayersControl layersControlOptions hideGroup
#' @importFrom leaflet addMeasure addMiniMap
eq_map <- function(data, annot_col){
  # Calculates the outline of the observed quake's epicenters.
  outline <- data[chull(data$LONGITUDE, data$LATITUDE),]
  # Renders the epicenters an R's Leaflet map, please refer to:
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

#' Creates the R Leaflet maps' labels with the quake's traits.
#'
#' @param data A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the earthquake traits(i.e., location name, magnitude, and
#'    total deaths).
#'
#' @export
#' @importFrom purrr pmap_chr
#' @importFrom scales comma
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

