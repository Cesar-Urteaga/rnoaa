# https://rstudio.github.io/leaflet/

eq_map <- function(data, annot_col){
  outline <- data[chull(data$LONGITUDE, data$LATITUDE),]

  map <- data %>%
    dplyr::mutate_(popup_text = as.name(annot_col)) %>%
    leaflet::leaflet() %>%
    # Base groups
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Gray") %>%
    # Overlay groups
    leaflet::addCircleMarkers(lng = ~LONGITUDE,
                              lat = ~LATITUDE,
                              radius = ~EQ_PRIMARY,
                              popup = ~popup_text,
                              color = "red",
                              opacity = 1,
                              fillOpacity = 0.4,
                              # Defines the width of each point.
                              weight = 1,
                              group = "Quakes") %>%
    addPolygons(data = outline, lng = ~LONGITUDE, lat = ~LATITUDE,
                fill = F, weight = 2, color = "red", group = "Outline") %>%
    # Layers control
    addLayersControl(
      baseGroups = c("OSM", "Dark", "Gray"),
      overlayGroups = c("Quakes", "Outline"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMeasure(position = "bottomleft") %>%
    addMiniMap(tiles = providers$CartoDB.DarkMatter,
               toggleDisplay = TRUE, minimized = TRUE) %>%
    hideGroup("Outline")
  map
}

eq_create_label <- function(data){
  with(data,
       purrr::pmap_chr(list(LOCATION_NAME,
                            EQ_PRIMARY,
                            TOTAL_DEATHS),
                       function(x, y, z)
                         paste0("if"(!is.na(x),
                                     paste0("<b>Location: </b>",
                                            trimws(x),
                                            "</br>")
                                     ),
                                "if"(!is.na(y),
                                     paste0("<b>Magnitude: </b>",
                                            trimws(y),
                                            "</br>")
                                     ),
                                "if"(!is.na(z),
                                     paste0("<b>Total deaths: </b>",
                                            scales::comma(as.numeric(z)),
                                            "</br>")
                                     )
                                )
                       )
  )
  }

