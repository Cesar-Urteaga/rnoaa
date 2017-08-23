GeomEarthquakeTimeline <- ggproto(`_class`     = "GeomEarthquakeTimeline",
                                  `_inherit`   = ggplot2::Geom,
                                  setup_data = function(data, params){
                                    if(is.null(data$y)){
                                      data$y = 0.5
                                    }
                                    data
                                  },
                                  required_aes = "x",
                                  default_aes  = ggplot2::aes(shape  = 19,
                                                              colour = "black",
                                                              size   = 1.5,
                                                              fill   = NA,
                                                              alpha  = NA,
                                                              stroke = 0.5),
                                  draw_key    = draw_key_point,
                                  draw_panel  = function(data, panel_scales, coord, na.rm = FALSE) {
                                    # Scales the data x and y to the range [0,1].
                                    coords <- coord$transform(data, panel_scales)
                                    dates_grob <- grid::pointsGrob(
                                      coords$x, coords$y,
                                      pch = coords$shape,
                                      gp = grid::gpar(col = coords$colour,
                                                      fill = scales::alpha(coords$fill, alpha = coords$alpha)
                                      )
                                    )
                                    axis_grob <- grid::polylineGrob(
                                      coords$x, coords$y,
                                      id = coords$group
                                    )
                                    x_axis_grob <- grid::linesGrob(y = c(0, 0))
                                    grid::gList(dates_grob, axis_grob, x_axis_grob)
                                    #print(coords)
                                  }
                                  )

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomEarthquakeTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
  }

library(ggplot2)
clean_data %>%
  filter(COUNTRY %in% c("MEXICO", "CHILE", "CUBA", "PERU", "CHINA")) %>%
  ggplot(mapping = aes(x = date
                       , y = COUNTRY
                       )) +
  geom_timeline()
