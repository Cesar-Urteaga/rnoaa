GeomTimeline <- ggproto(`_class`        = "GeomTimeline",
                        `_inherit`      = ggplot2::Geom,
                        required_aes    = "x",
                        non_missing_aes = c("size", "shape", "colour"),
                        default_aes     = aes(shape = 19, colour = "black",
                                              size  = 2,  fill = NA,
                                              alpha = 0.7, stroke = 0.5
                                              ),
                        setup_data      = function(data, params){
                          if(is.null(data$y)){
                            data$y = 0.5
                          }
                          data
                        },
                        draw_panel      = function(data, panel_params,
                                                   coord, na.rm = FALSE){
                          coords <- coord$transform(data, panel_params)
                          dates_grob <- grid::pointsGrob(
                            coords$x, coords$y,
                            pch = coords$shape,
                            gp = grid::gpar(col = scales::alpha(coords$colour,
                                                                coords$alpha),
                                      fill = scales::alpha(coords$fill,
                                                           coords$alpha),
                                      fontsize = coords$size * .pt +
                                                 coords$stroke * .stroke / 2,
                                      lwd = coords$stroke * .stroke / 2
                                      )
                            )
                          axis_grob <- grid::polylineGrob(
                            coords$x, coords$y,
                            id = coords$group,
                            gp = grid::gpar(col = gray(0.5))
                            )
                          #grid::gList(dates_grob, axis_grob)
                          print(coords)
                        },
                        draw_key = draw_key_point
                        )

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...){
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
                 )
                 }



GeomTimelineLabel <- ggproto(`_class`     = "GeomTimelineLabel",
                          `_inherit`      = ggplot2::Geom,
                          required_aes    = c("x", "label"),
                          non_missing_aes = c("size", "shape", "colour"),
                          default_aes     = aes(shape = 19, colour = "black",
                                                size  = 2,  fill = NA,
                                                alpha = 0.7, stroke = 0.5
                                                ),
                          setup_data      = function(data, params){
                            if(is.null(data$y)){
                              data$y = 0.5
                            }
                            data
                          },
                          draw_panel      = function(data, panel_params,
                                                     coord, na.rm = FALSE){
                            coords <- coord$transform(data, panel_params)
                            dates_grob <- grid::pointsGrob(
                              coords$x, coords$y,
                              pch = coords$shape,
                              gp = grid::gpar(col = scales::alpha(coords$colour,
                                                                  coords$alpha),
                                        fill = scales::alpha(coords$fill,
                                                             coords$alpha),
                                        fontsize = coords$size * .pt +
                                                   coords$stroke * .stroke / 2,
                                        lwd = coords$stroke * .stroke / 2
                                        )
                              )
                            axis_grob <- grid::polylineGrob(
                              coords$x, coords$y,
                              id = coords$group,
                              gp = grid::gpar(col = gray(0.5))
                              )
                            #grid::gList(dates_grob, axis_grob)
                            print(coords)
                          },
                          draw_key = draw_key_point
                          )

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, ...){
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
                 )
                 }


library(ggplot2)
clean_data %>%
  filter(
    COUNTRY %in% c("USA", "MEXICO"),
    YEAR %in% as.character(2000:2016)) %>%
  ggplot(mapping = aes(x = DATE
                       , y = COUNTRY
  #                     , size = as.numeric(EQ_MAG_MW)
                       , color = as.numeric(TOTAL_DEATHS)
                       )) +
  geom_timeline() +
  #geom_jitter(aes(y = COUNTRY)) +
  labs(size = "Richter scale value", color = "# deaths", y = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        # remove the vertical grid lines
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y        =element_blank())
