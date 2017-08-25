next_y_cut <- function(value, n, percentage = 2 / 3){
  if (n == 1) return(0.5 + 0.5 * percentage)
  initial_value <- 3 / (1 + 5 * n)
  cuts <- seq(initial_value, 1 - initial_value, length.out = n)
  index <- 1
  while(!isTRUE(all.equal(value, cuts[index], tolerance = 0.00000001)) &
        index <= n)
    index <- index + 1
  # If the function do not find the value, it will return NA.
  if (index == n + 1) return(NA)
  if(index == n){
    return(cuts[index] + (1 - cuts[index]) * percentage)
  } else {
    return(cuts[index] + (cuts[index + 1] - cuts[index]) * percentage)
  }
}
# I have taken this function from the ggplot2 package, see:
# https://raw.githubusercontent.com/tidyverse/ggplot2/master/R/utilities-grid.r
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
#-------------------------------------------------------------------------------
GeomTimeline <- ggplot2::ggproto(`_class` = "GeomTimeline",
                          # Because this geom is quite similar to the GeomPoint,
                          # we can recycle it instead of reinventing the wheel.
                          `_inherit`      = ggplot2::GeomPoint,
                          required_aes    = "x",
                          default_aes     = plyr::defaults(
                                              ggplot2::aes(size  = 2,
                                                           alpha = 0.7),
                                              GeomPoint$default_aes
                                              ),
                          draw_panel      = function(data, panel_params,
                                                     coord){
                            dates_grob <- ggplot2::GeomPoint$draw_panel(data,
                              panel_params, coord)
                            coords <- coord$transform(data, panel_params)
                            axis_grob <- grid::polylineGrob(
                              coords$x, coords$y,
                              id = coords$group,
                              gp = grid::gpar(col = gray(0.5))
                              )
                            ggname("geom_timeline",
                                   grid::grobTree(dates_grob, axis_grob)
                                   )
                          }
                          )
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE,
                          na.rm = FALSE,
                          ...){
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
                 }
#-------------------------------------------------------------------------------
GeomTimelineLabel <- ggplot2::ggproto(`_class` = "GeomTimelineLabel",
                               `_inherit`      = ggplot2::GeomSegment,
                               required_aes    = c("x", "label"),
                               draw_panel      = function(
                                                     data, panel_params,
                                                     coord, n_max = n_max,
                                                     fontsize = fontsize,
                                                     angle = angle,
                                                     line_height = line_height){
                                 coords <- coord$transform(data, panel_params)
                                 n <- length(unique(coords$y))
                                 coords$xend <- coords$x
                                 coords$yend <- mapply(next_y_cut, coords$y,
                                                       n,
                                                       percentage = line_height)
                                 coords <- coords %>%
                                   dplyr::group_by(y) %>%
                                   dplyr::arrange(desc(size)) %>%
                                   dplyr::filter(row_number() <= n_max)
                                 segments_grob <- grid::segmentsGrob(
                                  coords$x, coords$y,
                                  coords$xend, coords$yend,
                                  gp = grid::gpar(col = gray(0.5))
                                  )
                                 labels_grob <- grid::textGrob(
                                  label = coords$label,
                                  x = coords$xend, y = coords$yend,
                                  just = c("left", "bottom"),
                                  rot = angle,
                                  gp = gpar(fontsize = fontsize *
                                            ggplot2::.pt)
                                  )
                                 ggname("geom_timeline_label",
                                        grid::grobTree(segments_grob,
                                          labels_grob)
                                        )
                               },
                               draw_key = draw_key_blank
                               )
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity",
                                show.legend = NA,
                                inherit.aes = TRUE,
                                n_max = 1,
                                fontsize = 3.88,
                                angle = 10,
                                line_height = 1 / 3,
                                na.rm = FALSE,
                                ...){
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(
                  n_max = n_max,
                  fontsize = fontsize,
                  angle = angle,
                  line_height = line_height,
                  na.rm = na.rm, ...)
                 )
                 }
#-------------------------------------------------------------------------------
library(ggplot2)
clean_data %>%
  filter(
#    COUNTRY == "MEXICO",
   COUNTRY %in% c("USA" #"MEXICO"),
                  ,"ITALY","IRAN", "UK",
                  "AUSTRALIA", "MEXICO",
                  "CHINA", "CHILE", "PERU",
                  "JAPAN", "ECUADOR",
                  "ARGENTINA", "INDIA"
                  ),
    !is.na(EQ_MAG_MW),
    YEAR %in% as.character(2000:2016)) %>%
  ggplot(mapping = aes(x = DATE
                       , y = COUNTRY
                       , size = as.numeric(EQ_MAG_MW)
                       , color = as.numeric(TOTAL_DEATHS) / 1000
                       , label = LOCATION_NAME
                       )) +
  geom_timeline() +
  geom_timeline_label(n_max = 2, fontsize = 2.5, angle = 10, line_height = 1/4) +
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
        # axis.text.y        = element_blank()
        )
