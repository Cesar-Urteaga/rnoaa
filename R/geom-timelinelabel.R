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

#' Class of the geom_timeline_label
#'
#' Please refer to the documentation of the \code{\link{geom_timeline_label}}.
GeomTimelineLabel <- ggplot2::ggproto(`_class` = "GeomTimelineLabel",
                               `_inherit`      = ggplot2::GeomSegment,
                               required_aes    = c("x", "label"),
                               default_aes     = plyr::defaults(
                                                ggplot2::aes(y = 0.5),
                                                ggplot2::GeomSegment$default_aes
                                                ),
                               draw_panel      = function(
                                                     data, panel_params,
                                                     coord, n_max = n_max,
                                                     fontsize = fontsize,
                                                     angle = angle,
                                                     line_height = line_height){
                                 coords <- coord$transform(data, panel_params)
                                 n <- length(unique(coords$y))
                                 coords$xend <- coords$x
                                 # We calculate the end points of the vertical
                                 # lines with the labels.  The function
                                 # next_y_cut calculates them so that they do
                                 # not overlap.
                                 coords$yend <- mapply(next_y_cut, coords$y,
                                                       n,
                                                       percentage = line_height)
                                 # If there is not variation for the size
                                 # aesthetic, it will label the last points.
                                 if(length(unique(coords$size)) == 1){
                                  coords <- coords %>%
                                    dplyr::group_by(y) %>%
                                    dplyr::arrange(desc(x)) %>%
                                    dplyr::filter(row_number() <= n_max)
                                  alignment = c("right", "bottom")
                                  angle_slope = -1
                                 } else {
                                 coords <- coords %>%
                                   dplyr::group_by(y) %>%
                                   dplyr::arrange(desc(size)) %>%
                                   dplyr::filter(row_number() <= n_max)
                                   alignment = c("left", "bottom")
                                   angle_slope = 1
                                 }
                                 # Creates the vertical lines for those
                                 # observations that display labels.
                                 segments_grob <- grid::segmentsGrob(
                                  coords$x, coords$y,
                                  coords$xend, coords$yend,
                                  gp = grid::gpar(col = gray(0.5))
                                  )
                                 # Generates the labels.
                                 labels_grob <- grid::textGrob(
                                  label = coords$label,
                                  x = coords$xend, y = coords$yend,
                                  just = alignment,
                                  rot = angle * angle_slope,
                                  gp = grid::gpar(fontsize = fontsize *
                                                  ggplot2::.pt)
                                  )
                                 ggname("geom_timeline_label",
                                        grid::grobTree(segments_grob,
                                          labels_grob)
                                        )
                               },
                               draw_key = ggplot2::draw_key_blank
                               )
