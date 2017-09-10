#' Timeline label ggplot2's geom
#'
#' \code{geom_timeline_label} works together with \code{\link{geom_timeline}}
#' and shows the labels of the higher values for the aesthetic \code{size}
#' (e.g., the magnitude of a quake) or the last observations if this aesthetic
#' is omitted.
#'
#' @inheritParams geom_timeline
#' @param n_max Number of labels to be depicted by level.  If the aesthetic
#' \code{size} is specified, it will be used to show the highest values of this.
#' When this aesthetic is omitted, it will display the last observations.
#' Defaults to 3.
#' @param line_height Length (expressed as proportion of the available vertical
#' space) of the vertical lines attached to the text labels.  Defaults to 2/3.
#' @param fontsize Size of the font. Default to 3.88.
#' @param angle Rotation of the label in degrees; it is counter clockwise.
#' Defaults to 45.
#' @export
#' @section Details:
#' Parameter \code{line_height} should be used with care because as long as it
#' is lower than 1, it will not overlap with other observations.
#' @examples
#' require(dplyr)
#' require(ggplot2)
#'
#' # Before using the geom, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- eq_location_clean(clean_data)
#'
#' # Quakes in USA
#' clean_data %>%
#'   filter(COUNTRY == "USA",
#'          !is.na(EQ_PRIMARY),
#'          YEAR %in% 2000:2016) %>%
#'   ggplot(mapping = aes(x = DATE,
#'                        size = EQ_PRIMARY,
#'                        color = TOTAL_DEATHS / 1000,
#'                        label = LOCATION_NAME)
#'          ) +
#'   geom_timeline() +
#'   # We label the five biggest quakes in size.
#'   geom_timeline_label(n_max = 5,
#'                       line_height = 1 / 5) +
#'   labs(size  = "Richter scale value",
#'        color = "# deaths",
#'        y = "") +
#'   theme_timeline()
#'
#' # Quakes in USA and China
#' clean_data %>%
#'   filter(COUNTRY %in% c("USA", "CHINA"),
#'          !is.na(EQ_PRIMARY),
#'          YEAR %in% 2000:2016) %>%
#'   ggplot(mapping = aes(x = DATE,
#'                        y = COUNTRY,
#'                        color = TOTAL_DEATHS / 1000,
#'                        label = LOCATION_NAME)
#'          ) +
#'   geom_timeline() +
#'   geom_timeline_label(mapping = aes(size = EQ_PRIMARY),
#'                                     n_max = 5,
#'                                     line_height = 1 / 10) +
#'   labs(color = "# deaths in thousands",
#'        y = "") +
#'   guides(size = FALSE) +
#'   theme_timeline()
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity",
                                show.legend = NA,
                                inherit.aes = TRUE,
                                n_max = 3,
                                line_height = 2 / 3,
                                fontsize = 3.88,
                                angle = 45,
                                na.rm = FALSE,
                                ...){
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(
                                                      n_max = n_max,
                                                      line_height = line_height,
                                                      fontsize = fontsize,
                                                      angle = angle,
                                                      na.rm = na.rm,
                                                      ...
                                                          )
                 )
                 }

#' Class of the geom_timeline_label
#'
#' Please refer to the documentation of the \code{\link{geom_timeline_label}}.
#'
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto GeomSegment aes .pt draw_key_blank
#' @importFrom plyr defaults
#' @importFrom grid segmentsGrob gpar textGrob grobTree
#' @importFrom dplyr %>% group_by arrange desc filter row_number
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
                                                     line_height = line_height,
                                                     fontsize = fontsize,
                                                     angle = angle){
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
                                    dplyr::arrange(dplyr::desc(x)) %>%
                                    dplyr::filter(row_number() <= n_max)
                                  alignment = c("right", "bottom")
                                  angle_slope = -1
                                 } else {
                                 coords <- coords %>%
                                   dplyr::group_by(y) %>%
                                   dplyr::arrange(dplyr::desc(size)) %>%
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
