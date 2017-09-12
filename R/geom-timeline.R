#' Timeline ggplot2's geom
#'
#' The \code{geom_timeline} displays the timeline of the observations along
#' with each point observed during the given period.
#'
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to the layer function. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @export
#' @importFrom ggplot2 layer
#' @seealso \code{\link{geom_timeline_label}} to include labels for each
#' observation.
#' @examples
#' require(dplyr)
#' require(ggplot2)
#'
#' # Before using the geom, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#'
#' # Quakes in USA
#' clean_data %>%
#'   filter(COUNTRY == "USA",
#'          !is.na(EQ_PRIMARY),
#'          YEAR %in% 2000:2016) %>%
#'   ggplot(mapping = aes(x = DATE,
#'                        size = EQ_PRIMARY,
#'                        color = TOTAL_DEATHS)
#'          ) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value",
#'        color = "# deaths",
#'        y = "") +
#'   theme_timeline()
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          inherit.aes = TRUE, na.rm = FALSE,
                          ...){
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
                 }

# Class of the geom_timeline
#
# Please refer to the documentation of the \code{\link{geom_timeline}}.
#
# @format NULL
# @usage NULL
# @importFrom ggplot2 ggproto GeomPoint aes
# @importFrom plyr defaults
# @importFrom grid polylineGrob gpar grobTree
GeomTimeline <- ggplot2::ggproto(`_class` = "GeomTimeline",
                          # Because this geom is quite similar to the GeomPoint
                          # class, we can recycle it instead of reinventing the
                          # wheel.
                          `_inherit`      = ggplot2::GeomPoint,
                          required_aes    = "x",
                          default_aes     = plyr::defaults(
                                              ggplot2::aes(y     = 0.5,
                                                           size  = 2,
                                                           alpha = 0.7),
                                              ggplot2::GeomPoint$default_aes
                                              ),
                          draw_panel      = function(data, panel_params,
                                                     coord){
                            # Adds the data points.
                            dates_grob <- ggplot2::GeomPoint$draw_panel(data,
                              panel_params, coord)
                            coords <- coord$transform(data, panel_params)
                            # Includes the timeline along the data points.
                            timeline_grob <- grid::polylineGrob(
                              coords$x, coords$y,
                              id = coords$group,
                              gp = grid::gpar(col = gray(0.5))
                              )
                            ggname("geom_timeline",
                                   grid::grobTree(dates_grob, timeline_grob)
                                   )
                          }
                          )
