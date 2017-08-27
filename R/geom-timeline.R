#' Timeline
#'
#' The \code{geom_timeline} displays the timeline of the observations along
#' with each point observed during the timeline's period.
#'
#' @inheritParams ggplot2::layer
#' @export
#' @importFrom ggplot2 layer
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          inherit.aes = TRUE, na.rm = FALSE,
                          ...){
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
                 }

#' Class of the geom_timeline
#'
#' Please refer to the documentation of the \code{\link{geom_timeline}}.
#'
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomPoint aes
#' @importFrom plyr defaults
#' @importFrom grid polylineGrob gpar grobTree
GeomTimeline <- ggplot2::ggproto(`_class` = "GeomTimeline",
                          # Because this geom is quite similar to the GeomPoint,
                          # we can recycle it instead of reinventing the wheel.
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
                            # Includes the timeline along the above data points.
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
