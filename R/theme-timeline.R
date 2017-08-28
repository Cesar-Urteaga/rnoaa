#' Timeline theme
#'
#' gglot2's theme for the \code{\link{geom_timeline}} and
#' \code{\link{geom_timeline_label}} geoms.
#'
#' @usage NULL
#' @export
#' @importFrom ggplot2 theme_minimal theme element_line element_blank
theme_timeline <- function(){
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 axis.line.x     = ggplot2::element_line(colour = "black"),
                 axis.ticks.x    = ggplot2::element_line(colour = "black"),
                 axis.text.y     = ggplot2::element_blank(),
                 # remove the vertical grid lines
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank()
                 )
}
