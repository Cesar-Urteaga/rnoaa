#' Timeline theme
#'
#' gglot2's theme for the \code{\link{geom_timeline}} and
#' \code{\link{geom_timeline_label}} geoms.
#'
#' @usage NULL
#' @export
#' @importFrom ggplot2 theme_minimal theme element_line element_blank
#' @examples
#' require(dplyr)
#' require(ggplot2)
#'
#' # Before using the geom, we need to tidy the data up.
#' raw_data <- get_earthquake_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- eq_location_clean(clean_data)
#'
#' # Quakes in Indonesia
#' timeline_plot <- clean_data %>%
#'   filter(COUNTRY == "INDONESIA",
#'          !is.na(EQ_PRIMARY),
#'          YEAR %in% 2000:2016) %>%
#'   ggplot(mapping = aes(x = DATE,
#'                        size = EQ_PRIMARY,
#'                        color = TOTAL_DEATHS / 1000,
#'                        label = LOCATION_NAME)
#'          ) +
#'   geom_timeline() +
#'   geom_timeline_label(line_height = 1 / 4) +
#'   labs(size = "Richter scale value",
#'        color = "# deaths in thousands",
#'        y = "")
#' # Sans the new theme.
#'   timeline_plot
#' # With the timeline's theme.
#'   timeline_plot + theme_timeline()
theme_timeline <- function(){
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 axis.line.x     = ggplot2::element_line(colour = "black"),
                 axis.ticks.x    = ggplot2::element_line(colour = "black"),
                 # remove the vertical grid lines
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank()
                 )
}
