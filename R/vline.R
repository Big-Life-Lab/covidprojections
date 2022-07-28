#' Accepts an x asxis value and a string representing color value to return a list that can be used to add as a vertical line trace to the plotly plot.
#'
#' @param x A value on the xaxis where the vertical line should start.
#' @param color A string value representing the color of the vertical line to be added
#' @return A list that is used by plotly to add as trace to a plotly figure to add a vertical line.
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
vline <- function(x = 0, color = "blue"){
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(dash = 'dot', width = 1.5, color = color)
  )
}
