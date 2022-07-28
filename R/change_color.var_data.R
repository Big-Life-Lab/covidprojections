#' Accepts a list containing a template for a trace of a scatter/line plot for the trace plot being added and set the color of the plot.
#'
#' @param template A list from the observed_data.R function created using trace_presets variable to identify the trace plot getting added.
#' @param color A hex value to set as the color.
#' @param width A numeric value that indicates the width of the plot.
#' @return Sets color of the trace plot template added as argument to the function
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
change_color.var_data <- function(template, color, width) {
  return(list(
    type = "scatter",
    mode = "lines+marker",
    showlegend = TRUE,
    line = list(
      color = color,
      width = width
    ),
    marker = list(color = color)
  ))
}
