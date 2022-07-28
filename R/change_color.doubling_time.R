#' Accepts a list containing a template for a trace of a line plot for doubling time and set the color of the plot.
#'
#' @param template A list from the observed_data.R function created using trace_presets variable to identify the trace plot getting added.
#' @param color A hex value to set as the color.
#' @return Sets color of the trace plot template added as argument to the function
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
change_color.doubling_time <- function(template, color) {
  return(list(
    mode = "line",
    type = "scatter",
    showlegend = FALSE,
    line = list(
      dash = "dash",
      color = color,
      width = 5
    )
  ))
}
