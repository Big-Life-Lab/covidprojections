#' Accepts a list containing a template for a trace of a scatter plot for the signal data plot and set the color of the plot.
#'
#' @param template A list from the observed_data.R function created using trace_presets variable to identify the trace plot getting added.
#' @param color A hex value to set as the color.
#' @return Sets color of the trace plot template added as argument to the function
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
change_color.signal_data <- function(template, color) {
  return(list(
    type = "scatter",
    showlegend = TRUE,
    marker = list(color = color)
  ))
}
