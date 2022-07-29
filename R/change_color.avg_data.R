#' Accepts a list containing a template for a trace of a line plot for the smoothed average plot and set the color of the plot.
#'
#' @param template A list from the observed_data.R function created using trace_presets variable to identify the trace plot getting added.
#' @param color A hex value to set as the color.
#' @param width A numeric value indicating the width of line plot.
#' @return Sets color of the trace plot template added as argument to the function
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
change_color.avg_data <- function(template, color, width){
  return(list(
    type = "scatter",
    mode = "line",
    showlegend = TRUE,
    line = list(
      color = color,
      width = width
    )
  ))
}
