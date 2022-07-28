#' Accepts a list containing a template for a trace of a plot and set the color of the plot using the method change_color.
#'
#' @param template A list from the observed_data.R function created using trace_presets variable to identify the trace plot getting added.
#' @return Sets color of the trace plot template added as argument to the function
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
change_color <- function(template, ...) {
  UseMethod("change_color", template)
}











