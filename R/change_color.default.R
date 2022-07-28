#' Does not change the color of the trace plot if default color is used.
#'
#' @param template A list from the observed_data.R function created using trace_presets variable to identify the trace plot getting added.
#' @return Returns a stop message
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
change_color.default <- function(template, color) {
  stop("Unspecified template passed")
}
