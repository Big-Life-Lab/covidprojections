#' Accepts incoming wastewater data and normalizes the value of the wastewater signal.
#'
#' @param dataset A data frame.
#' @param site A character string specifying the site ID to filter
#' @param y_column A character string specifying the name of the wastewater signal column to normalize.
#' @param start_date A date value specifying the start date in the dataset to filter.
#' @param end_date A date value specifying the end date in the dataset to filter.
#' @return The final normalized dataframe where the wastewater signal column is multiplied with 1000000.
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @export
normalized_ww_data <- function(dataset, site, y_column, start_date, end_date){
  ww_clean_1 <- dataset %>% filter(date>=start_date & date <= end_date) %>% filter(siteID == site)
  ww_normalized <- ww_clean_1
  ww_normalized[,y_column] = ww_normalized[,y_column]*1000000
  return(ww_normalized)
}
