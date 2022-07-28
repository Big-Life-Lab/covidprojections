#' Accepts incoming data, cleans it by selecting the variables, filtering the site ID, and formatting the date.
#'
#' @param data A data frame.
#' @param sites A character string specifying the site for waste water or Covid case data.
#' @param column A vector that specifies which columns to select
#' @return A data frame that is cleaned
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
#' @examples
#' data_prep(data = read.csv("C:/Users/saran/OneDrive/Documents/WasteWaterCollabAnalysis/wastewater-collaborative-analysis/Data/Observed_data/Ottawa_WWTP_Extended_Aggregated_Data_MR.csv"), sites = "Ottawa WWTP", column = c("sampleDate", "siteID", "nmN1N2_7dma"))
#' data_prep(data = read.csv("C:/Users/saran/OneDrive/Documents/WasteWaterCollabAnalysis/wastewater-collaborative-analysis/Data/Observed_data/Windsor_WWTP_Extended_Aggregated_Data_MR.csv"), sites = "LA", column = c("sampleDate", "siteID", "nmN1N2_7dma"))

data_prep <- function(data,
                      sites, column = c("sampleDate", "siteID", "nmN1N2_7dma")){
  data_clean <- data %>%
    dplyr::select(column) %>%
    filter(siteID %in% sites) %>%
    mutate(sampleDate = as.Date(sampleDate)) %>%
    rename(date = "sampleDate")
  return(data_clean)
}
