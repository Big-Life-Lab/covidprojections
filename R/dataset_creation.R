#' Creates a list of R objects to be used for EpiNow plotting using plotly function in observed_data.R script.
#'
#' @param dataset A data frame.
#' @param site A character string specifying the name of waste water site to be used to create plot.
#' @param enddate A date value specifying the value of end date for Ottawa Waste water data.
#' @param y_col A character string specifying the name of column from observed data to plot.
#' @param column A vector that contains the name of columns to filter from the input data and passed as argument to data_prep function
#' @return The list of R objects to be used in the plotting function in observed_data.R.
#' \itemize{
#'   \item cleaned_data - The input dataset that has been cleaned by selecting the right siteID and columns and right formatting of date.
#'   \item cleaned_march_data - Based on site = "Ottawa WWTP", The input dataset has been cleaned by selecting the right siteID and columns and right formatting of date and filtering data from month of March.
#'   \item waste_roc - A data frame that has change and percent change in signal from previous day along with its log values.
#'   \item y_col - A character value specifying the wastewater signal column being used to plot in the dataset.
#'   \item "daily_viral_signal_call" - A list that is used to provide arguments to plotting function in observed_data.R for the wastewater signal column.
#'   \item "daily_roc_call_log_mag" - A list that is used to provide arguments to plotting function in observed_data.R for the column that contains the log of daily change in wastewater signal.
#'   \item "daily_roc_call_log_pct" - A list that is used to provide arguments to plotting function in observed_data.R for the column that contains the log of daily % change in wastewater signal.
#'   \item "daily_roc_call_pct" - A list that is used to provide arguments to plotting function in observed_data.R for the column that contains the daily % change in wastewater signal.
#' }
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @export
#' @examples
#' dataset_creation(dataset = "C:/Users/saran/OneDrive/Documents/WasteWaterCollabAnalysis/wastewater-collaborative-analysis/Data/Observed_data/Toronto_WWTP_Extended_Aggregated_Data_MR.csv",site = "NT", y_col = "nmN1N2_7dma")



dataset_creation <- function(dataset = "C:/Users/saran/OneDrive/Documents/WasteWaterCollabAnalysis/wastewater-collaborative-analysis/Data/Observed_data/Ottawa_WWTP_Extended_Aggregated_Data_MR.csv",
                             site = "Ottawa WWTP", enddate = "2022-02-01", y_col = "nmN1N2_7dma", column = c("sampleDate", "siteID", "nmN1N2_7dma")
){
  signal_col <- "rgb(226, 127, 88)"
  rolling_avg_col <- "rgb(226, 127, 88)"
  incoming_data <- read.csv(dataset)

  print("INCOMING DATA")
  print(incoming_data)
  if(site == "Ottawa WWTP"){
    name <- "Daily viral signal in Ottawa"
    cleaned_data <- wastewater_prep(incoming_data) %>%
      select(date, `y_col`) %>%
      mutate(date = as.Date(date))  # Filters out March data from the Ottawa dataset where quality flag was FALSE (Snow melt period)
    march_data <- incoming_data %>% filter((sampleDate >"2021-02-28" & sampleDate <= "2021-03-31") & qualityFlag == FALSE) # fetching only march data with qualityflag = False
    march_data <- wastewater_prep(march_data) %>%
      select(date, `y_col`) # cleaning march data

    cleaned_march_data <- wastewater_prep(incoming_data) %>%
      select(date, `y_col`) %>%
      filter(date >= "2020-07-01" & date <= enddate) %>% # filter data between July 2020 and Feb 2022
      filter(date < "2021-02-28" | date > "2021-03-31") %>% # omit March 2021
      rbind(march_data) %>% #concatenate march data with qualityflag = False
      arrange(date) #arrange date column
  }
  else {
      print("NOT OTTAWA")
      y_col <- y_col
      split_site <- strsplit(site, " ")
      name <- paste("Daily viral signal in ", split_site[[1]][[1]])
      cleaned_data <- data_prep(data = incoming_data,
                                sites = site, column = column)
    }

  print("CLEANED DATA")
  print(cleaned_data)

  daily_viral_signal_call <- list(type = "observed_data", y_column = y_col,
                                  name = `name`, short_name = "Daily signal",
                                  color = rolling_avg_col, yaxis = "y2", opacity = 0.50)

  waste_roc <- cleaned_data %>%
    mutate(daily_roc = cleaned_data[,y_col] - lag(cleaned_data[,y_col]),
           daily_roc_mag = cleaned_data[,y_col]/lag(cleaned_data[,y_col]),
           daily_roc_pct = (cleaned_data[,y_col] - lag(cleaned_data[,y_col]))/lag(cleaned_data[,y_col]) * 100,
           log_daily_roc_mag = log10(daily_roc_mag),
           log_daily_roc_pct = log10(abs(daily_roc_pct/100)))

  pct_5_fold <- nrow(waste_roc[waste_roc$daily_roc_mag >= 5,])/nrow(waste_roc) * 100
  daily_roc_call_log_mag <- list(type = "observed_data", y_column = "log_daily_roc_mag",
                                 name = "Daily rate of change", short_name = "Daily rate of change",
                                 color = signal_col, yaxis = "y2", opacity = 0.5)

  daily_roc_call_log_pct <- list(type = "observed_data", y_column = "log_daily_roc_pct",
                                 name = "Daily rate of change", short_name = "Daily rate of change",
                                 color = signal_col, yaxis = "y2", opacity = 0.5)

  daily_roc_call_pct <- list(type = "observed_data", y_column = "daily_roc_pct",
                             name = "Daily rate of change", short_name = "Daily rate of change",
                             color = signal_col, yaxis = "y2", opacity = 0.5)

  if(exists("cleaned_march_data") && is.data.frame(get('cleaned_march_data'))){
    print("FINAL CLEANED MARCH DAATA")
    print(cleaned_march_data)
    plotting_data_list <- list("cleaned_data" = cleaned_data, "cleaned_march_data"= cleaned_march_data,
                               "waste_roc" = waste_roc,
                               "y_col" = y_col, "daily_viral_signal_call" = daily_viral_signal_call,
                               "pct_5_fold" = pct_5_fold, "daily_roc_call_log_mag" = daily_roc_call_log_mag,
                               "daily_roc_call_log_pct" = daily_roc_call_log_pct, "daily_roc_call_pct" = daily_roc_call_pct)
  }else{
    print("FINAL CLEANED DATA")
    print(cleaned_data)
    plotting_data_list <- list("cleaned_data" = cleaned_data, "waste_roc" = waste_roc,
                               "y_col" = y_col, "daily_viral_signal_call" = daily_viral_signal_call,
                               "pct_5_fold" = pct_5_fold, "daily_roc_call_log_mag" = daily_roc_call_log_mag,
                               "daily_roc_call_log_pct" = daily_roc_call_log_pct, "daily_roc_call_pct" = daily_roc_call_pct)
  }


  return(plotting_data_list)
}

