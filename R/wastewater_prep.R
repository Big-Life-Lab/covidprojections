#' Accepts incoming wastewater data and cleans it.
#'
#' @param data A data frame.
#' @return The final cleaned data frame which contains renamed variables, formatted date, new variables for rolling averages for 5, 7, and 10 days and daily rate of change in signal.
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @export
#' @examples
#' wastewater_prep(read.csv("https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv"))
wastewater_prep <- function(data){
  data_clean <- data %>%
    rename(date = "sampleDate",
           N1 = "covN1_nPMMoV_meanNr",
           N2 = "covN2_nPMMoV_meanNr") %>%
    select(date, N1, N2, qualityFlag, reportDate) %>%
    mutate(date = as.Date(date),
           reportDate = as.Date(reportDate)) %>%
    filter(!is.na(date)) %>%
    # Create mean value of N1 and N2
    mutate(N1_N2_avg = (N1 + N2)/2) %>%
    # create daily rate of change of viral signal
    mutate(
      viral_roc_daily = ((N1_N2_avg - lag(N1_N2_avg))/lag(N1_N2_avg))/
        (as.numeric(as_date(date)-lag(as_date(date))))
    )

  data_clean$N1_N2_avg_clean <-
    ifelse(data_clean$qualityFlag == TRUE, NA, data_clean$N1_N2_avg)
  data_clean$N1_N2_avg_omit <-
    ifelse(data_clean$qualityFlag == TRUE, data_clean$N1_N2_avg, NA)
  data_final <- data_clean %>%
    # create 5 day rolling avg of viral signal
    mutate(
      N1_N2_5_day =
        rollapply(N1_N2_avg_clean, width=5,
                  FUN=function(x) mean(x, na.rm = TRUE),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="right")
    ) %>%
    # create 7 day rolling avg of viral signal
    mutate(
      N1_N2_7_day =
        rollapply(N1_N2_avg_clean, width=7,
                  FUN=function(x) mean(x, na.rm = TRUE),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="right")
    ) %>%
    # create 10 day rolling avg of viral signal
    mutate(
      N1_N2_10_day =
        rollapply(N1_N2_avg_clean, width=10,
                  FUN=function(x) mean(x, na.rm = TRUE),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="right")
    ) %>%
    # create 10 day rolling avg of daily rate of change of viral signal
    mutate(
      avg_viral_roc_10_day =
        rollapply(viral_roc_daily, width=10,
                  FUN=function(x) mean(x),
                  by=1, by.column=TRUE, partial=FALSE,
                  fill=NA, align="right")
    ) %>%
    # create change in 5 day rolling avg from 5 day rolling avg 5 days ago
    mutate(
      change_N1_N2_5_day =
        (N1_N2_5_day - lag(N1_N2_5_day, 5))/lag(N1_N2_5_day, 5) * 100
    )
    return(data_final)
}
