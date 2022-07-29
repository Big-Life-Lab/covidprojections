#' Accepts incoming projection with observed data and extracts 7th and 14th day of median projections along with confidence intervals.
#'
#' @param data A data frame specif.
#' @param days A vetor specifying which days to extract values for.
#' @param projection_variable A character string specifies which projection variabe to extract. Example: "median" or "mean".
#' @param ci A numeric value specifying the confidence interval.
#' @param obs_column A character string specifying the name of column for observed variable.
#' @return Returns a data frame containing the EpiNow projections and observed values for only the specified days in the days argument.
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @import EpiNow2
#' @import purrr
#' @import stringr
#' @export
create_historic_forecast_dataset <- function(data,
                                             obs_column,
                                             days = c(7, 14),
                                             projection_variable = "median",
                                             ci = 90){

  for (x in 1:length(data)){
    data[[x]] <- data[[x]] %>%
      select(date, projection_variable, obs_column, sd, paste0("lower_", ci),
             paste0("upper_", ci))
    daily_proj <- list()
    i <- 0
    for(day in unique(days)){
      i <- i + 1
      daily_proj[[i]] <- data[[x]][day,] %>%
        rename_with(~paste0(.,paste0("_day_", day)), .cols = -c("date", obs_column))
    }
    data[[x]] <- bind_rows(daily_proj)
  }
  forecast_dataset <- bind_rows(data) %>%
    group_by(date) %>%
    summarise_all(list(~ .[!is.na(.)][1])) %>%
    arrange(date) %>%
    relocate(date, obs_column)
  return(as.data.frame(forecast_dataset))
}
