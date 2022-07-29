#' Accepts incoming data, cleans it and run EpiNow projections on it.
#'
#' @param data A data frame.
#' @param input A data frame column to run projections on.
#' @param input_multiplier An initial number to multiply the input column with before running projections.
#' @param start_date A start date for training data for EpiNow projections
#' @param end_date And end date for training data for EpiNow projections.
#' @param omit_last_date A boolean value specifying Whether to omit last date when running projections.
#' @param generation_time Time it takes on average for 1 infected person to cause secondary infections.
#' @param incubation_period Incubation period of the virus.
#' @param reporting_delay The delay in reporting an infection.
#' @param horizon The number of days to forecast into future.
#' @param rw Numeric step size of random walk (7 for weekly)
#' @param gp A list that defines the Gaussian process.
#' @param output Type of output to produce. There are 3 types: projection data frame, list of summary estimates, or both. Accepted values: "projections", "estimates", "both".
#' @param CrI A vector specifying which confidence interval to include in projections.
#' @return The forecast data frame or list of summary estimates or Both
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @import EpiNow2
#' @import purrr
#' @import stringr
#' @export
short_term_forecast <- function(data,
                                input,
                                start_date,
                                end_date,
                                generation_time,
                                incubation_period,
                                reporting_delay,
                                omit_last_date = FALSE,
                                input_multiplier = 1,
                                horizon = 14,
                                rw = 7,
                                gp = NULL,
                                output = "projections",
                                CrI = c(0.2, 0.5, 0.75, 0.9)

){

  # Format dataset
  if(missing(end_date)) {
    end_date <- max(as.Date(data$date), na.rm = TRUE)
  }

  data_formatted <- data %>%
    filter(as.Date(date) >= as.Date(start_date)) %>%
    filter(as.Date(date) <= as.Date(end_date)) %>%
    select(date, as.character(input)) %>%
    rename(confirm = as.character(input)) %>%
    mutate(date = as.Date(date),
           confirm = as.integer(confirm * input_multiplier))

  if(isTRUE(omit_last_date)){
    data_formatted <- data_formatted[data_formatted$date < as.Date(end_date),]
  }

  # Run epinow2 sim
  if(is.null(reporting_delay)){
    projections <-
      EpiNow2::epinow(reported_cases = data_formatted,
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period),
                      rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = rw),
                      stan = stan_opts(cores = 4),
                      gp = gp, horizon = horizon,
                      CrIs = CrI)
  }
  else{
    projections <-
      EpiNow2::epinow(reported_cases = data_formatted,
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = rw),
                      stan = stan_opts(cores = 4),
                      gp = gp, horizon = horizon, CrIs = CrI)
  }
  if(output == as.character("projections")){
    forecast <-
      # Obtain summarized projections
      projections[[1]][[2]]
  }
  else if(output == as.character("estimates")){
    growth_measures <- cbind(projections[[3]][[1]], projections[[3]][[2]])
    colnames(growth_measures) <- c("measure", "estimate")
    growth_measures[2, "measure"] <- "Expected change in viral amount"
    growth_measures <- growth_measures[-1,]
    forecast <- list()
    forecast[["Growth summary"]] <- growth_measures
    forecast[["Growth estimates"]] <- projections[[3]][[3]]
  }
  else if(output == as.character("both")){
    growth_measures <- cbind(projections[[3]][[1]], projections[[3]][[2]])
    colnames(growth_measures) <- c("measure", "estimate")
    growth_measures[2, "measure"] <- "Expected change in viral amount"
    growth_measures <- growth_measures[-1,]
    forecast <- list()
    forecast[["Forecast"]] <- projections[[1]][[2]]
    forecast[["Growth summary"]] <- growth_measures
    forecast[["Growth estimates"]] <- projections[[3]][[3]]
  }

  return(forecast)
}
