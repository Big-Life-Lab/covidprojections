#' Accepts incoming data, cleans it, run two week rolling forecast using EpiNow where the initial start date is incremented in each interval to cover approximately 20, 12, 9, 6, 3, and 1 month of training data in each interval.
#' @param dataset A data frame.
#' @param site A character string specifying the site for waste water or Covid case data OR the column in the dataset in case of absenteeism data.
#' @param startdate A date value specifying the initial start date for training data for validation using EpiNow.
#' @param enddate A date value specifying the end date for training data for rolling predictions using EpiNow.
#' @param y_col A character string specifying the variable to perform projections on from the wastewater data.
#' @param date_intervals A vector that specifies the numeric values to add to the start date of the data in each interval in the for loop.
#' @param projecting A character string specifies whether we are projecting waste water or cases. Accepted values: "ww" or "cases"
#' @param input_multiplier A numeric value which gets multiplied to the input variable that is being projected.
#' @param column A vector that provides the list of variables to be selected from waste water data. Date column must be named "sampleDate" in the input dataset to work with this function.
#' @return Saves 6 different forecast data frame into Data/Projections directory. Data directory is in root folder. The data saved contains entire forecast, projections filtered by growth rate and reported_cases, and filtered by 7th and 14th day of projections for growth rate and reported cases in separate data frames.
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @import EpiNow2
#' @import purrr
#' @import stringr
#' @export
intervals_projections <- function(dataset = "https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv",
                                     site = "Ottawa WWTP",
                                     y_col = "N1_N2_avg_clean", startdate = as.Date("2020-07-01"),
                                     enddate = as.Date("2022-02-01"),
                                     column = c("sampleDate", "siteID", "nmN1N2_7dma"),
                                     date_intervals = c(0, 137, 229, 321, 419, 470),
                                     input_multiplier = 1000000,
                                     projecting = "ww"){
  if (projecting == "ww"){
    # load data
    ww_data <-
      read.csv(dataset)

    if(site == "Ottawa WWTP"){
      ww_clean <- wastewater_prep(ww_data) %>%
        select(date, N1_N2_avg_clean) %>%
        mutate(date = as.Date(date))
    }
    else{

      ww_clean <- data_prep(data = ww_data,
                            sites = site, column = column)
    }
  }else if (projecting == "cases"){
    if (site == "Ottawa WWTP"){
      covid_data <- read.csv(file.path(getwd(), dataset))
    }else if(site == "Covid Employee Data"){
      covid_data <- read_excel(dataset, sheet = "Data")
      covid_data <- covid_data %>%
        mutate(date = as.Date(sampleDate)) %>%
        select(date, covidPosAbs, wSIAbsence) %>%
        rename(observed_new_cases = covidPosAbs) %>%
        arrange(date) #arrange date column
    }else if(site == "Covid Staff Absent Data"){
      covid_data <- read_excel(dataset, sheet = "Data")
      covid_data <- covid_data %>%
        mutate(date = as.Date(sampleDate)) %>%
        select(date, covidPosAbs, wSIAbsence) %>%
        rename(observed_new_cases = wSIAbsence) %>%
        arrange(date) #arrange date column
    }else if (site == "cumulative Covid staff"){
      covid_data <- read_excel(dataset, sheet = "Data")
      covid_data <- covid_data %>%
        mutate(date = as.Date(sampleDate)) %>%
        select(date, covidPosAbs, wSIAbsence, cummCovidPosAbs) %>%
        rename(observed_new_cases = cummCovidPosAbs) %>%
        arrange(date) #arrange date column
    }else{
      covid_data <- read.csv(dataset)
      covid_data <- covid_data %>%
        filter(siteID == site) %>%
        mutate(date = lubridate::dmy(date)) %>%
        mutate(date = as.Date(date)) %>%
        select(date, num_case_by_report_date, hos_covid_adm_by_adm_date) %>%
        rename(observed_new_cases = num_case_by_report_date,
               observed_census_ICU_p_acute_care = hos_covid_adm_by_adm_date) %>%
        arrange(date) #arrange date column
    }
  }
  # Set reporting delay, generation time, incubation period for simulation
  if (projecting == "cases"){
    reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
  }else if (projecting == "ww"){
    reporting_delay <- NULL
  }
  generation_time <-
    get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
  incubation_period <-
    get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

  date_intervals <- date_intervals

  ##Date intervals Windsor date_intervals <- c(0, 157, 249, 341, 433, 492)
  ## Date intervals for Peel: c(0, 113, 205, 270, 389, 448)
  ## Date intervals for Toronto AB: c(0, 86, 178, 270, 362, 421)
  ## Date intervals for Toronto HC, AM: c(0, 82, 174, 264, 359, 418)
  ## Date Interval for LA, LPCC: c(0, 137, 229, 321, 419, 470)
  ## Date Interval for TB: c(0, 95, 187, 279, 371, 428)
  ## Date Interval for com2a: c(0,140,231,323,415,472)
  ## Date Interval for com4: c(0,147,239,331,423,480)
  ## Date Interval for GALT: c(0, 110, 202, 294, 386, 443)
  ## Date Interval for Humber AMF: c(0, 113, 205, 297, 389, 446)
  ## Date Interval for KITCHENER: c(0,110,202,294,386,443)
  ## Date Interval for Leslie: c(0,88,180,272,364,421)
  ## Date Interval for Waterloo: c(0,110,202,294,386,443)
  ## Date Interval for HR region: c(0, 162, 254, 346, 438, 495)

  if (projecting == "ww"){
    input_data <- ww_clean
  }else if (projecting == "cases"){
    input_data <- covid_data
  }

  if (projecting == "cases"){
    y_col = "observed_new_cases"
  }else if (projecting == "ww"){
    y_col = y_col
  }

  projections_data <- list()
  x <- 0

  for(date in date_intervals){
    x <- x + 1
    data_forecast <- short_term_forecast(
      data = input_data,
      input = y_col,
      start_date = as.Date(startdate) + date,
      end_date = as.Date(enddate),
      input_multiplier = input_multiplier,
      omit_last_date = TRUE,
      generation_time = generation_time,
      incubation_period = incubation_period,
      reporting_delay = reporting_delay,
      output = "both"
    )
    projections_data[[x]] <- data_forecast
  }

  return(projections_data)


}
