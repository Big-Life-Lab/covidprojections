#' Accepts incoming data, cleans it, run two week rolling forecast using EpiNow where the initial start and end date is incremented by 7 days at every step. 7th and 14th day of forecast values from different rolling periods have been extracted for analysis.
#'
#' @param dataset A data frame.
#' @param site A character string specifying the site for waste water or Covid case data OR the column in the dataset in case of absenteeism data.
#' @param startdate A date value specifying the initial start date for training data for validation using EpiNow.
#' @param enddate A date value specifying the initial end date for training data for validation using EpiNow.
#' @param y_col A character string specifying the variable to perform projections on from the wastewater data.
#' @param project_until A date value specifying the maximum value for final end date for the rolling projections.
#' @param projecting A character string specifies whether we are projecting waste water or cases. Accepted values: "ww" or "cases"
#' @param input_multiplier A numeric value which gets multiplied to the input variable that is being projected.
#' @param column A vector that provides the list of variables to be selected from waste water data
#' @return Saves 6 different forecast data frame into Data/Projections directory. Data directory is in root folder. The data saved contains entire forecast, projections filtered by growth rate and reported_cases, and filtered by 7th and 14th day of projections for growth rate and reported cases in separate data frames.
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @import EpiNow2
#' @import purrr
#' @import stringr
#' @export
projections_validation <- function(dataset = "https://raw.githubusercontent.com/Big-Life-Lab/PHESD/main/Wastewater/Ottawa/Data/wastewater_virus.csv",
                                   site = "Ottawa WWTP",
                                   startdate = as.Date("2020-07-01"),
                                   enddate = as.Date("2020-11-01"),
                                   y_col = "N1_N2_avg_clean",
                                   project_until = as.Date("2022-07-10"),
                                   projecting = "cases",
                                   input_multiplier = 1,
                                   column = c("sampleDate", "siteID", "nmN1N2_7dma")){

  ## Code below will load and Clean Data based on the site provided.
  ## Each site and whether we are projecting wastewater or cases has a specific loading and cleaning method.
  # the argument site below has been used to denote both the waste water site and also the column in Absenteeism we are projecting
  if (projecting == "ww"){
    # load data
    ww_data <-
      read.csv(dataset)

    if(site == "Ottawa WWTP"){
      ww_clean <- wastewater_prep(ww_data) %>%
        select(date, N1_N2_avg_clean) %>%
        mutate(date = as.Date(date))
    }else{
      ww_clean <- data_prep(data = ww_data,
                            sites = site, column = column)
    }
  }else if (projecting == "cases"){
    if (site == "Ottawa WWTP"){
      # load data
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

  #ORIGINAL end_date <- 2022-02-01

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


  start_date <- as.Date(startdate)
  end_date = as.Date(enddate)

  if (projecting == "ww"){
    input_data = y_col
  }else{
    input_data = "observed_new_cases"
  }

  # Create an empty list to save the two week rolling projections
  cases_data_forecast <- list()

  # Iterate through the While loop to generate two week rolling projections from mid Feb to project_until date
  # The training data in each iteration is incremented by one day to generate the next rolling forecast.
  x <- 0
  while(end_date <= project_until){
    x <- x + 1
    cases_forecast <- short_term_forecast(
      data = covid_data,
      input = input_data,
      start_date = start_date,
      end_date = end_date,
      input_multiplier = input_multiplier,
      omit_last_date = TRUE,
      generation_time = generation_time,
      incubation_period = incubation_period,
      reporting_delay = reporting_delay,
      output = "projections"
    )


    cases_data <- covid_data %>%
      mutate(date = as.Date(date)) %>%
      select(date, `input_data`)
    cases_data[,input_data] = cases_data[,input_data]*input_multiplier
    cases_data_forecast[[x]] <- cases_data %>%
      full_join(cases_forecast) %>%
      arrange(date)

    start_date <- start_date + 7
    end_date <- end_date + 7
  }

  # All the rolling forecasts are stored in the list cases_data_forecast which is saved to RData file.


  # List below only saves the two week rolling forecast for reported cases by iterating through for loop
  cases_data_forecast_cases = list()

  for (i in 1:length(cases_data_forecast)){
    cases_data_forecast_cases[[i]] <- cases_data_forecast[[i]] %>% filter(!is.na(variable), variable == "reported_cases",
                                                                          type == 'forecast')
  }

  # List below only saves the two week rolling forecast for growth rate by iterating through for loop
  cases_data_forecast_grate = list()

  for (i in 1:length(cases_data_forecast)){
    cases_data_forecast_grate[[i]] <- cases_data_forecast[[i]] %>% filter(!is.na(variable), variable == "growth_rate",
                                                                          type == 'forecast') %>%

      rename_with(~paste0(.,paste0("_grate")), .cols=-c("date", "observed_new_cases"))
  }


  # cases_data_final stores vertically concatenates the forecasts for reported cases and growth rate in one dataset
  # By vertical concatenation there is no duplication of same date for the two types of forecast.
  cases_data_final = list()

  for (i in 1:length(cases_data_forecast_cases)){

    cases_data_final[[i]] <- cases_data_forecast_cases[[i]] %>% inner_join(cases_data_forecast_grate[[i]], by = c("date", "observed_new_cases"))
  }

  #cases_data_final is saved to RData file (contains reported cases and growth rate forecast values only)


  # Extract the 7th day forecast for reported cases and then save it to RData file.
  cases_data_forecast_7_day_fig5 <-
    create_historic_forecast_dataset(cases_data_final,
                                     days = 7,
                                     projection_variable = "median",
                                     ci = 90,
                                     obs_column = "observed_new_cases")

  # Save 7th day forecast of reported cases to RData file

  # Extract the 7th day forecast for growth rate and then save it to RData file.
  cases_data_forecast_7_day_fig7 <-
    create_historic_forecast_dataset(cases_data_final,
                                     days = 7,
                                     projection_variable = c("median", "median_grate"),
                                     ci = 90,
                                     obs_column = "observed_new_cases")

  # Save 7th day forecast of growth rate to RData file


  # Extract the 14th day forecast for reported cases and then save it to RData file.
  cases_data_forecast_14_day_fig5 <-
    create_historic_forecast_dataset(cases_data_final,
                                     days = 14,
                                     projection_variable = "median",
                                     ci = 90,
                                     obs_column = "observed_new_cases")

  # Save 14th day forecast of reported cases to RData file


  # Extract the 14th day forecast for growth rate and then save it to RData file.
  cases_data_forecast_14_day_fig7 <-
    create_historic_forecast_dataset(cases_data_final,
                                     days = 14,
                                     projection_variable = c("median", "median_grate"),
                                     ci = 90,
                                     obs_column = "observed_new_cases")

  # Save 14th day forecast of growth rate to RData file
  return(list(cases_data_forecast, cases_data_final, cases_data_forecast_7_day_fig5, cases_data_forecast_7_day_fig7, cases_data_forecast_14_day_fig5, cases_data_forecast_14_day_fig7))


}
