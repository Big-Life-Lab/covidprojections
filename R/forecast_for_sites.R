#' Accepts incoming data, cleans it based on the site provided and calls short_term_forecast function to run EpiNow projections.
#'
#' @param dataset A data frame.
#' @param site A character string specifying the site for waste water or Covid case data OR the column in the dataset in case of absenteeism data.
#' @param startdate A date value specifying the start date for training data for EpiNow projections
#' @param enddate A date value specifying the end date for training data for EpiNow projections.
#' @param horizon A numeric value specifying the number of days to forecast into future.
#' @param projecting A character string specifies whether we are projecting wastewater or cases. Accepted values: "ww" or "cases"
#' @param column A vector that provides the list of variables to be selected from waste water data.
#' @param project_hospital A boolean value that determines whether projections are being made on hospital data or Covid case data.
#' @param input_multiplier A numeric value which gets multiplied to the input variable that is being projected.
#' @param y_col A character string specifying the variable to perform projections on from the wastewater data.
#' @return Saves the forecast data frame or list of summary estimates or Both into Data/Projections directory. Data directory is in root folder.
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @import EpiNow2
#' @import purrr
#' @import stringr
#' @export
forecast_for_sites <- function(dataset = "Data/Observed_data/OPH_Observed_COVID_Data.csv",
                                         site = "Ottawa WWTP", startdate = as.Date("2021-02-01"), horizon = 14,
                                         enddate = as.Date("2022-07-16"),
                                         projecting = "cases",
                                         column = c("sampleDate", "siteID", "nmN1N2_7dma"),
                                         input_multiplier = 1,
                                         project_hospital = FALSE,
                                         y_col = "N1_N2_avg_clean"){
  # Load and Clean Data
  # Clean Data based on the site and based on whether we are projecting for cases or waste water or absenteeism data using If else loop
  # the argument site below has been used to denote both the waste water site and also the column in Absenteeism we are projecting
  if (projecting == "ww"){
    # load data
    ww_data <-
      read.csv(dataset)

    if(site == "Ottawa WWTP"){
      ww_clean <- wastewater_prep(ww_data) %>%
        select(date, N1_N2_avg_clean) %>%
        mutate(date = as.Date(date))
    }else{ww_clean <- data_prep(data = ww_data, sites = site, column = column)
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

  if (project_hospital){
    input_data = "observed_census_ICU_p_acute_care"
  }else{
    if (projecting == "cases"){
    input_data = "observed_new_cases"
  }else if (projecting == "ww"){
    input_data = y_col
  }
}

  #Run Epinow forecast by calling short_term_forecast function from the epinow_functions.R file
  # Projects the cases using historic data from startdate to end date.
  cases_forecast <- short_term_forecast(
    data = covid_data,
    input = input_data,
    start_date = startdate,
    end_date = enddate,
    input_multiplier = input_multiplier,
    omit_last_date = TRUE,
    generation_time = generation_time,
    incubation_period = incubation_period,
    reporting_delay = reporting_delay,
    output = "both",
    horizon = horizon
  )
  ## Save the projections in the following RData file.
  return(cases_forecast)
}
