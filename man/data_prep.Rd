% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{data_prep}
\alias{data_prep}
\title{Accepts incoming data, cleans it by selecting the variables, filtering the site ID, and formatting the date.}
\usage{
data_prep(data, sites, column = c("sampleDate", "siteID", "nmN1N2_7dma"))
}
\arguments{
\item{data}{A data frame.}

\item{sites}{A character string specifying the site for waste water or Covid case data.}

\item{column}{A vector that specifies which columns to select}
}
\value{
A data frame that is cleaned
}
\description{
Accepts incoming data, cleans it by selecting the variables, filtering the site ID, and formatting the date.
}
\examples{
data_prep(data = read.csv("C:/Users/saran/OneDrive/Documents/WasteWaterCollabAnalysis/wastewater-collaborative-analysis/Data/Observed_data/Ottawa_WWTP_Extended_Aggregated_Data_MR.csv"), sites = "Ottawa WWTP", column = c("sampleDate", "siteID", "nmN1N2_7dma"))
data_prep(data = read.csv("C:/Users/saran/OneDrive/Documents/WasteWaterCollabAnalysis/wastewater-collaborative-analysis/Data/Observed_data/Windsor_WWTP_Extended_Aggregated_Data_MR.csv"), sites = "LA", column = c("sampleDate", "siteID", "nmN1N2_7dma"))
}
