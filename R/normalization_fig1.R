#' Accepts incoming data and creates plotly plots to show the missingness of data in the variables for mN1, mN2, and mE genes viral signal in the Ontario data for different sites.
#'
#' @param data A vector with path of wastewater datasets to be used from different labs.
#' @param arrow_x_end A vector of date values to specify where to add an arrow to show the date breaks on the plot.
#' @param text_display A vector the same dates as in arrow_x_end vector in form of character values to add as text to the plot.
#' @param figwidth A numeric value specifying the width of the plot.
#' @param figheight A numeric value specifying the height of the plot.
#' @param margin_subplot A numeric value specifying the distance between each subplot for different sites.
#' @return A list containing plotly plot showing missingness of data in the variables for the variables mN1, mN2, and mE viral signal in wastewater for different sites in subplots and the combined data with all labs.
#' @importFrom dplyr "%>%"
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import zoo
#' @import ggplot2
#' @import plotly
#' @import EpiNow2
#' @import purrr
#' @import stringr
#' @export
normalization_fig1 <- function(data= c("../../Data/Observed_data/Waterloo_WWTP_Extended_Aggregated_Data_MR.csv",
                                       "../../Data/Observed_data/Ottawa_WWTP_Extended_Aggregated_Data_rev_MR.csv",
                                       "../../Data/Observed_data/Ryerson_WWTP_Extended_Aggregated_Data_VP.csv",
                                       "../../Data/Observed_data/Toronto_WWTP_Extended_Aggregated_Data_MR.csv",
                                       "../../Data/Observed_data/Windsor_WWTP_Extended_Aggregated_Data_MR.csv"),
                               arrow_x_end  = c(as.Date("2020-07-14"), as.Date("2020-10-12"), as.Date("2021-01-10"), as.Date("2021-04-10"),
                                                as.Date("2021-07-09"), as.Date("2021-10-07"), as.Date("2022-01-05"), as.Date("2022-04-05"), as.Date("2022-07-04"),as.Date("2022-09-22")),
                               text_display = c("2020-07-14", "2020-10-12", "2021-01-10", "2021-04-10", "2021-07-09", "2021-10-07", "2022-01-05", "2022-04-05", "2022-07-04", "2022-09-22"),
                               figwidth = 1500, figheight = 8500, margin_subplot = 0.005
){
  n = length(data)
  datalist = list()
  # or pre-allocate for slightly more efficiency
  datalist = vector("list", length = n)

  for (i in 1:n) {
    # ... make some data
    dat <- read.csv(data[[i]])
    datalist[[i]] <- dat # add it to your list

  }

  big_data = do.call(rbind, datalist)
  grouped_df <- big_data[with(big_data, order(sys_PHU_region, sys_PHU, sampleDate)),]

  grouped_df_new <- grouped_df[,c(1,78,2:77,79:83)]

  grouped_df_new$present <- 0
  for (i in 1:nrow(grouped_df_new)){
    if (!is.na(grouped_df_new[[i, "mN1"]]) | !is.na(grouped_df_new[[i, "mN2"]]) | !is.na(grouped_df_new[[i, "mE"]])){
      grouped_df_new[[i, "present"]] <- 1
    } else{
      grouped_df_new[[i, "present"]] <- 0.2
    }
  }

  grouped_df_new[['sampleDate']] <- as.Date(grouped_df_new[['sampleDate']])

  grouped_df_new$mN1Present<-factor(ifelse(grouped_df_new$present<1,"Absent","Present"))

  t1 <- list(
    family = "Times New Roman",
    size = 14,
    color = "black"
  )

  site_names <- unique(grouped_df_new$sys_siteDesc)
  mylist <- lapply(site_names, function(x) {
    site <- site_names[[1]]
    if (x == site){
      if (stri_sub(x, -4) %in% c("WWTP", "WPCP", "PCP")){
        len = nchar(x)
        ylabel = substr(x, 1, len-4)
      }else{
        ylabel = x
      }
      Y_Chart <- plot_ly(data= grouped_df_new[grouped_df_new$sys_siteDesc == x,]) %>%
        add_trace(x = ~sampleDate, y = ~present, type = 'bar', color = ~mN1Present, showlegend = TRUE
        ) %>%
        layout(barmode = 'stack',
               title = "Presence of mN1/ mN2/ mE viral signal in wastewater across different sites in Ontario across time",
               xaxis = list(title = "Date"),
               yaxis = list(title = paste0("<b>",ylabel,"</b>"), range = list(0,1.5)), showlegend = TRUE) %>%
        add_annotations(x = ~arrow_x_end,
                        y = ~1,
                        text = ~text_display,
                        showarrow = TRUE,
                        font = list(size = 16))
    }else{
      if (stri_sub(x, -4) %in% c("WWTP", "WPCP", "PCP")){
        len = nchar(x)
        ylabel = substr(x, 1, len-4)
      }else{
        ylabel = x
      }
      Y_Chart <- plot_ly(data= grouped_df_new[grouped_df_new$sys_siteDesc == x,]) %>%
        add_trace(x = ~sampleDate, y = ~present, type = 'bar', color = ~mN1Present, showlegend = FALSE
        ) %>%
        layout(barmode = 'stack',
               xaxis = list(title = "Date"),
               yaxis = list(title = paste0("<b>",ylabel,"</b>"), range = list(0,1.5)), showlegend = FALSE) %>%
        add_annotations(x = ~arrow_x_end,
                        y = ~1,
                        text = ~text_display,
                        showarrow = TRUE,
                        font = list(size = 16))
    }
  })

  n_row <- length(site_names)
  CombinePlot <- subplot(mylist,
                         nrows=n_row, shareX = TRUE, shareY = FALSE, titleY = TRUE, margin = margin_subplot) %>% layout(width = figwidth, height = figheight, showlegend = TRUE)



  output_list <- list("Plot" = CombinePlot, "Data" = grouped_df_new)

  return(output_list)
}
