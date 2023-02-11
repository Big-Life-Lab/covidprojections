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
normalization_fig1 <- function(grouped_df_new= data,
                               arrow_x_end  = c(as.Date("2020-07-14"), as.Date("2020-10-12"), as.Date("2021-01-10"), as.Date("2021-04-10"),
                                                as.Date("2021-07-09"), as.Date("2021-10-07"), as.Date("2022-01-05"), as.Date("2022-04-05"), as.Date("2022-07-04"),as.Date("2022-09-22")),
                               text_display = c("2020-07-14", "2020-10-12", "2021-01-10", "2021-04-10", "2021-07-09", "2021-10-07", "2022-01-05", "2022-04-05", "2022-07-04", "2022-09-22"),
                               figwidth = 1500, figheight = 8500, margin_subplot = 0.005
){

  t1 <- list(
    family = "Times New Roman",
    size = 14,
    color = "black"
  )

  site_names <- unique(grouped_df_new$sys_siteDesc)
  site_names <- sort(site_names)
  #print("SITE NAMES")
  #print(site_names)
  mylist <- lapply(site_names, function(x) {
    #print("X")
    #print(x)
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
               title = "",
               xaxis = list(title = "Date", titlefont = list(size =30)),
               yaxis = list(title = paste0("<b>",ylabel,"</b>"), range = list(0,1.5), titlefont = list(size =18)), showlegend = TRUE, legend = list(font = list(size = 30))) %>%
        add_annotations(x = ~arrow_x_end,
                        y = ~1,
                        text = ~text_display,
                        showarrow = TRUE,
                        font = list(size = 18))
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
               xaxis = list(title = "Date", titlefont = list(size =30)),
               yaxis = list(title = paste0("<b>",ylabel,"</b>"), range = list(0,1.5), titlefont = list(size =18)), showlegend = FALSE) %>%
        add_annotations(x = ~arrow_x_end,
                        y = ~1,
                        text = ~text_display,
                        showarrow = TRUE,
                        font = list(size = 18))
    }
  })

  n_row <- length(site_names)
  CombinePlot <- subplot(mylist,
                         nrows=n_row, shareX = TRUE, shareY = FALSE, titleY = TRUE, margin = margin_subplot) %>% layout(width = figwidth, height = figheight, showlegend = TRUE)



  output_list <- list("Plot" = CombinePlot, "Data" = grouped_df_new)

  return(output_list)
}
