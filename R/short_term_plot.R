#' Accepts incoming data and creates plotly plots to show observed as data as bar plots and smoothed estimates as line plot along with future projections.
#'
#' @param interval_num A numeric value denoting how many dates to show on x axis prior to the last date in data.
#' @param projections A data frame that contains EpiNow projections.
#' @param levels A vector that is used to create legend for historic data and projected data.
#' @param obs_data A data frame that consists of observed historic data.
#' @param obs_column A character string that specifies which column to select for plotting from observed data.
#' @param forecast_type A character string specifying what type of EpiNow projections to plot. Acceptable values are reported_cases, R, infection, growth_rate.
#' @param start_date A date value specifying what date to include as start date on x axis of the plot.
#' @param ylab A character string specifying the label for y axis.
#' @param title A character string specifying the title of the plot.
#' @param scale A boolean value specifying whether to scale the plot
#' @param tick_period A character string specifying the intervals between two consecutive dates on x axis
#' @param tick_labels_date A character string specifying the tick labels for dates on x axis.
#' @param annotation_text A character string that provides annotation text to show on the plot for description.
#' @param CrI A numeric value specifying which confidence interval to include.
#' @param x_shift A numeric value to show where to shift the annotation text on the x axis
#' @param y_shift A numeric value to show where to shift the annotation text on the y axis
#' @param CrIp A numeric value used for denoting the lower level of confidence interval desired.
#' @return A plotly plot showing observed values as bar and smoothed estimates as line plot along with projections as line plot.
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
short_term_plot <- function(projections,
                            obs_data,
                            obs_column,
                            forecast_type,
                            ylab,
                            title,
                            scale = FALSE,
                            interval_num=40,
                            start_date = first(as.Date(projections$date)),
                            levels = c("historic", "forecast"),
                            tick_period = "1 week",
                            tick_labels_date = "%b %d",
                            x_shift = 165,
                            y_shift = 0,
                            annotation_text = "<b>*Shaded area represents the 75% credible region</b>",
                            CrI = 75,
                            CrIp = 25

){

  cr_upper <- paste0("upper_", CrI)
  cr_lower <- paste0("lower_", CrI)

  if(!(cr_upper %in% colnames(projections)) &&
     !(cr_lower %in% colnames(projections))){
    stop("The ",paste0(CrI, "% "), "credible interval is not included in the projections.")
  }

  # Filter data based on forecast type and remove 50% CI
  projections <- projections %>%
    filter(variable == as.character(forecast_type)) %>%
    select(c(date, type, median, mean, sd, as.character(cr_upper), as.character(cr_lower)))


  max_y_lim <- max(projections$upper_90, na.rm = TRUE)
  # Omit last day of observed data
  obs_data_omit <- obs_data %>%
    filter(date < as.Date(last(date)))

  # Set types to levels indicated in function call
  projections$type[projections$date <= as.Date(last(obs_data_omit$date))] <-
    as.character(levels[[1]])

  projections$type[projections$date > as.Date(last(obs_data_omit$date))] <-
    as.character(levels[[2]])

  projections$type <- factor(projections$type, levels =
                               c(as.character(levels[[1]]), as.character(levels[[2]])))
  for (col in colnames(projections)){
    if (paste0("lower_",CrI) %in% col){
      val <- 100 - CrI
      col_name <- paste0("lower_", val)
      projections[,`col_name`] <- projections %>% select(`col`)

    }
  }
  # set up CrI index
  CrIs <- CrIp

  index <- 1
  alpha_per_CrI <- 0.6 / (length(CrIs) - 1)



  # Set up ggplot object
  projections$date <- as.Date(projections$date)
  plot<-
    ggplot(projections[as.Date(projections$date) >= as.Date(start_date),],
           aes(x = date, col = type, fill = type))

  # Add observed data if R or growth rate is not specified
  obs_plot <- filter(obs_data_omit, as.Date(date) >= start_date)
  y_col <- obs_plot[[as.character(obs_column)]]

  obs_plot$date <- as.Date(obs_plot$date)
  max_ycol <- max(y_col, na.rm = TRUE)
  if(forecast_type != as.character("R") &
     forecast_type != as.character("growth_rate")){
    plot <- plot +
      geom_col(data =
                 obs_plot[as.Date(obs_plot$date) >= as.Date(start_date),],
               aes(x = as.Date(date),
                   y = y_col,
                   text = paste("Observed:",
                                y_col)),
               fill = "#008080", col = "white", alpha = 0.5,
               show.legend = FALSE, na.rm = TRUE)
  }

  # plot v line for last observed date
  historic <- projections[projections$type == levels[[1]],]
  plot <- plot +
    geom_vline(
      xintercept =
        as.numeric(last(historic$date)),
      linetype = 2)

  if(forecast_type != as.character("R") &
     forecast_type != as.character("growth_rate")){


    max_predict <- max(projections$median, na.rm = TRUE)


    if(max_ycol > max_y_lim){
      max_val_y <- max_ycol
    }else{
      max_val_y <- max_y_lim
    }
  }

  # plot median line
  plot <- plot +
    geom_line(aes(y = median),
              lwd = 0.9)

  # plot CrIs
  # plot CrIs
  for (CrI in CrIs) {
    up= 100 - CrI
    bottom <- paste0("lower_", CrI)
    top <-  paste0("upper_", up)
    plot <- plot +
      geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
                  alpha = 0.2, size = 0.05)

  }


  # Set custom palette
  palette <- brewer.pal(name = "Dark2", n = 8)[c(1,3)]
  if(forecast_type != as.character("R") &
     forecast_type != as.character("growth_rate")){
    plot <- plot + annotate("text",  x = min(obs_plot$date) + x_shift, y  = max_val_y - y_shift, hjust = 1.1, vjust = 2,
                            label = annotation_text, size = 3)
  }else{
    max_predict <- max(projections$median, na.rm = TRUE)
    plot <- plot + annotate("text",  x = min(obs_plot$date) + x_shift, y  = max_y_lim - y_shift, hjust = 1.1, vjust = 5,
                            label = annotation_text, size = 3)

  }

  # add plot theming
  plot <- plot +
    theme(
      text = element_text(size = 14, family = "Arial"),
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      axis.line.x = element_line(colour = "grey"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    labs(y = ylab, x = "<b>Date</b>", col = "Type", fill = "Type", title = title) +
    expand_limits(y = c(-0.4, 0.8)) +
    scale_x_date(expand = c(0,0), date_breaks = tick_period,
                 date_labels = tick_labels_date) +
    scale_y_continuous(expand = c(0, 0))



  # Convert to plotly object
  plot <- plotly::ggplotly(plot, tooltip = c("date", "text", "median",
                                             paste0("lower", "_", CrIs), paste0("upper", "_", 100 - CrIs)))



  # Set date display constraints
  if(as.numeric(as.Date(first(projections$date))) > as.Date(last(projections$date) - interval_num)){
    a <- as.numeric(as.Date(first(projections$date)))
  }
  else{
    a <- as.numeric(as.Date(last(projections$date) - interval_num))
  }
  b <- as.numeric(as.Date(last(projections$date)))

  # Format legend layout & add annotation
  plot <- plotly::layout(plot,
                         title = list(title = list(family = "Arial",
                                                   size = 14)),
                         xaxis = list(title = list(list(
                           family = "Arial",
                           size = 10
                         )),
                           range = c(a,b)),
                         yaxis = list(title = list(list(
                           family = "Arial",
                           size = 10
                         ))),
                         legend = list(title = list(list(
                           family = "Arial",
                           size = 10
                         )),
                           #orientation = "h",
                           x = 0.02, y = 1
                         ),
                         dragmode = "pan",
                         width = 1150)



  if(isTRUE(scale)){
    tmp <- 1.75*max(projections$mean, na.rm = TRUE)
    plot <- layout(plot,
                   yaxis = list(range = c(0, tmp)))
  }

  return(plot)
}
