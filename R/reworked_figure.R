#' Accepts incoming data and creates plotly plots to show trend in waste water data and compare observed and predicted plots.
#' @param data A dataframe to be used as the source of input data for plotting.
#' @param xaxis A character string that identifies the x axis variable (Date variable) to plot.
#' @param yaxis A list of lists that contains the entities to be used to plot the y axis. Each nested list is added as trace to the plot.
#' \itemize{
#'   \item y_column - The name of the variable to plot on yaxis.
#'   \item type - A character string that identifies the type of trace to add whether it is smoothed data, observed data or signal data. Based on that the template for plotting gets selected. Accepted values: "avg_data", "observed_data", "var_data", "doubling_time", "signal_data".
#'   \item name - A character string for the legend of the yaxis trace.
#'   \item color - A character string that indicates the color to use for the trace plot in the nested list in yaxis list that is to be added.
#'   \item width - A numeric value indicating the width of the trace plot in the nested list inside yaxis list.
#'   \item short_name - A character string indicating the type of data plotted in current trace. Accepted values: "projections", "Daily signal".
#' }
#' @param yaxis2 Another list of lists to add y axis plots. The same list components as above
#' @param yaxis_button A boolean value that specifies whether to add button for yaxis trace
#' @param yaxis2_button A boolean value that specifies whether to add button for 2nd yaxis2 trace.
#' @param y_button_name A character string specifying the name for button
#' @param y2_button_name A character string specifying the name for button on second y axis
#' @param error_bands A boolean value that identifies whether we add confidence interval bands or not.
#' @param error_data A list that contains the confidence interval variable from the dataset for calculating the band width.
#' @param error_pct A boolean value representing whether the confidence intervals are calculated as percentages
#' @param error_col A hex value representing the color of the confidence bands
#' @param error_bars A boolean value that indicates whether error bars are to be formed on the yaxis trace.
#' @param level A boolean value that indicates whether to add a trace plot for level of detect.
#' @param level_upper A variable in the dataset which contains the upper limit value for level of detect.
#' @param level_lower A variable in the dataset which contains the lower limit value for level of detect.
#' @param level_col A hex value that indicates the color for the level of detect plot
#' @param titles A list that contains titles for xaxis, yaxis and the title of the plot.
#' \itemize{
#'   \item y - A character string for the label for the yaxis
#'   \item x - A character string for the label for the xaxis.
#'   \item title - A character string for the title of the plot.
#' }
#' @param vline A boolean value indicating whether there should be a vertical line added to the plot on the xaxis.
#' @param vline_date A date value that represents where on the xaxis the vertical line should start.
#' @param smooth A boolean value indicating whether to add a smoothed curve to the plot.
#' @param smooth_y A variable in the dataset to be used for creating the smooth curve.
#' @param smooth_name A character string representing the legend name for the smoothed curve.
#' @param smooth_bandwidth A numeric value indicating the bandwidth to use for smoothed curve.
#' @param smooth_colour A hex value representing the color of the smoothed curve.
#' @param width A numeric value representing the width of the plot.
#' @param height A numeric value representing the height of the plot.
#' @param ticks A boolean value indicating whether ticks should be visible on the axis.
#' @param ticklabels A character string indicates how to represent the tick labels for datetime values on xaxis. Ex: as month and year use: "%b %Y".
#' @param date_constraint A boolean value indicates whether the xaxis dates should be constrained on the default visible plot.
#' @param constraint_val A numeric value to subtract from the last date of input dataset to determine the start date in the input dataset.
#' @param a A numeric value representing the lower limit of yaxis scale
#' @param b A numeric value representing the upper limit of yaxis scale.
#' @param specified_type A character string which contains the value of `type` entity present inside the yaxis list (another argument in this function) for the trace on which the error bars are required. Ex: "avg_data" is the value for `type` entity inside one of yaxis list which represents the trace plot for the average/ smoothed data.
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
reworked_figure <-
  function(xaxis,
           yaxis,
           data,
           titles,
           yaxis2 = NULL,
           yaxis_button = FALSE,
           yaxis2_button = FALSE,
           y_button_name = "",
           y2_button_name = "",
           error_bands = FALSE,
           error_data = NULL,
           error_pct = FALSE,
           error_col = NULL,
           error_bars = FALSE,
           level = FALSE,
           level_upper = NULL,
           level_lower = NULL,
           level_col = NULL,
           vline = FALSE,
           vline_date = NULL,
           smooth = FALSE,
           smooth_y = NULL,
           smooth_bandwidth = NULL,
           smooth_name = NULL,
           smooth_colour = NULL,
           width = 800,
           height = 500,
           ticks = TRUE,
           ticklabels = "%b %d",
           date_constraint = FALSE,
           constraint_val = NULL,
           specified_type = NULL,
           a = NULL,
           b = NULL) {
    # ---------- PRESETS ----------
    tickvals <- floor_date(as_date(data$date), "month")


    trace_presets <- list(
      doubling_time =
        list(
          mode = "line",
          type = "scatter",
          showlegend = FALSE,
          line = list(
            dash = "dash",
            color = "rgb(39, 62, 71)",
            width = 5
          )
        ),
      avg_data = list(
        type = "scatter",
        mode = "line",
        showlegend = TRUE
      ),
      observed_data = list(
        type = "bar",
        showlegend = TRUE,
        line = NULL,
        mode = NULL
        #marker = list(color = "rgb(204, 102, 255)")
      ),
      signal_data = list(
        type = "scatter",
        showlegend = TRUE
      ),
      var_data = list(
        type = "scatter",
        mode = "lines+marker",
        showlegend = TRUE
      )
    )
    attr(trace_presets$doubling_time, "class") <-
      "doubling_time"
    attr(trace_presets$avg_data, "class") <-
      "avg_data"
    attr(trace_presets$observed_data, "class") <-
      "observed_data"
    attr(trace_presets$signal_data, "class") <-
      "signal_data"
    attr(trace_presets$var_data, "class") <-
      "var_data"

    library(plotly)

    p <- plot_ly()

    # base parameters for buttons
    base_params <- 'list(
  list(
  active = 0,
  x = -0.2,
  type= "dropdown",
  direction = "down",
  xanchor = "center",
  yanchor = "top",
  pad = list("r"= 0, "t"= -25, "b" = 0),
  buttons = list(
  %s)
  )
  )'
    base_params_y2 <- 'list(
  list(
  active = 0,
  x = 1.20,
  y = 0.86,
  type= "dropdown",
  direction = "down",
  xanchor = "center",
  yanchor = "top",
  pad = list("r"= 5, "t"= 0, "b" = 0),
  buttons = list(
  %s)
  )
  )'
    updated <- NULL
    menu <- ""
    updated_y2 <- NULL
    menu_y2 <- ""
    min_val_vec = c()
    max_val_vec = c()
    for (i in 1:length(yaxis)) {
      var_to_map <- yaxis[[i]]
      min_val_vec[i] = min(data[, var_to_map$y_column])
      max_val_vec[i] = max(data[, var_to_map$y_column])
      curr_temp <- trace_presets[[var_to_map$type]]
      if (!is_null(var_to_map$color) & !is.null(var_to_map$width)) {
        curr_temp <-
          change_color(template = trace_presets[[var_to_map$type]],
                       color = var_to_map$color, width = var_to_map$width)
      }
      else if(!is_null(var_to_map$color)){
        curr_temp <-
          change_color(template = trace_presets[[var_to_map$type]],
                       color = var_to_map$color)
      }
      if (isTRUE(yaxis_button)){
        vis_logical <- c(rep(NA, length(yaxis)), rep(T, length(yaxis2)))
        vis_logical[i] <- T
        vis_logical[is.na(vis_logical)] <- F
        vis_logical <- paste0("c(",stringr::str_flatten(vis_logical, ","),")")
        menu_item <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s")))',
                             yaxis[[i]][["short_name"]],
                             vis_logical,
                             titles[["title"]])

        if (i < length(yaxis)){
          menu <- stringr::str_glue(stringr::str_glue(menu,menu_item),",")
        } else {
          menu <- stringr::str_glue(menu,menu_item)
        }
        if(i == 1){
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column]),
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}')
            ))
        }else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column]),
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}'),
              visible = FALSE
            ))
        }
      }
      else{
        if (!is.null(specified_type)){
          if((yaxis[[i]]$type == specified_type & error_bars == TRUE)){
            curr_temp_copy = curr_temp
          }
          else if(yaxis[[i]]$type != specified_type & error_bars == TRUE){
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column]),
                opacity = var_to_map$opacity,
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}')
              ))
          }
        }
        else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column]),
              opacity = var_to_map$opacity,
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}')
            ))
        }
      }
    }

    updated <- sprintf(base_params, menu)
    updated <- eval(parse(text = updated))
    if(is.null(a)){
      a <- min(min_val_vec)
    }
    if(is.null(b)){
      a <- max(max_val_vec)
    }

    if(!is.null(yaxis2)){
      for (i in 1:length(yaxis2)) {
        var_to_map <- yaxis2[[i]]
        curr_temp <- trace_presets[[var_to_map$type]]
        if (!is_null(var_to_map$color) & !is.null(var_to_map$width)) {
          curr_temp <-
            change_color(template = trace_presets[[var_to_map$type]],
                         color = var_to_map$color, width = var_to_map$width)
        }
        else if(!is_null(var_to_map$color)){
          curr_temp <-
            change_color(template = trace_presets[[var_to_map$type]],
                         color = var_to_map$color)
        }

        if (isTRUE(yaxis2_button)){
          vis_logical <- c(rep(T, length(yaxis)))
          vis_logical2 <- c(rep(NA, length(yaxis2)))
          vis_logical2[i] <- T
          vis_logical2[is.na(vis_logical2)] <- F
          vis_logical2 <- c(vis_logical, vis_logical2)
          vis_logical2 <- paste0("c(",stringr::str_flatten(vis_logical2, ","),")")
          menu_item2 <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s",
                          yaxis2.range = c(0,"%s"))))',
                                yaxis2[[i]][["short_name"]],
                                vis_logical2,
                                titles[["title"]],
                                2*max(data[,yaxis2[[i]][["y_column"]]],
                                      na.rm = TRUE))

          if (i < length(yaxis2)){
            menu_y2 <- stringr::str_glue(stringr::str_glue(menu_y2,menu_item2),",")
          } else {
            menu_y2 <- stringr::str_glue(menu_y2,menu_item2)
          }
          if(i == 1){
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column],
                     yaxis = "y2"),
                opacity = var_to_map$opacity,
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}')
              ))
          }else{
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column],
                     yaxis = "y2"),
                opacity = var_to_map$opacity,
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}'),
                visible = FALSE
              ))
          }

        }
        else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column],
                   yaxis = "y2"),
              opacity = var_to_map$opacity,
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}')
            ))
        }
      }
    }

    updated_y2 <- sprintf(base_params_y2, menu_y2)
    updated_y2 <- eval(parse(text = updated_y2))

    if(isTRUE(smooth)){
      curve <- ksmooth(x = data$date, y = data[[as.character(smooth_y)]],
                       kernel = "normal", bandwidth = smooth_bandwidth,
                       x.points = data$date)
      p <- add_trace(p, x=curve$x, y=curve$y, name=as.character(smooth_name),
                     mode = "lines",
                     line = list(color = as.character(smooth_colour),
                                 width = 2.5))
    }

    if(isTRUE(error_bands)){
      if(isTRUE(error_pct)){
        error_y_upper <-
          ifelse((data[[var_to_map$y_column]]
                  + data[[as.character(error_data)]]) > 100, 100,
                 data[[var_to_map$y_column]] + data[[as.character(error_data)]])
        error_y_lower <-
          ifelse((data[[var_to_map$y_column]] -
                    data[[as.character(error_data)]]) < 0, 0,
                 data[[var_to_map$y_column]] - data[[as.character(error_data)]])
      }
      else{
        if(length(error_data) > 1){
          error_y_upper <- data[[error_data[[1]]]]
          error_y_lower <- data[[error_data[[2]]]]
        }
        else{
          error_y_upper <-
            data[[var_to_map$y_column]] + data[[as.character(error_data)]]
          error_y_lower <-
            data[[var_to_map$y_column]] - data[[as.character(error_data)]]
        }
      }
      if(isTRUE(error_bars)){
        if(!is.null(specified_type)){
          for (i in 1:length(yaxis)) {
            if((yaxis[[i]]$type == specified_type)){
              if(!(is.null(trace_presets[[yaxis[[i]]$type]]$mode))){
                p <-
                  add_trace(p, name = yaxis[[i]]$name,  type = curr_temp_copy$type,
                            showlegend = curr_temp_copy$showlegend,
                            line = list(color = yaxis[[i]]$color, width =yaxis[[i]]$width),
                            mode = curr_temp_copy$mode,
                            x = data$date,
                            y = data[, yaxis[[i]]$y_column],
                            error_y = list(type = "data",

                                           symmetric = FALSE,

                                           array = error_y_upper,

                                           arrayminus = error_y_lower,
                                           color = "#38D9B2")
                  )
                p <- layout(p, yaxis = list(range = c(a, b)))
              }
              else{
                p <-
                  add_trace(p,  name = yaxis[[i]]$name, type = curr_temp_copy$type,
                            showlegend = curr_temp_copy$showlegend,
                            marker = list(color = yaxis[[i]]$color),
                            mode = curr_temp_copy$mode,
                            x = data$date,
                            y = data[, yaxis[[i]]$y_column],
                            #type = trace_presets[[yaxis[[i]]$type]]$type,
                            #showlegend = trace_presets[[yaxis[[i]]$type]]$showlegend,
                            error_y = list(type = "data",

                                           symmetric = FALSE,

                                           array = error_y_upper,

                                           arrayminus = error_y_lower,
                                           color = "black")
                  )
              }
            }
          }
        }
      }
      else{
        p <- add_trace(p, x = data$date, y = error_y_upper,
                       type = "scatter",
                       mode = "lines",
                       line = list(color = "transparent"),
                       showlegend = FALSE,
                       name = "Upper bound")
        p <- add_trace(p, x = data$date, y = error_y_lower,
                       type = "scatter",
                       mode = "lines",
                       fill = "tonexty",
                       fillcolor = error_col,
                       line = list(color = "transparent"),
                       showlegend = FALSE,
                       name = "Lower bound")
      }
    }
    if(isTRUE(level)){
      p <- add_trace(p, x = data$date, y = level_upper,
                     type = "scatter",
                     mode = "lines",
                     line = list(color = "transparent"),
                     showlegend = FALSE,
                     name = "Level of detection")
      p <- add_trace(p, x = data$date, y = level_lower,
                     type = "scatter",
                     mode = "lines",
                     fill = "tonexty",
                     fillcolor = level_col,
                     line = list(color = "transparent"),
                     showlegend = TRUE,
                     name = "Level of detection")
    }

    if(is.null(yaxis2)){
      if(!isTRUE(yaxis_button)){
        if(isTRUE(vline)){
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.025, y = 0.9),
                dragmode = "pan",
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.025, y = 0.9),
                dragmode = "pan",
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
        }
        else{
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.025, y = 0.9),
                dragmode = "pan"
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.025, y = 0.9),
                dragmode = "pan"
              )
          }
        }
      }
      else{
        if(isTRUE(vline)){
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 1),
                updatemenus = updated,
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 1),
                updatemenus = updated,
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
        }
        else{
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 1),
                updatemenus = updated
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 1),
                updatemenus = updated
              )
          }
        }
      }
    }
    else{
      if(isTRUE(yaxis_button)){
        if(isTRUE(vline)){
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated,
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated,
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
        }
        else{
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated)
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated)
          }
        }
      }
      else if(isTRUE(yaxis2_button)){
        tmp <- 2*max(data[,yaxis2[[1]][["y_column"]]], na.rm = TRUE)
        if(isTRUE(vline)){
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                annotations = list(
                  x = 1.26, y = 0.95, text = y2_button_name,
                  showarrow = F, xref='paper', yref='paper',
                  font=list(size=15)
                ),
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated_y2,
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                annotations = list(
                  x = 1.26, y = 0.95, text = y2_button_name,
                  showarrow = F, xref='paper', yref='paper',
                  font=list(size=15)
                ),
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated_y2,
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
        }
        else{
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                annotations = list(
                  x = 1.26, y = 0.95, text = y2_button_name,
                  showarrow = F, xref='paper', yref='paper',
                  font=list(size=15)
                ),
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated_y2
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                annotations = list(
                  x = 1.26, y = 0.95, text = y2_button_name,
                  showarrow = F, xref='paper', yref='paper',
                  font=list(size=15)
                ),
                legend = list(x = 0.05, y = 0.9),
                updatemenus = updated_y2
              )
          }
        }
      }
      else{
        tmp <- 2*max(data[,yaxis2[[1]][["y_column"]]], na.rm = TRUE)
        if(isTRUE(vline)){
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9),
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9),
                shapes = list(vline(x = as.Date(vline_date)))
              )
          }
        }
        else{
          if(isTRUE(ticks)){
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickvals = tickvals,
                             tickformat = "%b"),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9)
              )
          }
          else{
            p <-
              layout(
                p,
                title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
                xaxis = list(type = "date",
                             title = list(text = as.character(titles[["x"]])),
                             automargin = TRUE, tickformat = ticklabels),
                yaxis = list(title = list(text = as.character(titles[["y"]])),
                             automargin = TRUE, overlaying = "y2",
                             zeroline = FALSE),
                yaxis2 = list(
                  side = "right",
                  title = list(text = as.character(titles[["y2"]])),
                  automargin = TRUE,
                  showgrid = FALSE,
                  range = c(0, tmp)
                ),
                barmode =  "relative",
                bargap = 0,
                autosize = FALSE,
                width = width,
                height = height,
                legend = list(x = 0.05, y = 0.9)
              )
          }
        }
      }
    }

    if(isTRUE(date_constraint)){
      if(as.Date(first(data$date)) > as.Date(last(data$date)) - 40){
        a <- as.Date(first(data$date))
      }
      else{
        a <- as.Date(last(data$date)) - constraint_val
      }
      b <- as.Date(last(data$date))
      p <- layout(p, xaxis = list(range = c(a, b)))
    }
    p <- p %>% layout(font = list(family = "Arial"))
    return(p)

  }
