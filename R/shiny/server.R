# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## Shiny server

server <- function(input, output) {

  ## leaflet map
  output$wsmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(data = wxstn_df, ~unique(Longitude), ~unique(Latitude), layerId = ~unique(Site),
                 popup = paste("<b>", unique(wxstn_df$Site), "</b>", "<br>",
                               "Latitude: ", unique(wxstn_df$Latitude), "<br>",
                               "Longitude: ", unique(wxstn_df$Longitude), "<br>",
                               "Elevation: ", unique(wxstn_df$Elevation), "m"
                 ))
  })

  ## generate reactive dataframes
  ggplot_data <- reactive({
    req(input$wsmap_marker_click$id)
    site <- input$wsmap_marker_click$id
    wxstn_df[wxstn_df$Site %in% site,]
  })

  ggplot_long <- reactive({
    req(input$wsmap_marker_click$id)
    site <- input$wsmap_marker_click$id
    df_long[df_long$Site %in% site, ]
  })

  ggplot_wind <- reactive({
    req(input$wsmap_marker_click$id)
    site <- input$wsmap_marker_click$id
    wind_df[wind_df$Site %in% site, ]
  })

  output$caption <- renderUI({
    HTML("Click on a station to see climate data and summary statistics.")
  })

  output$statsum <- renderUI({
    req(input$wsmap_marker_click$id)
    str1 <- paste("<h4><b>", input$wsmap_marker_click$id, "</b></h4>")
    str2 <- paste("Data range:", filter(wxstn_df, Site == input$wsmap_marker_click$id)$Date[1], "to",
                  filter(wxstn_df, Site == input$wsmap_marker_click$id)$Date[nrow(filter(wxstn_df, Site == input$wsmap_marker_click$id))], "<br/>")
    str3 <- paste("Maximum temperature:", round(max(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "°C", "<br/>", sep = " ")
    str4 <- paste("Minimum temperature:", round(min(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "°C", "<br/>", sep = " ")
    str5 <- paste("Average temperature:", round(mean(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "°C", "<br/>", sep = " ")
    str6 <- paste("Maximum daily precipitation:", round(max(ggplot_data()$Rain_sum, na.rm = TRUE), 1), "mm", "<br/>", sep = " ")
    str7 <- paste("Maximum gust speed:", round(max(ggplot_data()$GS_max, na.rm = TRUE), 1), "m/s", "<br/>", sep = " ")
    str8 <- "<br/>*All plots are from daily records except for windrose using hourly wind speed and directions."

    HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8))
  })

  ## average daily temperature plot
  output$tempplot <- renderPlotly({
    plot <- subset(ggplot_long(), variable == "Temperature (degree C)" | variable == "Relative Humidity (%)") %>%
      ggplot(ggplot_long(), mapping = aes(dates, value, group = years, colour = years,
                                          text = paste("<br>Date:", as.Date(Date), "<br>Value:", value))) +
      geom_line(size = 0.3, alpha = 0.7) +
      xlab("") +
      ylab("") +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_colour_brewer(palette = "Paired") +
      theme_light() +
      theme(panel.grid.minor = element_blank(), strip.text = element_text(colour = "black"),
            strip.background = element_blank(), legend.title = element_blank())
    ggplotly(plot, tooltip = c("text"))
  })

  ## average daily total precipitation plot
  output$precipplot <- renderPlotly({
    plot <- subset(ggplot_long(), variable == "Precipitation (mm)" | variable == "Pressure (mb)") %>%
      ggplot(ggplot_long(), mapping = aes(dates, value, group = years, colour = years,
                                          text = paste("<br>Date:", as.Date(Date), "<br>Value:", value))) +
      geom_line(size = 0.3, alpha = 0.7) +
      xlab("") +
      ylab("") +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_brewer(palette = "Paired") +
      theme_light() +
      theme(panel.grid.minor = element_blank(), strip.text = element_text(colour = "black"),
            strip.background = element_blank(), legend.title = element_blank())
   ggplotly(plot, tooltip = c("text"))
  })

  ## average wind speed and direction plot for summer and winter growing seasons
  output$windplot <- renderPlot({
    if (all(is.na(ggplot_wind()$WS))) {
      ggplot(ggplot_wind(), mapping = aes(WD, fill = WS)) +
        scale_y_continuous(labels = percent) +
        scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W")) +
        xlab("") +
        ylab("") +
        coord_polar() +
        theme_light() +
        theme(panel.grid.minor = element_blank(), text = element_text(size = 14),
              strip.text = element_text(colour = "black", size = 14), strip.background = element_blank())

    } else {
      filter(ggplot_wind(), !is.na(WS)) %>%
        ggplot(ggplot_wind, mapping = aes(WD, fill = WS)) +
        geom_histogram(binwidth = 30, mapping = aes(y = (..count..)/sum(..count..)), alpha = 0.8,
                       colour = "grey30") +
        scale_y_continuous(labels = percent) +
        scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W")) +
        scale_fill_viridis(discrete = TRUE, guide_legend(title = "Wind Speed\n(km/h)"),
                           labels = c(">9", "6 - 9", "3 - 6", "<3"), direction = -1) +
        xlab("") +
        ylab("") +
        facet_grid(. ~ gseason) +
        coord_polar() +
        theme_light() +
        theme(panel.grid.minor = element_blank(), text = element_text(size = 14),
              strip.text = element_text(colour = "black", size = 14), strip.background = element_blank())
    }
  })

  ## average wind gust plot
  output$gustplot <- renderPlotly({
    plot <- ggplot(ggplot_data(), aes(dates, GS_max, group = years, colour = years,
                                      text = paste("<br>Date:", as.Date(Date), "<br>Value:", GS_max))) +
      geom_line(size = 0.3, alpha = 0.7) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_brewer(palette = "Paired") +
      xlab("") +
      ylab("Maximum Gust Speed (m/s)") +
      theme_light() +
      theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
            axis.title.y = element_text(size = 10))
    p <- ggplotly(plot, tooltip = c("text"))

    ## adjusting y axis position so it doesn't overlap axis labels
    p[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1
    p %>% layout(margin = list(l = 75))
  })

  ## average daily insolation plot
  output$solarplot <- renderPlotly({
    if (all(is.na(ggplot_data()$SR_avg))) {
      plot <- ggplot(ggplot_data(), aes(dates, SR_avg, group = years, colour = years)) +
        theme_light() +
        xlab("") +
        ylab("Smoothened Insolation (W/m^2)") +
        theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title.y = element_text(size = 10))
      p <- ggplotly(plot)

      ## adjusting y axis position so it doesn't overlap axis labels
      p[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1
      p %>% layout(margin = list(l = 75))

    } else {
      plot <- ggplot(ggplot_data(), aes(dates, SR_avg, group = years, colour = years,
                                        text = paste("<br>Date:", as.Date(Date), "<br>Value:", SR_avg))) +
        geom_line(stat = "smooth", method = "loess", se = FALSE, alpha = 0.7, size = 0.3) +
        # stat_summary(aes(color = years), geom = "point", fun.y = mean) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        scale_color_brewer(palette = "Paired") +
        xlab("") +
        ylab("Smoothened Insolation (W/m^2)") +
        theme_light() +
        theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title.y = element_text(size = 10))
      p <- ggplotly(plot, tooltip = c("text"))

      ## adjusting y axis position so it doesn't overlap axis labels
      p[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1
      p %>% layout(margin = list(l = 75))
    }
  })

  ## download file
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$selected_site, ".csv")
    },
    content = function(file) {
      if (input$selected_site == "All stations") {
        write.csv(wxstn_df, file, row.names = FALSE)
      } else {
        write.csv(wxstn_df[wxstn_df$Site %in% input$selected_site,], file, row.names = FALSE)
      }
    }
  )

  ## select time range
  # observeEvent(input$selected_site, {
  #   updateSliderInput(session, "timerange",
  #                     min = wxstn_df[wxstn_df$Site %in% input$selected_site, "Date"][1],
  #                     max = wxstn_df[wxstn_df$Site %in% input$selected_site, "Date"][nrow(wxstn_df[wxstn_df$Site %in% input$selected_site,])])
  # })


   ## datatable
  suminput <- reactive({
    switch(input$sum_tbl,
           "Annual" = annual_sum[annual_sum$Site == input$sum_site, ],
           "Monthly" = monthly_sum[monthly_sum$Site == input$sum_site, ],
           "Seasonal" = seasonal_sum[seasonal_sum$Site == input$sum_site, ],
           "Growing season" = gseason_sum[gseason_sum$Site == input$sum_site, ])
  })

  output$table <- renderDataTable(
    suminput()
  )


  ## download file
  output$exportstats <- downloadHandler(
    filename <- function() {
      paste0(input$sum_site, "_", input$sum_tbl, "_summary", ".csv")
    },
    content <- function(file) {
      write.csv(suminput(), file, row.names = FALSE)
    }
  )


  ## real time station data
  rt_df <- eventReactive(input$station, {
    ## reading in weekly data
    if (input$station == "Canoe") {
      df <- tail(read.csv("http://datagarrison.com/users/300234062103550/300234065020820/temp/20143961_003.txt",
                          sep = "\t", skip = 2)[, c("Date_Time", "Rain_10892830_mm", "Pressure_3247647_mbar", "Temperature_10804732_deg_C", "RH_10804732_.", "Wind.Speed_10918296_m.s", "Gust.Speed_10918296_m.s", "Wind.Direction_10918296_deg", "Solar.Radiation_10400749_W.m.2")], 168)
      }
    else if (input$station == "Hudson Bay Mountain") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065724550/temp/20143959_003.txt",
                          sep = "\t", skip = 2)[, c("Date_Time", "Rain_2284502_mm", "Pressure_10369385_mbar", "Temperature_3324931_deg_C", "RH_3324931_.", "Wind.Speed_10918298_m.s", "Gust.Speed_10918298_m.s", "Wind.Direction_10918298_deg", "Solar.Radiation_10485755_W.m.2")], 168)
      }
    else if (input$station == "McBride Peak") {
      df <- tail(read.csv("http://datagarrison.com/users/300234062103550/300234064336030/temp/10839071_004.txt",
                          sep = "\t", skip = 2)[, c("Date_Time", "Rain_2007476_mm", "Pressure_3247631_mbar", "Temperature_10492947_deg_C", "RH_10492947_.", "Wind.Speed_3330635_m.s", "Gust.Speed_3330635_m.s", "Wind.Direction_3330635_deg", "Solar.Radiation_2280206_W.m.2")], 168)
      }
    else if (input$station == "Nonda") {
      df <- tail(read.csv("http://datagarrison.com/users/300234062103550/300234065500940/temp/10890475_004.txt",
                          sep = "\t", skip = 2)[, c("Date_Time", "Rain_10540414_mm", "Pressure_3247646_mbar", "Temperature_3241737_deg_C", "RH_3241737_.", "Wind.Speed_3284783_m.s", "Gust.Speed_3284783_m.s", "Wind.Direction_3284783_deg", "Solar.Radiation_10328367_W.m.2")], 168)
      } else {
      df <- tail(read.csv("http://datagarrison.com/users/300234062103550/300234065506710/temp/10890467_008.txt",
                          sep = "\t", skip = 2)[, c("Date_Time", "Rain_2440494_mm", "Pressure_3247633_mbar", "Temperature_2450352_deg_C", "RH_2450352_.", "Wind.Speed_3330634_m.s", "Gust.Speed_3330634_m.s", "Wind.Direction_3330634_deg", "Solar.Radiation_1114619_W.m.2")], 168)
      }

    ## data cleaning
    colnames(df) <- c("Date_Time", "Rain_sum", "Pressure_avg", "Temperature_avg", "RH_avg", "WS_avg", "GS_max", "WD_avg", "SR_avg")
    df$Date_Time <- as.POSIXct(df$Date_Time, format = "%m/%d/%y %H:%M:%S")
    df$WS_avg <- df$WS_avg * 3.6
    df$WS_avg <- cut(df$WS_avg, c(-Inf, 3, 6, 9, Inf))
    return(df)
    }

  )

  output$rt_tempplot <- renderPlot(
    ggplot(rt_df(), aes(Date_Time)) +
      geom_line(aes(y = Temperature_avg, colour = "Temperature"), size = 1, alpha = 0.7) +
      geom_line(aes(y = RH_avg/5, colour = "Relative Humidity"), size = 1, alpha = 0.7) + # /5 for second axis transformation
      xlab("") +
      scale_x_datetime(date_breaks = "1 day") +
      scale_y_continuous(name = "Temperature degree C",
                         sec.axis = sec_axis(~.*5, name = "Relative Humidity (%)")) + # *5 from original/left axis
      scale_color_manual(values = c("#fb6a4a", "#6baed6")) +
      theme_light() +
      theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
            legend.position = c(0.95, 0.9), legend.background = element_blank(), legend.title = element_blank())
  )

  output$rt_precipplot <- renderPlot(
    ggplot(rt_df(), aes(Date_Time)) +
      geom_bar(aes(y = Rain_sum, fill = "Precipitation"), stat = "identity") +
      geom_line(aes(y = Pressure_avg - 770, colour = "Pressure"), size = 0.8) +
      xlab("") +
      scale_x_datetime(date_breaks = "1 day") +
      scale_y_continuous(name = "Precipitation (mm)",
                         sec.axis = sec_axis(~.+770, name = "Pressure (mb)")) +
      scale_fill_manual(values = "#67a9cf") +
      scale_colour_manual(values = "grey") +
      theme_light() +
      theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
            legend.position = c(0.95, 0.9), legend.background = element_blank(), legend.title = element_blank())
  )

  output$rt_windplot <- renderPlot(
    filter(rt_df(), !is.na(WS_avg)) %>%
      ggplot(rt_df(), mapping = aes(WD_avg, fill = WS_avg)) +
      geom_histogram(binwidth = 30, mapping = aes(y = (..count..)/sum(..count..)), alpha = 0.8,
                     colour = "grey30") +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W")) +
      scale_fill_viridis(discrete = TRUE, guide_legend(title = "Wind Speed\n(km/h)"),
                         labels = c(">9", "6 - 9", "3 - 6", "<3"), direction = -1) +
      xlab("") +
      ylab("") +
      coord_polar() +
      theme_light() +
      theme(panel.grid.minor = element_blank(), text = element_text(size = 14),
            strip.text = element_text(colour = "black", size = 14), strip.background = element_blank())

  )
}

