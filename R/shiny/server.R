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

## Shiny server ##

server <- function(input, output) {

  ## Long-term records ####

  ## leaflet map
  output$wsmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
      addAwesomeMarkers(data = wxstn_sites, ~Longitude, ~Latitude, ~Site,icon = icons,
                 popup = paste0("<b>", wxstn_sites$Site, "</b>", "<br>",
                                "Latitude: ", wxstn_sites$Latitude, "<br>",
                                "Longitude: ", wxstn_sites$Longitude, "<br>",
                                "Elevation: ", wxstn_sites$Elevation, "m", "<br>"
                                # ,"<img src='", wxstn_sites$Site, ".png'", "/>"
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
    HTML(paste("Click on a station to view climate data and summary statistics.<br>",
               "You can hover on the graph, zoom in and out, and select specific years for display."))
  })

  output$statsum <- renderUI({
    req(input$wsmap_marker_click$id)
    str1 <- paste("<h4><b>", input$wsmap_marker_click$id, "</b></h4>")
    str2 <- paste("Data range:", filter(wxstn_df, Site == input$wsmap_marker_click$id)$Date[1], "to",
                  filter(wxstn_df, Site == input$wsmap_marker_click$id)$Date[nrow(filter(wxstn_df, Site == input$wsmap_marker_click$id))], "<br/>")
    str3 <- paste("Maximum temperature:", round(max(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "&deg;C", "<br/>", sep = " ")
    str4 <- paste("Minimum temperature:", round(min(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "&deg;C", "<br/>", sep = " ")
    str5 <- paste("Average temperature:", round(mean(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "&deg;C", "<br/>", sep = " ")
    str6 <- paste("Maximum daily precipitation:", round(max(ggplot_data()$Rain_sum, na.rm = TRUE), 1), "mm", "<br/>", sep = " ")
    str7 <- paste("Maximum gust speed:", round(max(ggplot_data()$GS_max, na.rm = TRUE), 1), "m/s", "<br/>", sep = " ")
    str8 <- "<br/>*All plots are from daily records except for windrose using hourly wind speed and directions."

    HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8))
  })

  ## average daily temperature, relative humidity, precipitation and pressure plot
  output$tempplot <- renderPlotly({

    plot <- subset(ggplot_long(), variable == "Temperature (degree C)" | variable == "Relative Humidity (%)" |
                     variable == "Precipitation (mm)" | variable == "Pressure (mb)") %>%
      ggplot(ggplot_long(), mapping = aes(dates, value, group = years, colour = years,
                                          text = paste("<br>Date:", as.Date(Date), "<br>Value:", value))) +
      geom_line(size = 0.3, alpha = 0.7) +
      xlab("") +
      ylab("") +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_manual(values = pal) +
      theme_light() +
      theme(panel.grid.minor = element_blank(), strip.text = element_text(colour = "black"),
            strip.background = element_blank(), legend.title = element_blank())
    ggplotly(plot, tooltip = c("text")) %>%
      layout(margin = list(l = 35)) # to fully display the x and y axis labels
  })


  ## average wind speed and direction plot for summer and winter growing seasons
  output$windplot <- renderPlot({
    if (all(is.na(ggplot_wind()$WS))) {
      ggplot(ggplot_wind(), mapping = aes(WD, fill = WS)) +
        scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W")) +
        xlab("") +
        ylab("") +
        coord_polar() +
        theme_light() +
        theme(panel.grid.minor = element_blank(), text = element_text(size = 14),
              strip.text = element_text(colour = "black", size = 14), strip.background = element_blank())

    } else {
      ggplot(ggplot_wind(), aes(WD, fill = WS)) +
        geom_histogram(stat = "identity", mapping = aes(y = n, width = 1), alpha = 0.8,
                       colour = "grey30") +
        scale_x_discrete(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W")) +
        scale_fill_viridis_d(guide_legend(title = "Wind Speed\n(km/h)"), direction = -1,
                             labels = c(">9", "6 - 9", "3 - 6", "<3")) +
        labs(title = "", x = "", y = "") +
        facet_grid(. ~ gseason) +
        coord_polar(start = -0.25) + # tilt the polar plot to the correct direction
        theme_light() +
        theme(panel.grid.minor = element_blank(), text = element_text(size = 14),
              strip.text = element_text(colour = "black", size = 14), strip.background = element_blank())

    }
  })

  ## average wind gust plot
  output$gustplot <- renderPlotly({

    plot <- ggplot(ungroup(ggplot_data()), aes(dates, GS_max, group = years, colour = years,
                                               text = paste("<br>Date:", as.Date(Date), "<br>Value:", GS_max))) +
      geom_line(size = 0.3, alpha = 0.7) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_colour_manual(values = pal) +
      xlab("") +
      ylab("Maximum Gust Speed (m/s)") +
      theme_light() +
      theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
            axis.title.y = element_text(size = 10))
    p <- ggplotly(plot, tooltip = c("text")) %>%
      layout(margin = list(l = 75)) ## adjusting y axis position so it doesn't overlap axis labels
  })

  ## average daily insolation plot
  output$solarplot <- renderPlotly({

    if (all(is.na(ggplot_data()$monthly_inso))) {
      plot <- ggplot(ungroup(ggplot_data()), aes(months, monthly_inso, group = years, colour = years)) +
        theme_light() +
        xlab("") +
        ylab("Average Monthly Insolation (W/m^2)") +
        theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title.y = element_text(size = 10))
      p <- ggplotly(plot) %>%
        layout(margin = list(l = 75))

    } else {
      plot <- ggplot(ungroup(ggplot_data()), aes(months, monthly_inso, group = years, colour = years,
                                                 text = paste("<br>Month:", Month, "<br>Value:", monthly_inso))) +
        geom_line(alpha = 0.7, size = 0.3, na.rm = TRUE) +
        # scale_x_date(date_labels = "%b") +
        scale_colour_manual(values = pal) +
        xlab("") +
        ylab("Average Monthly Insolation (W/m^2)") +
        theme_light() +
        theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title.y = element_text(size = 10))
      p <- ggplotly(plot, tooltip = c("text")) %>%
        layout(margin = list(l = 75))
    }
  })

  ## download file
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$selected_site, ".csv")
    },
    content = function(file) {
      wxstn_download <- wxstn_df[, !names(wxstn_df) %in% c("years", "months", "dates", "monthly_inso", "seasons", "gseason")]
      if (input$selected_site == "All stations") {
        write.csv(wxstn_download, file, row.names = FALSE)
      } else {
        write.csv(wxstn_download[wxstn_download$Site %in% input$selected_site,], file, row.names = FALSE)
      }
    }
  )


  ## Statistics ####

  ## datatable
  suminput <- reactive({
    switch(input$sum_tbl,
           "Annual" = annual_sum[annual_sum$Site == input$sum_site, ],
           "Monthly all years" = monthly_sum[monthly_sum$Site == input$sum_site, ],
           "Monthly per year" = month_year_sum[month_year_sum$Site == input$sum_site, ],
           "Seasonal" = seasonal_sum[seasonal_sum$Site == input$sum_site, ],
           "Growing season" = gseason_sum[gseason_sum$Site == input$sum_site, ])
  })

  output$table <- renderDataTable({
    suminput()
  })

  output$caveat <- renderUI({
    HTML("obs_na shows the number of NAs during the period of statistical summary.
         Please use the summary results with caution.<br>")
  })

  ## export file
  output$exportstats <- downloadHandler(
    filename <- function() {
      paste0(input$sum_site, "_", input$sum_tbl, "_summary", ".csv")
    },
    content <- function(file) {
      write.csv(suminput(), file, row.names = FALSE)
    }
  )


  ## Real-time data ####

  ## real time station data
  output$rtmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
      addAwesomeMarkers(data = rt, ~Longitude, ~Latitude, layerId = ~Site, icon = icons,
                 popup = paste("<b>", rt$Site, "</b>", "<br>",
                               "Latitude: ", rt$Latitude, "<br>",
                               "Longitude: ", rt$Longitude, "<br>",
                               "Elevation: ", rt$Elevation, "m"
                 ))
  })



  rt_df <- eventReactive(input$rtmap_marker_click$id, {

    ## reading in weekly data
    req(input$rtmap_marker_click$id)
    if (input$rtmap_marker_click$id == "Blackhawk") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234062107550/temp/300234062107550_live.txt", # Dawson_Creek__010.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "Canoe") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065020820/temp/300234065020820_live.txt", # 20143961_004.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "Hourglass") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234062105500/temp/300234062105500_live.txt", # Dawson_creek__006.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24)  %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "Hudson Bay Mountain") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065724550/temp/300234065724550_live.txt", # 20143959_003.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "McBride Peak") {
      df <- tail(read.csv("http://datagarrison.com/users/300234062103550/300234064336030/temp/300234064336030_live.txt", # 10839071_004.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "Nonda") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065500940/temp/300234065500940_live.txt", # 10890475_008.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "Bowron Pit") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065022520/temp/300234065022520_live.txt", # BowronPit2_018.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }
    else if (input$rtmap_marker_click$id == "Gunnel") {
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065873520/temp/300234065873520_live.txt", # Gunneltest_006.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    } else {

      ## Pink Mountain
      df <- tail(read.csv("https://datagarrison.com/users/300234062103550/300234065506710/temp/300234065506710_live.txt", # 10890467_009.txt
                          sep = "\t", skip = 2, header = T), input$rt_days*24) %>%
        dplyr::select(matches(col_str))
    }

    ## data cleaning
    # rename
    change_to <- c("Rain_sum", "Pressure_avg", "Temp_avg", "RH_avg", "WS_avg", "GS_max", "WD_avg", "SR_avg")
    old_names <- c("Rain", "Pressure", "Temperature", "RH", "Wind.Speed", "Gust.Speed", "Wind.Direction", "Solar")

    for (i in 1:length(old_names)) {
      col <- df %>% select(matches(old_names[i])) %>% colnames()
      names(df)[names(df) == col] <- change_to[i]
    }

    df$Date_Time <- as.POSIXct(df$Date_Time, format = "%m/%d/%y %H:%M:%S")
    df$WD <- cut(df$WD_avg, 22.5*(0:16), right = FALSE, dig.lab = 4)
    levels(df$WD) <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW",
                       "W", "WNE", "NW", "NNW")
    return(df)
  }
  )

  output$rt_tempplot <- renderPlotly({
    plot_ly(rt_df(), x = ~(Date_Time)) %>%
      add_lines(y = ~Temp_avg, name = "Temperature", line = list(color = "#fb8072")) %>%
      add_lines(y = ~RH_avg, name = "Relative Humidity", line = list(color = "#a6cee3"), yaxis = "y2") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Temperature (degree C)"),
             yaxis2 = list(overlaying = "y", side = "right", title = "Relative Humidity (%)"),
             # legend = list(y = 1.3)
             margin = list(r = 50), showlegend = FALSE) # margin so second axis renders OK

  })

  output$rttemp <- renderUI({
    temp <- "<font color='#fb8072'>Temperature</font><br>"
    t <- round(rt_df()[nrow(rt_df()), "Temp_avg"], 1)
    rh <- "<font color='#a6cee3'>Relative Humidity</font><br>"
    r <- round(rt_df()[nrow(rt_df()), "RH_avg"], 1)
    HTML(paste(temp, "<font size='6', color='#fb8072'>", t, "</font>",  "<font color='#fb8072'> &deg;C</font><br>",
               rh, "<font size='6', color='#a6cee3'>", r, "</font>", "<font color='#a6cee3'> %</font>", sep = ""))
  })

  output$rt_precipplot <- renderPlotly({
    plot_ly(rt_df(), x = ~Date_Time) %>%
      add_bars(y = ~Rain_sum, name = "Precipitation", marker = list(color = "#67a9cf")) %>%
      add_lines(y = ~Pressure_avg, name = "Pressure", line = list(color = "#b0b0b0"), yaxis = "y2") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Precipitation (mm)"),
             yaxis2 = list(overlaying = "y", side = "right", title = "Pressure (mb)"),
             margin = list(r = 50), showlegend = FALSE)
  })

  output$rtprecip <- renderUI({
    rain <- "<font color='#67a9cf'>Precipitation</font><br>"
    r <- round(rt_df()[nrow(rt_df()), "Rain_sum"], 1)
    pres <- "<font color='#b0b0b0'>Pressure</font><br>"
    p <- round(rt_df()[nrow(rt_df()), "Pressure_avg"], 1)
    HTML(paste(rain, "<font size='6', color='#67a9cf'>", r, "</font>",  "<font color='#67a9cf'> mm</font><br>",
               pres, "<font size='6', color='#b0b0b0'>", p, "</font>", "<font color='#b0b0b0'> mb</font>", sep = ""))
  })

  output$rt_windplot <- renderPlotly({
    plot_ly(rt_df(), x = ~Date_Time, text = ~paste("Wind Direction:", WD)) %>%
      add_lines(y = ~WS_avg, name = "Wind Speed", line = list(color = "#2171b5")) %>%
      add_lines(y = ~GS_max, name = "Gust Speed", line = list(dash = "dash", color = "#505050"), yaxis = "y2") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Wind Speed (m/s)"),
             yaxis2 = list(overlaying = "y", side = "right", title = "Maximum Gust Speed (m/s)"),
             margin = list(r = 50), showlegend = FALSE)
  })

  output$rtwind <- renderUI({
    wind <- "<font color='#2171b5'>Wind Speed</font><br>"
    w <- round(rt_df()[nrow(rt_df()), "WS_avg"], 1)
    wd <- "<font color='#2171b5'>Wind Direction</font><br>"
    d <- rt_df()[nrow(rt_df()), "WD"]
    gust <- "<font color='#505050'>Gust Speed</font><br>"
    g <- round(rt_df()[nrow(rt_df()), "GS_max"], 1)
    HTML(paste(wind, "<font size='6', color='#2171b5'>", w, "</font>", "<font color='#2171b5'>m/s</font><br>",
               wd, "<font size='6', color='#2171b5'>", d, "</font><br>",
               gust, "<font size='6', color='#505050'>", g, "</font>", "<font color='#505050'> m/s</font><br>", sep = ""))
  })

  output$rt_solarplot <- renderPlotly({
    plot_ly(rt_df(), x = ~Date_Time) %>%
      add_lines(y = ~SR_avg, name = "Solar Radiation", line = list(color = "#fc8d59")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Solar Radiation (W/m^2)"))
  })

  output$rtsolar <- renderUI({
    solar <- "<font color='#fc8d59'>Solar Radiation</font><br>"
    s <- round(rt_df()[nrow(rt_df()), "SR_avg"], 1)
    HTML(paste(solar, "<font size='6', color='#fc8d59'>", s, "</font>",  "<font color='#fc8d59'> W/m<sup>2</sup></font><br>", sep = ""))
  })

  output$rtcap <- renderUI({
    HTML("Click on a station to view or download real-time data.")
  })

  output$rtstation <- renderUI({
    req(input$rtmap_marker_click$id)
    HTML(paste("<h4><b>", input$rtmap_marker_click$id, "</b></h4>",
               "Plots display the last available",
               input$rt_days,
               "days of records. For complete records, please see About page."))})

  ## download
  output$downloadrt <- downloadHandler(
    filename = function() {
      paste0(input$rtmap_marker_click$id, "_", Sys.Date(), "_", input$rt_days,"d_", real-time_data.csv")
    },
    content = function(file) {
      write.csv(rt_df(), file, row.names = FALSE)
    }
  )


  ## About ####

  output$about <- renderUI({
    HTML(paste("<h1>Climate Data</h1>",
               "Long-term data are summarised daily records except for wind speed and
               direction which use hourly records. Real-time data are hourly records. The column names
               in both the long-term record and real-time dataframes are coded as follows:<br>",
               "<br>Rain_sum: total precipitation, mm<br>
               Pressure_avg: averaged pressure, mbar<br>
               Temp_max: maximum temperature, &deg;C<br>
               Temp_min: minimum temperature, &deg;C<br>
               Temp_avg: averaged temperature, &deg;C<br>
               RH_avg: averaged relative humidity, %<br>
               DP_avg: averaged dew point, &deg;C<br>
               WS_avg: averaged wind speed, m/s<br>
               GS_max: maximum gust speed, m/s<br>
               WD_avg: averaged wind direction, &Phi;<br>
               SR_avg: averaged solar radiation, W/m<sup>2</sup><br>
               SD_avg: averaged snow depth, cm<br>
               WC_avg: averaged soil moisture water content, m<sup>3</sup>/m<sup>3</sup><br>
               W_avg: averaged wetness, %<br>
               WS_EC5: averaged soil moisture water content from EC5 sensor, m<sup>3</sup>/m<sup>3</sup><br>",
               "<br><h1>Real-time Data</h1>",
               "Please refer to the following links for complete real-time data records.<br>",
               "<br><a href='https://datagarrison.com/users/300234062103550/300234062107550/plots.php'  target='_blank'>Blackhawk</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234065022520/plots.php'  target='_blank'>Bowron Pit</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234065020820/plots.php' target='_blank'>Canoe</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234065873520/plots.php'  target='_blank'>Gunnel</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234062105500/plots.php' target='_blank'>Hourglass</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234065724550/plots.php' target='_blank'>Hudson Bay Mountain</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234064336030/plots.php' target='_blank'>McBride Peak</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234065500940/plots.php' target='_blank'>Nonda</a><br>",
               "<a href='https://datagarrison.com/users/300234062103550/300234065506710/plots.php' target='_blank'>Pink Mountain</a><br>",
               "<h1>Contact</h1>",
               "<b>Vanessa Foord</b> (Research Climatologist): Vanessa.Foord@gov.bc.ca<br>
               <b>Alexandre Bevington</b> (Research Hydrologist): Alexandre.Bevington@gov.bc.ca<br>
               <b>Jane Wang</b> (Application Support): https://twitter.com/janewyx<br>
               <br>The code for creating this website application is
               <a href='https://github.com/bcgov/nbcclim' target='_blank'>available on GitHub.</a><br>"
    ))
  })

  }

