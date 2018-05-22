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

library(ggplot2)
library(viridis) # for colour palettes
library(scales) # for percentage axis label
library(leaflet)
library(plotly) # for interactive plots
library(shiny)


## Shiny user interface

ui <- fluidPage(
  titlePanel("Northern British Columbia Climate Research Stations"),

  fluidRow(
    column(4, offset = 1, br(),
           leafletOutput("wsmap", height = "500px"),
           htmlOutput("statsum"),
           br(),
           selectInput(inputId = "selected_site",
                       label = "Download Station Data",
                       choices = c("All stations", levels(wxstn_df$Site))),
           downloadButton("download", "Download")
    ),

    fluidRow(
      column(6, offset = -1,
             fluidRow(column(12, plotlyOutput("tempplot", height = "400px"))),
             fluidRow(column(12, plotlyOutput("precipplot", height = "400px"))),
             fluidRow(column(12, plotOutput("windplot", height = "300px"))),
             fluidRow(column(12, plotlyOutput("gustplot", height = "300px"))),
             fluidRow(column(12, plotlyOutput("solarplot", height = "300px")))
      )
    )
  )
)


## Shiny server

server <- function(input, output) {

  ## leaflet map
  output$wsmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(data = wxstn_df, ~unique(Longitude), ~unique(Latitude), layerId = ~unique(Site),
                 # clusterOptions = markerClusterOptions(),
                 popup = paste("<b>", unique(wxstn_df$Site), "</b>", "<br>",
                               "Start Date: ", wxstn_df$Date[1], "<br>",
                               "Latitude: ", unique(wxstn_df$Latitude), "<br>",
                               "Longitude: ", unique(wxstn_df$Longitude), "<br>",
                               "Elevation: ", unique(wxstn_df$Elevation), "m"
                               ))
  })

  ## generate reactive dataframe
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

  output$statsum <- renderUI({
    str1 <- paste("Maximum temperature:", round(max(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "°C", sep = " ")
    str2 <- paste("Minimum temperature:", round(min(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "°C", sep = " ")
    str3 <- paste("Average temperature:", round(mean(ggplot_data()$Temp_avg, na.rm = TRUE), 1), "°C", sep = " ")
    str4 <- paste("Maximum daily precipitation:", round(max(ggplot_data()$Rain_sum, na.rm = TRUE), 1), "mm", sep = " ")
    str5 <- paste("Maximum gust speed:", round(max(ggplot_data()$GS_max, na.rm = TRUE), 1), "m/s", sep = " ")

    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
  })

  ## average daily temperature plot
  output$tempplot <- renderPlotly({
    plot <- subset(ggplot_long(), variable == "Temperature (degree C)" | variable == "Relative Humidity (%)") %>%
      ggplot(ggplot_long(), mapping = aes(dates, value, group = years, colour = years,
                                          text = paste("<br>Date:", as.Date(Date), "<br>Value", value))) +
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
                                          text = paste("<br>Date:", as.Date(Date), "<br>Value", value))) +
      geom_line(size = 0.3, alpha = 0.7) +
      xlab("") +
      ylab("") +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_brewer(palette = "Paired", guide_legend(title = "")) +
      theme_light() +
      theme(panel.grid.minor = element_blank(), strip.text = element_text(colour = "black"),
            strip.background = element_blank(), legend.title = element_blank())
    ggplotly(plot, tooltip = c("text"))
  })

  ## average wind speed and direction plot for summer and winter seasons
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
      facet_grid(. ~ Season) +
      coord_polar() +
      theme_light() +
      theme(panel.grid.minor = element_blank(), text = element_text(size = 14),
            strip.text = element_text(colour = "black", size = 14), strip.background = element_blank())
    }
  })



  ## average wind gust plot
  output$gustplot <- renderPlotly({
    plot <- ggplot(ggplot_data(), aes(dates, GS_max, group = years, colour = years,
                                      text = paste("<br>Date:", as.Date(Date), "<br>Value", GS_max))) +
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
                                      text = paste("<br>Date:", as.Date(Date), "<br>Value", SR_avg))) +
      geom_line(stat = "smooth", method = "loess", se = FALSE, alpha = 0.7, size = 0.3) +
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
}

shinyApp(ui, server)
