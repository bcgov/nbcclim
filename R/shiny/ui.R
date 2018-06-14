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
library(DT) # for rendering data tables
library(formattable)
library(shiny)


## Shiny user interface

ui <- navbarPage(HTML("<h4><b>Northern British Columbia Climate Research Stations</b></h4>"),

  tabPanel(HTML("<h4>Long-term Records</h4>"),

    fluidRow(
      column(4, offset = 1, br(),
             leafletOutput("wsmap", height = "500px"),
             htmlOutput("caption"),
             htmlOutput("statsum"),
             br(),
             selectInput(inputId = "selected_site",
                         label = "Download Station Data",
                         choices = c("All stations", levels(wxstn_df$Site))),
             downloadButton("download", "Download")
                   ),

      fluidRow(
        column(6, offset = -1,
               fluidRow(column(12, plotlyOutput("tempplot", height = "800px"))),
               fluidRow(column(12, plotOutput("windplot", height = "300px"))),
               fluidRow(column(12, plotlyOutput("gustplot", height = "300px"))),
               fluidRow(column(12, plotlyOutput("solarplot", height = "300px")))
               )
        )
      )
    ),

  tabPanel(HTML("<h4>Statistics</h4>"),
    selectInput("sum_site", "Site", c(levels(wxstn_df$Site))),
    selectInput("sum_tbl", "Summary type", c("Annual", "Monthly all years", "Monthly per year", "Seasonal", "Growing season")),
    dataTableOutput("table"),
    htmlOutput("caveat"),
    downloadButton("exportstats", "Export")
    ),

  tabPanel(HTML("<h4>Real-time Data</h4>"),
           fluidRow(
             column(4, offset = 1, br(),
                    leafletOutput("rtmap", height = "500px"),
                    htmlOutput("rtcap"),
                    htmlOutput("rtstation"), br(),
                    downloadButton("downloadrt", "Download")
             ),
             fluidRow(
               column(6,
                      fluidRow(column(10, plotlyOutput("rt_tempplot", height = "300px")),
                               fluidRow(column(2, br(), br(), htmlOutput("rttemp")))
                               ),
                      fluidRow(column(10, plotlyOutput("rt_precipplot", height = "300px")),
                               fluidRow(column(2, br(), br(), htmlOutput("rtprecip")))),
                      fluidRow(column(10, plotlyOutput("rt_windplot", height = "300px")),
                               fluidRow(column(2, br(), br(), htmlOutput("rtwind")))),
                      fluidRow(column(10, plotlyOutput("rt_solarplot", height = "280px")),
                               fluidRow(column(2, br(), br(), htmlOutput("rtsolar"))))
                      )
             )
          )
        ),

  tabPanel(HTML("<h4>About</h4>"),
           fluidRow(
             column(12, offset = 1, htmlOutput("about"))
             )
           )
)
