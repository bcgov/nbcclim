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
library(RColorBrewer)
library(scales) # for percentage axis label
library(leaflet)
library(plotly) # for interactive plots
library(DT) # for rendering data tables
library(shiny)


## Shiny user interface

ui <- navbarPage(
  theme = "css/bcgov.css", title = "Northern BC Climate",

  tabPanel(HTML("Long-term Records"),
    fluidRow(
      column(4, offset = 1, br(),
             leafletOutput("wsmap", height = "500px"),
             htmlOutput("caption"),
             htmlOutput("statsum"),
             br(),
             selectInput(inputId = "selected_site",
                         label = "Download Station Data",
                         choices = c("All stations", levels(wxstn_sites$Site))),
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

  tabPanel(HTML("Statistics"),
    selectInput("sum_site", "Site", wxstn_sites$Site),
    selectInput("sum_tbl", "Summary type", c("Annual", "Monthly all years", "Monthly per year", "Seasonal", "Growing season")),
    dataTableOutput("table"),
    htmlOutput("caveat"),
    downloadButton("exportstats", "Export")
    ),

  tabPanel(HTML("Real-time Data"),
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

  tabPanel(HTML("About"),
           fluidRow(
             column(10, offset = 1, htmlOutput("about"))
             )
           ),

  tags$head(tags$script(src="js/bcgov.js")),
  tags$head(tags$link(rel="shortcut icon", href="/images/favicon.ico")),
  footer = HTML('<div id="footer">
                <img src="/images/back-to-top.png" alt="Back to top" title="Back to top" class="back-to-top footer-overlap" style="bottom: 10px; display: inline;">
                <div id="footerWrapper">
                <div id="footerAdminSection">
                <div id="footerAdminLinksContainer" class="container">
                <div id="footerAdminLinks" class="row">
                <ul class="inline">
                <li data-order="0">
                <a href="//www2.gov.bc.ca/gov/content/home/disclaimer" target="_self">Disclaimer</a>
                </li>
                <li data-order="1">
                <a href="//www2.gov.bc.ca/gov/content/home/privacy" target="_self">Privacy</a>
                </li>
                <li data-order="2">
                <a href="//www2.gov.bc.ca/gov/content/home/accessibility" target="_self">Accessibility</a>
                </li>
                <li data-order="3">
                <a href="//www2.gov.bc.ca/gov/content/home/copyright" target="_self">Copyright</a>
                </li>
                </ul>
                </div>
                </div>
                </div>
                </div>
                </div>')
)

