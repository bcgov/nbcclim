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

library(reshape2)
library(ggplot2)
library(viridis) # for colour palettes
library(RColorBrewer)
library(leaflet)
library(plotly) # for interactive plots
library(DT) # for rendering data tables
library(shiny)

wxstn_df <- read.csv("data/wxstn_df.csv")
wind_df <- read.csv("data/wind_df.csv")
rt <- read.csv("data/real_time_stn.csv")
wxstn_sites <- read.csv("data/wxstn_sites.csv")
annual_sum <- read.csv("data/annual_sum.csv")
monthly_sum <- read.csv("data/monthly_sum.csv")
month_year_sum <- read.csv("data/month_year_sum.csv")
seasonal_sum <- read.csv("data/seasonal_sum.csv")
gseason_sum <- read.csv("data/gseason_sum.csv")

wxstn_df$dates <- as.Date(wxstn_df$dates)
wxstn_df$years <- as.character(wxstn_df$years)
wxstn_df$months <- factor(wxstn_df$months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

df <- select(wxstn_df, c("Site", "Date", "years", "months", "dates", "Temp_avg", "RH_avg", "Rain_sum", "Pressure_avg"))

## converting table to long format for ggplots
df_long <- melt(df, id.vars = c("Site", "Date", "dates", "months", "years"))
levels(df_long$variable) <- c("Temperature (degree C)", "Relative Humidity (%)", "Precipitation (mm)", "Pressure (mb)")

wind_df$WS <- factor(wind_df$WS, levels = c("(9, Inf]", "(6,9]", "(3,6]", "(-Inf,3]"))
wind_df$WD <- as.factor(wind_df$WD)
icons <- awesomeIcons(icon = "circle",  markerColor = "blue", iconColor = "#ffffff", library = "fa")

## real time station columns to include
col_str <- "Date_Time|Rain|Pressure|Temperature|RH|Wind.Speed|Gust.Speed|Wind.Direction|Solar"

## define colour by year palette
pal = rep(colorRampPalette(brewer.pal(12, "Paired"))(10), 2)
