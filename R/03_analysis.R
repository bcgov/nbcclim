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


library(dplyr) # for data analysis
library(reshape2) # for long dataframes

## classifying months to summer and winter climate seasons
wxstn_df$Season <- NA
wxstn_df[wxstn_df$months == "October" | wxstn_df$months == "November" | wxstn_df$months == "December"| wxstn_df$months == "January" |
           wxstn_df$months == "February" | wxstn_df$months == "March"| wxstn_df$months == "April" , "Season"] <- "Winter"
wxstn_df[wxstn_df$months == "May" | wxstn_df$months == "June"| wxstn_df$months == "July" | wxstn_df$months == "August" | wxstn_df$months == "September", "Season"] <- "Summer"

## calculating month average insolation
wxstn_df <- wxstn_df %>%
  group_by(Site, months) %>%
  mutate(monthly_inso = mean(SR_avg, na.rm = TRUE))

wxstn_df <- wxstn_df %>%
  group_by(Site, years, months) %>%
  mutate(inso = mean(SR_avg, na.rm = TRUE))

## selecting variables needed for long dataframe
df <- select(wxstn_df, c("Site", "Date", "years", "months", "dates", "Rain_sum", "Pressure_avg", "Temp_avg", "RH_avg", "DP_avg"))

## converting table to long format for ggplots
df_long <- melt(df, id.vars = c("Site", "Date", "dates", "months", "years"))
levels(df_long$variable) <- c("Precipitation (mm)", "Pressure (mb)", "Temperature (degree C)", "Relative Humidity (%)", "Dew Point (degree C)")

## converting wind speed from m/s to km/h
wind_df$WS <- wind_df$WS * 3.6

## classifying months to summer and winter climate seasons
wind_df$Season <- NA
wind_df[wxstn_df$months == "October" | wxstn_df$months == "November" | wxstn_df$months == "December"| wxstn_df$months == "January" |
           wxstn_df$months == "February" | wxstn_df$months == "March"| wxstn_df$months == "April" , "Season"] <- "Winter"
wind_df[wxstn_df$months == "May" | wxstn_df$months == "June"| wxstn_df$months == "July" | wxstn_df$months == "August" | wxstn_df$months == "September", "Season"] <- "Summer"

## classifying wind speed into categories for windrose plot
wind_df$WS <- cut(wind_df$WS, c(-Inf, 3, 6, 9, Inf))
levels(wind_df$WS) <- rev(levels(wind_df$WS))
