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

library(DT)
library(dplyr) # for data analysis
library(reshape2) # for long dataframes

## daily wxstn data analysis ####
## calculating month average insolation
wxstn_df <- wxstn_df %>%
  filter(!is.na(months)) %>%
  group_by(Site, years, months) %>%
  dplyr::mutate(monthly_inso = round(mean(SR_avg, na.rm = TRUE), 2))

## selecting variables needed for long dataframe
df <- select(wxstn_df, c("Site", "Date", "years", "months", "dates", "Temp_avg", "RH_avg", "Rain_sum", "Pressure_avg"))

## converting table to long format for ggplots
df_long <- melt(df, id.vars = c("Site", "Date", "dates", "months", "years"))
levels(df_long$variable) <- c("Temperature (degree C)", "Relative Humidity (%)", "Precipitation (mm)", "Pressure (mb)")

## classifying months to 4 seasons
wxstn_df$seasons <- NA
wxstn_df[wxstn_df$months == "Dec"| wxstn_df$months == "Jan" | wxstn_df$months == "Feb", "seasons"] <- "Winter"
wxstn_df[wxstn_df$months == "Mar"| wxstn_df$months == "Apr" | wxstn_df$months == "May", "seasons"] <- "Spring"
wxstn_df[wxstn_df$months == "Jun"| wxstn_df$months == "Jul" | wxstn_df$months == "Aug", "seasons"] <- "Summer"
wxstn_df[wxstn_df$months == "Sep"| wxstn_df$months == "Oct" | wxstn_df$months == "Nov", "seasons"] <- "Fall"

## classifying months to summer and winter growing seasons
wxstn_df$gseason <- NA
wxstn_df[wxstn_df$months == "Oct" | wxstn_df$months == "Nov" | wxstn_df$months == "Dec"| wxstn_df$months == "Jan" |
           wxstn_df$months == "Feb" | wxstn_df$months == "Mar"| wxstn_df$months == "Apr" , "gseason"] <- "Winter (Oct-Apr)"
wxstn_df[wxstn_df$months == "May" | wxstn_df$months == "Jun"| wxstn_df$months == "Jul" | wxstn_df$months == "Aug" | wxstn_df$months == "Sep", "gseason"] <- "Summer (May-Sep)"


## hourly wind data analysis ####
## converting wind speed from m/s to km/h
wind_df$WS <- wind_df$WS * 3.6

## classifying wind speed and direction into categories for windrose plot
wind_df$WS <- cut(wind_df$WS, c(-Inf, 3, 6, 9, Inf))
wind_df$WS <- factor(wind_df$WS, levels = c("(9, Inf]", "(6,9]", "(3,6]", "(-Inf,3]"))
wind_df$WD <- cut(wind_df$WD, seq(-15, 375, 30), labels = c(seq(0, 330, 30), 0))

## classifying months to 4 seasons
wind_df$seasons <- NA
wind_df[wind_df$months == "Dec"| wind_df$months == "Jan" | wind_df$months == "Feb", "seasons"] <- "Winter"
wind_df[wind_df$months == "Mar"| wind_df$months == "Apr" | wind_df$months == "May", "seasons"] <- "Spring"
wind_df[wind_df$months == "Jun"| wind_df$months == "Jul" | wind_df$months == "Aug", "seasons"] <- "Summer"
wind_df[wind_df$months == "Sep"| wind_df$months == "Oct" | wind_df$months == "Nov", "seasons"] <- "Fall"

## classifying months to summer and winter growing seasons
wind_df$gseason <- NA
wind_df[wind_df$months == "Oct" | wind_df$months == "Nov" | wind_df$months == "Dec"| wind_df$months == "Jan" |
          wind_df$months == "Feb" | wind_df$months == "Mar"| wind_df$months == "Apr" , "gseason"] <- "Winter"
wind_df[wind_df$months == "May" | wind_df$months == "Jun"| wind_df$months == "Jul" | wind_df$months == "Aug" | wind_df$months == "Sep", "gseason"] <- "Summer"

## counting wind directions in each categories
wind_df <- wind_df %>%
  filter(!is.na(WS) & ! is.na(WD)) %>%
  group_by(Site, WS, gseason) %>%
  count(WD)


## statistical summaries ####
summ <- select(wxstn_df, c("Site", "months", "years", "seasons", "gseason", "Temp_avg",
                           "Temp_min", "Temp_max", "Rain_sum", "Pressure_avg", "RH_avg", "DP_avg",
                           "WS_avg", "WD_avg", "GS_max", "SR_avg"))

## convert wind direction unit for calculating mean
summ$WD_avg <- atan(sin(summ$WD_avg*pi/180)/cos(summ$WD_avg*pi/180))*(180/pi)

sum_long <- melt(summ, id.vars = c("Site", "months", "years", "seasons", "gseason"))

annual <- sum_long %>%
  group_by(Site, years, variable) %>%
  dplyr::summarise(mean = round(mean(value, na.rm = TRUE), 2),
                   max = round(max(value, na.rm = TRUE), 2),
                   min = round(min(value, na.rm = TRUE), 2),
                   obs_na = sum(is.na(value)))
annual_sum <- melt(annual, id.vars = c("Site", "years", "variable"), variable.name = "annual")
annual_sum <- dcast(annual_sum, Site + years + annual ~ variable)

## convert wind direction negative values to positive
annual_sum$WD_avg <- ifelse(!is.na(annual_sum$WD_avg) & annual_sum$WD_avg < 0, annual_sum$WD_avg + 360, annual_sum$WD_avg)

monthly <- sum_long %>%
  group_by(Site, months, variable) %>%
  dplyr::summarise(mean = round(mean(value, na.rm = TRUE), 2),
                   max = round(max(value, na.rm = TRUE), 2),
                   min = round(min(value, na.rm = TRUE), 2),
                   obs_na = sum(is.na(value)))
monthly_sum <- melt(monthly, id.vars = c("Site", "months", "variable"), variable.name = "monthly")
monthly_sum <- dcast(monthly_sum, Site + months + monthly ~ variable)
monthly_sum$WD_avg <- ifelse(!is.na(monthly_sum$WD_avg) & monthly_sum$WD_avg < 0, monthly_sum$WD_avg + 360, monthly_sum$WD_avg)


month_year <- sum_long %>%
  group_by(Site, years, months, variable) %>%
  dplyr::summarise(mean = round(mean(value, na.rm = TRUE), 2),
                   max = round(max(value, na.rm = TRUE), 2),
                   min = round(min(value, na.rm = TRUE), 2),
                   obs_na = sum(is.na(value)))
month_year_sum <- melt(month_year, id.vars = c("Site", "years", "months", "variable"), variable.name = "monthly")
month_year_sum <- dcast(month_year_sum, Site + years + months + monthly ~ variable)
month_year_sum$WD_avg <- ifelse(!is.na(month_year_sum$WD_avg) & month_year_sum$WD_avg < 0, month_year_sum$WD_avg + 360, month_year_sum$WD_avg)


seasonal <- sum_long %>%
  group_by(Site, seasons, variable) %>%
  count(value) %>%
  dplyr::summarise(mean = round(mean(value, na.rm = TRUE), 2),
                   max = round(max(value, na.rm = TRUE), 2),
                   min = round(min(value, na.rm = TRUE), 2),
                   obs_na = sum(is.na(value)))
seasonal_sum <- melt(seasonal, id.vars = c("Site", "seasons", "variable"), variable.name = "seasonal")
seasonal_sum <- dcast(seasonal_sum, Site + seasons + seasonal ~ variable)
seasonal_sum$WD_avg <- ifelse(!is.na(seasonal_sum$WD_avg) & seasonal_sum$WD_avg < 0, seasonal_sum$WD_avg + 360, seasonal_sum$WD_avg)


gseason <- sum_long %>%
  group_by(Site, gseason, variable) %>%
  dplyr::summarise(mean = round(mean(value, na.rm = TRUE), 2),
                   max = round(max(value, na.rm = TRUE), 2),
                   min = round(min(value, na.rm = TRUE), 2),
                   obs_na = sum(is.na(value)))
gseason_sum <- melt(gseason, id.vars = c("Site", "gseason", "variable"), variable.name = "grow_season")
gseason_sum <- dcast(gseason_sum, Site + gseason + grow_season ~ variable)
gseason_sum$WD_avg <- ifelse(!is.na(gseason_sum$WD_avg) & gseason_sum$WD_avg < 0, gseason_sum$WD_avg + 360, gseason_sum$WD_avg)

getwd()
## outputting cleaned and analysed tables ####
write.csv(wxstn_df, paste0(getwd(),"/nbcclim/R/shiny/data/wxstn_df.csv"), row.names = FALSE)
write.csv(wind_df, paste0(getwd(),"/nbcclim/R/shiny/data/wind_df.csv"), row.names = FALSE)
write.csv(rt, paste0(getwd(),"/nbcclim/R/shiny/data/real_time_stn.csv"), row.names = FALSE)
write.csv(wxstn_sites, paste0(getwd(), "/nbcclim/R/shiny/data/wxstn_sites.csv"), row.names = FALSE)
write.csv(annual_sum, paste0(getwd(), "/nbcclim/R/shiny/data/annual_sum.csv"), row.names = FALSE)
write.csv(monthly_sum, paste0(getwd(), "/nbcclim/R/shiny/data/monthly_sum.csv"), row.names = FALSE)
write.csv(month_year_sum, paste0(getwd(), "/nbcclim/R/shiny/data/month_year_sum.csv"), row.names = FALSE)
write.csv(seasonal_sum, paste0(getwd(), "/nbcclim/R/shiny/data/seasonal_sum.csv"), row.names = FALSE)
write.csv(gseason_sum, paste0(getwd(), "/nbcclim/R/shiny/data/gseason_sum.csv"), row.names = FALSE)
