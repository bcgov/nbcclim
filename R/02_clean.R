# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(tidyverse)
library(glue)

`%nin%` = Negate(`%in%`)
YEAR = year(today())

wxstn_df = read.csv(glue("data/wxstn_{YEAR}.csv"))

# Every year when updating:
# if an old station name has been changed, remove all records and replace with
# new station's data.
# remove stations as needed in this step too
wind_prev <- read_rds(glue("data/wind_df_{YEAR - 1}.rds")) |>
  filter(Site != "Bednesti Tamarac", Site != "McDonnel")

wind_df <- read_rds(glue("data/hourly_{YEAR}.rds"))


if (nrow(wxstn_df |> filter(Site == "character(0)")) > 1) {
  wxstn_df |> filter(Site == "character(0)") |>
    write_csv("No_site_data.csv")

  print("REMOVING SITE WITH NO NAME FROM wxstn_df")

  wxstn_df <- wxstn_df |>
    filter(Site != "character(0)")
}


## converting Date column as date class
wxstn_df$Date <- as.Date(wxstn_df$Date)
wxstn_df$Date <- as.Date(wxstn_df$Date, "%Y-%m-%d")

## formatting Month column
wxstn_df$Month <- substr(wxstn_df$Date, 1, 7)

## adding years, months, and dates columns for indexing later
wxstn_df$years <- substr(wxstn_df$Date, 1, 4)

wxstn_df$months <- months(wxstn_df$Date, abbreviate = TRUE)
wxstn_df$months <- factor(wxstn_df$months,
                          levels = c(
                            "Jan", "Feb", "Mar",
                            "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec")
                          )
wxstn_df$dates <- substr(wxstn_df$Date, 6, 10)
wxstn_df$dates <- as.Date(wxstn_df$dates, "%m-%d")

## stations data frame
## real time stations
rt <- subset(wxstn_df,  Site == "Blackhawk" | Site == "Bowron Pit" | Site == "Canoe" |
               Site == "Gunnel" | Site == "Hourglass" | Site == "Hudson Bay Mountain" |
               Site == "Sunbeam" | Site == "Nonda" | Site == "Pink Mountain",
             select = c(Site, Longitude, Latitude, Elevation))
rt <- subset(rt, !duplicated(rt$Site))

## long-term weather stations
wxstn_sites <- subset(wxstn_df,
                      select = c(Site, Longitude, Latitude, Elevation)
                      )
wxstn_sites <- subset(wxstn_sites, !duplicated(wxstn_sites$Site))

wxstn_df |>
  write_rds(glue("data/wxstn_df_{YEAR}.rds"))

wxstn_sites |>
  write.csv("data/wxstn_sites.csv", row.names = FALSE)

# hourly wind data - wind_df_{YYYY} is the file combining all of hourly wind
# data from the updates. Join updates with previous wind data by adding the
# sites that are not a part of the updates from the current year.
# join all year's hourly wind data

wind_sites <- wind_df |> pull(Site) |> unique()

wind_prev <- wind_prev |>
  filter(Site %nin% wind_sites, Site != "character(0)")

wind_df$Day <- as.Date(wind_df$Day, "%Y-%m-%d")
wind_df <- full_join(wind_df, wind_prev, by = c(colnames(wind_df)))

wind_df |>
  write_rds(glue("data/wind_df_{YEAR}.rds"))

write.csv(rt, "data/real_time_stn.csv", row.names = FALSE)
# remove hourly updates
unlink(glue("data/hourly_{YEAR}.rds"))

