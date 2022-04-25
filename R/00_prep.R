# Copyright 2020 Province of British Columbia
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

# Stand alone script for preparing weather station data from the site ready for next steps
# Updating all wind sensor records

library(tidyverse)
library(lubridate)

if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}



## reading in updated files with new wind records
updates <- dir("raw_data", pattern = "csv", full.names = TRUE)

## station lat long info, rename stations according to update csvs
sites <- readxl::read_excel("data/NBCClimateStns_VF.xlsx") %>%
  mutate(station_name = case_when(station_name == "BarrenWx" ~ "Barren",
                                   station_name == "BednestiWx" ~ "Bednesti Tamarac",
                                   station_name == "BlackhawkWx" ~ "Blackhawk",
                                   station_name == "BoulderWx" ~ "BoulderCr",
                                   station_name == "BowronPit" ~ "Bowron Pit",
                                   station_name == "BulkleyWx" ~ "Bulkley PGTIS 1",
                                   station_name == "Canoe Mountain Stn" ~ "Canoe",
                                   station_name == "ChiefLakeWx" ~ "ChiefLk",
                                   station_name == "CoalmineWx" ~ "Coalmine",
                                   station_name == "CPFWx" ~ "CPF PGTIS 3",
                                   station_name == "CrystalWx" ~ "CrystalLk",
                                   station_name == "DunsterWx" ~ "Dunster",
                                   station_name == "EndakoWx" ~ "Endako",
                                   station_name == "GeorgeWx" ~ "George",
                                   station_name == "GunnelWx" ~ "Gunnel",
                                   station_name == "HourglassWx" ~ "Hourglass",
                                   station_name == "Hudson Bay Mtn2" ~ "HudsonBayMtn2",
                                   station_name == "MacJxnWx" ~ "MacJxn",
                                   station_name == "McDonnellWx" ~ "McDonnel",
                                   station_name == "MiddleforkWx" ~ "Middlefork",
                                   station_name == "PinkWx" ~ "PinkMtnWx",
                                   station_name == "SaxtonWx" ~ "SaxtonLakeWx",
                                   station_name == "SumWxCC" ~ "Sunbeam", ## CHECK
                                   station_name == "ThompsonWx" ~ "Thompson",
                                   station_name == "Willow-BowronWx" ~ "WillowBowron PGTIS 2",
                                   TRUE ~ station_name
                                   ))
## station list to be updated

## adding new records to originals, then output to directory
for (i in 1:length(updates)) {
  df <- read_csv(updates[i])
  names(df)[1] <- "Date"
  names(df)[2] <- "Day"

  fname <- str_match(basename(updates[i]), "(.*)\\..*$")[,2]


  ## look for weird names that cannot be detected by the grep() function below
  print(glue::glue("-------------------------------------------- processing {updates[i]}"))
  print(names(df))
  print(range(df$Date))

  ## do a count and filter out daily records that are less than 12 observations
  df$key = seq(1, nrow(df), 1)
  df_wx <- df %>%
    select(Date, Day, key, Rain, Pressure, Temp, RH, DewPt, `Wind Speed`, `Gust Speed`,
           `Wind Direction`, `Solar Radiation`) %>%
    gather(key = 'var', value = 'val', -Date, -Day, -key) %>%
    group_by(Day, var) %>%
    mutate(s = sum(!is.na(val))) %>%
    filter(s > 12) %>%
    spread(var, val) %>%
    arrange(key) %>%
    select(-s, -key) %>%
    ungroup()

  ## add column if it doesn't exist / all values got filtered out
  `%nin%` <- Negate(`%in%`)

  for (col in c('Rain', 'Pressure', 'Temp', 'RH', 'DewPt', 'Wind Speed',
                'Gust Speed', 'Wind Direction', 'Solar Radiation')) {
    if (col %nin% names(df_wx)) {
      df_wx[, col] <- NA
    }
  }

  ## hourly wind columns
  ## cleaning input updated dataframe
  df_wind <- df_wx %>%
    select(Date, Day, `Wind Speed`, `Wind Direction`) %>%
    rename("WS" = `Wind Speed`,
           "WD" =  `Wind Direction`)
  df_wind$Site <- as.character(sites[sites$station_name == fname, "station_name"])

  ## if summing all NAs, return NA instead of 0
  suma = function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)

  df_wx <- df_wx %>%
    group_by(Day) %>%
    summarise(Rain_sum = round(suma(Rain), 2),
              Pressure_avg = round(mean(Pressure, na.rm = TRUE), 2),
              Temp_max = round(max(Temp, na.rm = TRUE), 2),
              Temp_min = round(min(Temp, na.rm = TRUE), 2),
              Temp_avg = round(mean(Temp, na.rm = TRUE), 2),
              RH_avg = round(mean(RH, na.rm = TRUE), 2),
              DP_avg = round(mean(DewPt, na.rm = TRUE), 2),
              WS_avg = round(mean(`Wind Speed`, na.rm = TRUE), 2),
              GS_max = round(max(`Gust Speed`, na.rm = TRUE), 2),
              WD_avg = round(mean(`Wind Direction`, na.rm = TRUE), 2),
              SR_avg = round(mean(`Solar Radiation`, na.rm = TRUE), 2))

  df_wx$Site <- as.character(sites[sites$station_name == fname, "station_name"])
  df_wx$Longitude <- as.character(sites[sites$station_name == fname, "lon"])
  df_wx$Latitude <- as.character(sites[sites$station_name == fname, "lat"])
  df_wx$Elevation <- as.character(sites[sites$station_name == fname, "elev"])

  ## check if the colnames are correctly renamed. Compare among two dataframes for number of
  ## NAs and dimensions
  print("---------------------------------------------------- summary of wx df")
  print(summary(df_wx))
  print(head(df_wx))


  df_wx$Site <- as.character(sites[sites$station_name == fname, "station_name"])
  df_wx$Longitude <- as.character(sites[sites$station_name == fname, "lon"])
  df_wx$Latitude <- as.character(sites[sites$station_name == fname, "lat"])
  df_wx$Elevation <- as.character(sites[sites$station_name == fname, "elev"])

  ## look for number of NAs, if they are consistent with inputs'. Look for start and end date
  ## and see if everything updated. Also detect for mutating rows (shouldn't be any for updates)
  print("-------------------------------------------------- summary of wind df")
  print(summary(df_wind))
  print(head(df_wind))

  ## stop and examine outputs
  # browser()
  write_csv(df_wx, paste0("data/processed/", fname, "_wx.csv"))
  write_csv(df_wind, paste0("data/processed/", fname, "_wind.csv"))
  print(glue::glue("-------------------------------------------- Finished processing {updates[i]}"))

}


