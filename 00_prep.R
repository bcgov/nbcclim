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

# Stand alone script for updating weather station data
# formatting and outputting for next step analyses for the app

library(tidyverse)
library(lubridate)
library(glue)

if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}

new_data_dir <- "data/2021-2022 data for Jane/"

## station list to be updated
## reading in updated files with new wind records
updates <- dir(new_data_dir, pattern = "csv", full.names = TRUE)

## station lat long info, rename stations according to update csvs
## left is Vanesssa's csv reference, right is raw data's station names
sites <- readxl::read_excel("data/NBCClimateStns_VF.xlsx") %>%
  mutate(station_name = case_when(station_name == "BarrenWx" ~ "Barren21_22",
                                   station_name == "BednestiWx" ~ "Bednesti Tamarac21_22",
                                   station_name == "BlackhawkWx" ~ "Blackhawk21_22",
                                   station_name == "BoulderWx" ~ "BoulderCr21_22",
                                   station_name == "BowronPit" ~ "Bowron Pit21_22_extra column",
                                   station_name == "BulkleyWx" ~ "Bulkley PGTIS 1_21_22",
                                   station_name == "Canoe Mountain Stn" ~ "Canoe21_22",
                                   station_name == "Chapman" ~ "Chapman21_22",
                                   station_name == "ChiefLakeWx" ~ "ChiefLk21_22",
                                   station_name == "CoalmineWx" ~ "Coalmine21_22",
                                   station_name == "CPFWx" ~ "CPF PGTIS 3_21_22",
                                   station_name == "CrystalWx" ~ "CrystalLk21_22",
                                   station_name == "DunsterWx" ~ "Dunster21_22",
                                   station_name == "EndakoWx" ~ "Endako21_22",
                                   station_name == "GeorgeWx" ~ "George21_22",
                                   station_name == "GunnelWx" ~ "Gunnel21_22",
                                   station_name == "HourglassWx" ~ "Hourglass21_22",
                                   station_name == "Hudson Bay Mtn2" ~ "HudsonBayMtn2_21_22",
                                   station_name == "Kluskus" ~ "Kluskus21_22",
                                   station_name == "MacJxnWx" ~ "MacJxn21_22",
                                   station_name == "McDonnellWx" ~ "McDonnel21_22",
                                   station_name == "MiddleforkWx" ~ "Middlefork21_22",
                                   station_name == "NondaWx" ~ "NondaWx21_22",
                                   station_name == "North Fraser" ~ "North Fraser21_22",
                                   station_name == "Stone Creek" ~ "Stone Creek21_22",
                                   station_name == "PinkWx" ~ "PinkMtnWx21_22_extra column",
                                   station_name == "SaxtonWx" ~ "SaxtonLakeWx21_22",
                                   station_name == "SumWxCC" ~ "Sunbeam21_22",
                                   station_name == "ThompsonWx" ~ "Thompson21_22",
                                   station_name == "Willow-BowronWx" ~ "WillowBowron PGTIS 2_21_22",
                                   TRUE ~ station_name
                                   ))

## adding new records to originals, then output to directory
for (i in 1:length(updates)) {
  print(i)
  df <- read.csv(updates[i], check.names = FALSE)
  names(df)[1] <- "Date"
  names(df)[2] <- "Day"

  fname <- str_match(basename(updates[i]), "(.*)\\..*$")[,2]


  ## look for weird names that cannot be detected by the grep() function below
  print(glue::glue("-------------------------------------------- processing {updates[i]}"))
  print(glue("Data range: {range(df$Date)}"))
  print("Data shape (ncol, nrow) and unique datetimes: ")
  print(paste(ncol(df), nrow(df), length(unique(df$Date))))

  ## keep the first 11 columns of interest
  df <- df[, c(1:11)]
  df$key = seq(1, nrow(df), 1)
  print(names(df))

  ## take the first few words before the comma separated names
  ## take the first few words before the comma separated names
  for (i in 1:length(names(df))) {
    names(df)[i] <- strsplit(names(df)[i], ",")[[1]][1]
  }

  ## add column if it doesn't exist / all values got filtered out
  `%nin%` <- Negate(`%in%`)

  for (col in c('Rain', 'Pressure', 'Temp', 'RH', 'DewPt', 'Wind Speed',
                'Gust Speed', 'Wind Direction', 'Solar Radiation')) {
    if (col %nin% names(df)) {
      df[, col] <- NA
      print(glue("WARNING - COLUMN '{col}' NOT FOUND IN DF-----"))
    }
  }


  ## correct 'NAN' character to be numbers
  ## could not use tidyverse dynamic variables to index thru cols
  df$Rain[toupper(df$Rain) == "NAN"] <- NA
  df$Pressure[toupper(df$Pressure) == "NAN"] <- NA
  df$Temp[toupper(df$Temp) == "NAN"] <- NA
  df$RH[toupper(df$RH) == "NAN"] <- NA
  df$DewPt[toupper(df$DewPt) == "NAN"] <- NA
  df$`Wind SpeedDewPt`[toupper(df$`Wind Speed`) == "NAN"] <- NA
  df$`Gust Speed`[toupper(df$`Gust Speed`) == "NAN"] <- NA
  df$`Wind Direction`[toupper(df$`Wind Direction`) == "NAN"] <- NA


  ## do a count and filter out daily records that are less than 12 observations
  ## for any variable, if we have more than 12 records
  ## then calculate stats (min, max and mean)

  ## get a QA df to join with full df for qualified datetime records
  df_qa <- df %>%
    select(Date, Day, key, Rain, Pressure, Temp, RH, DewPt, `Wind Speed`, `Gust Speed`,
           `Wind Direction`, `Solar Radiation`) %>%
    gather(key = 'var', value = 'val', -Date, -Day, -key) %>%
    group_by(Day, var) %>%
    mutate(s = sum(!is.na(val))) %>%
    filter(s > 12)

  df_qa <- tibble(Date = unique(df_qa$Date))
  df_wx <- left_join(df_qa, df, by = "Date")


  ## hourly wind columns
  ## cleaning input updated dataframe
  df_wind <- df_wx %>%
    select(Date, Day, `Wind Speed`, `Wind Direction`) %>%
    rename("WS" = `Wind Speed`,
           "WD" =  `Wind Direction`)
  df_wind$Site <- as.character(sites[sites$station_name == fname, "nbcclim_label"])

  ## if summing all NAs, return NA instead of 0
  suma = function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)

  df_wx <- df_wx %>%
    group_by(Day) %>%
    summarise(Rain_sum = round(suma(as.numeric(Rain)), 2),
              Pressure_avg = round(mean(as.numeric(Pressure), na.rm = TRUE), 2),
              Temp_max = round(max(as.numeric(Temp), na.rm = TRUE), 2),
              Temp_min = round(min(as.numeric(Temp), na.rm = TRUE), 2),
              Temp_avg = round(mean(as.numeric(Temp), na.rm = TRUE), 2),
              RH_avg = round(mean(as.numeric(RH), na.rm = TRUE), 2),
              DP_avg = round(mean(as.numeric(DewPt), na.rm = TRUE), 2),
              WS_avg = round(mean(as.numeric(`Wind Speed`), na.rm = TRUE), 2),
              GS_max = round(max(as.numeric(`Gust Speed`), na.rm = TRUE), 2),
              WD_avg = round(mean(as.numeric(`Wind Direction`), na.rm = TRUE), 2),
              SR_avg = round(mean(as.numeric(`Solar Radiation`), na.rm = TRUE), 2))

  df_wx$Site <- as.character(sites[sites$station_name == fname, "nbcclim_label"])
  df_wx$Longitude <- as.character(sites[sites$station_name == fname, "lon"])
  df_wx$Latitude <- as.character(sites[sites$station_name == fname, "lat"])
  df_wx$Elevation <- as.character(sites[sites$station_name == fname, "elev"])

  ## check if the colnames are correctly renamed. Compare among two dataframes for number of
  ## NAs and dimensions
  print("---------------------------------------------------- summary of wx df")
  print(summary(df_wx))
  print(head(df_wx))

  ## look for number of NAs, if they are consistent with inputs'. Look for start and end date
  ## and see if everything updated. Also detect for mutating rows (shouldn't be any for updates)
  print("-------------------------------------------------- summary of wind df")
  print(summary(df_wind))
  print(head(df_wind))

  # format fname to be correct
  fname <- strsplit(fname, "_21_22")[[1]][1]

  # do a second pattern split if an extra underscore is contained
  fname <- strsplit(fname, "21_22")[[1]][1]



  ## check to see if all is numeric
  df_wx <- df_wx %>%
    mutate(Longitude = as.numeric(Longitude),
           Latitude = as.numeric(Latitude),
           Elvation = as.numeric(Elevation))


  ## stop and examine outputs
  # browser()
  write_csv(df_wx, paste0("data/processed/", fname, "_wx.csv"))
  write_csv(df_wind, paste0("data/processed/", fname, "_wind.csv"))
  print(glue::glue("-------------------------------------------- Finished processing {fname}"))

  # browser()

}


