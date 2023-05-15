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

new_data_dir <- "data/VFoordCleanedHourly_All/"

## station list to be updated
## reading in updated files with new wind records
updates <- dir(new_data_dir, pattern = "csv", full.names = TRUE)

## station lat long info, rename stations according to update csvs
## left is Vanesssa's csv reference, right is raw data's station names
sites <- readxl::read_excel("data/NBCClimateStns_VF.xlsx") %>%
  mutate(station_name = case_when(station_name == "BarrenWx" ~ "Barren",
                                   station_name == "BednestiWx" ~ "BednestiTamarac",
                                   station_name == "BlackhawkWx" ~ "Blackhawk",
                                   station_name == "BoulderWx" ~ "BoulderCr",
                                   station_name == "BowronPit" ~ "BowronPit",
                                   station_name == "BulkleyWx" ~ "BulkleyPGTIS1",
                                   station_name == "Canoe Mountain Stn" ~ "Canoe",
                                   station_name == "Chapman" ~ "Chapman",
                                   station_name == "ChiefLakeWx" ~ "ChiefLk",
                                   station_name == "CoalmineWx" ~ "Coalmine",
                                   station_name == "CPFWx" ~ "CPFPGTIS3",
                                   station_name == "CrystalWx" ~ "CrystalLk",
                                   station_name == "DunsterWx" ~ "Dunster",
                                   station_name == "EndakoWx" ~ "Endako",
                                   station_name == "GeorgeWx" ~ "George",
                                   station_name == "GunnelWx" ~ "Gunnel",
                                   station_name == "HourglassWx" ~ "Hourglass",
                                   station_name == "Hudson Bay Mtn2" ~ "HudsonBayMtn2",
                                   station_name == "Kluskus" ~ "Kluskus",
                                   station_name == "MacJxnWx" ~ "MacJxn",
                                   station_name == "McDonnellWx" ~ "McDonnel",
                                   station_name == "MiddleforkWx" ~ "Middlefork",
                                   station_name == "NondaWx" ~ "NondaWx",
                                   station_name == "North Fraser" ~ "NorthFraser",
                                   station_name == "Stone Creek" ~ "StoneCreek",
                                   station_name == "PinkWx" ~ "PinkMtnWx",
                                   station_name == "SaxtonWx" ~ "SaxtonLakeWx",
                                   station_name == "SumWxCC" ~ "Sunbeam",
                                   station_name == "ThompsonWx" ~ "Thompson",
                                   station_name == "Willow-BowronWx" ~ "WillowBowronPGTIS2",
                                   TRUE ~ station_name
                                   ))

## column variables that may or may not be existent
optional_cols = c("Water Content 15cm", "Water Content 5cm", "Water Content 30cm", "Soil Temp",
                  "Wetness", "Snow depth")
optional_col_lookup <- c(SD_avg = "Snow depth",
                         WC_avg_5cm = "Water Content 5cm",
                         WC_avg_15cm = "Water Content 15cm",
                         WC_avg_30cm = "Water Content 30cm",
                         W_avg = "Wetness",
                         ST_avg  = "Soil Temp")

## adding new records to originals, then output to directory
for (i in 1:length(updates)) {
  print(i)
  df <- read.csv(updates[i], check.names = FALSE, encoding = "UTF-8")
  names(df)[1] <- "Date"
  names(df)[2] <- "Day"

  fname <- str_match(basename(updates[i]), "(.*)\\..*$")[,2]


  ## look for weird names that cannot be detected by the grep() function below
  print(glue::glue("-------------------------------------------- processing {updates[i]}"))
  # print(glue("Data range: {range(df$Date)}"))
  print("Data shape (ncol, nrow) and unique datetimes: ")
  print(paste(ncol(df), nrow(df), length(unique(df$Date))))
  print(names(df))


  ## keep the first 11 columns of interest as well as soil, water content, wetness and snow depth

  df <- df[, c(1:11,
               grep("^Water Content, m.*5 cm$", names(df)), # Water Content, m³/m³ 5 cm and 15 cm
               grep("^Water Content, m.*30 cm$", names(df)), # Water Content, m³/m³ 30 cm
               grep("^Soil Temp.*C$", names(df)), # Soil Temp, °C
               grep("Wetness", names(df)), # Wetness, %
               grep("Snow depth, cm", names(df)))] # Snow depth
  df$key = seq(1, nrow(df), 1)
  print(names(df))

  `%nin%` <- Negate(`%in%`)

  ## take the first few words before the comma separated names
  ## take the first few words before the comma separated names
  for (i in 1:length(names(df))) {
    if (str_detect(names(df)[i], "Water")) {
      if (str_detect(names(df)[i], "15 cm")) {

        names(df)[i] <- paste(strsplit(names(df)[i], ",")[[1]][1], "15cm")
        df$`Water Content 15cm`[toupper(df$`Water Content 15cm`) == "NAN"] <- NA

      } else if (str_detect(names(df)[i], "30 cm")) {
        names(df)[i] <- paste(strsplit(names(df)[i], ",")[[1]][1], "30cm")
        df$`Water Content 30cm`[toupper(df$`Water Content 30cm`) == "NAN"] <- NA

      } else{
        names(df)[i] <- paste(strsplit(names(df)[i], ",")[[1]][1], "5cm")
        df$`Water Content 5cm`[toupper(df$`Water Content 5cm`) == "NAN"] <- NA
      }

    } else {

      names(df)[i] <- strsplit(names(df)[i], ",")[[1]][1]
    }
  }


  # for (col in c('Rain', 'Pressure', 'Temp', 'RH', 'DewPt', 'Wind Speed',
  #               'Gust Speed', 'Wind Direction', 'Solar Radiation')) {
  #   if (col %nin% names(df)) {
  #     df[, col] <- NA
  #     print(glue("WARNING - COLUMN '{col}' NOT FOUND IN DF-----"))
  #   }
  # }

  if (length(names(df)) < 18) {
    print('Not all columns are present')
  }

  if (length(names(df)) > 18) {
    print("Wrong column lenghts present (more columns selected)")
  }


  ## correct 'NAN' character to be numbers
  ## could not use tidyverse dynamic variables to index thru cols
  df$Rain[toupper(df$Rain) == "NAN"] <- NA
  df$Pressure[toupper(df$Pressure) == "NAN"] <- NA
  df$Temp[toupper(df$Temp) == "NAN"] <- NA
  df$RH[toupper(df$RH) == "NAN"] <- NA
  df$DewPt[toupper(df$DewPt) == "NAN"] <- NA
  df$`Wind Speed`[toupper(df$`Wind Speed`) == "NAN"] <- NA
  df$`Gust Speed`[toupper(df$`Gust Speed`) == "NAN"] <- NA
  df$`Wind Direction`[toupper(df$`Wind Direction`) == "NAN"] <- NA
  df$`Solar Radiation`[toupper(df$`Solar Radiation`) == "NAN"] <- NA

  ## do a count and filter out daily records that are less than 12 observations
  ## for any variable, if we have more than 12 records
  ## then calculate stats (min, max and mean)

  ## get a QA df to join with full df for qualified datetime records
  df_qa <- df %>%
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
              SR_avg = round(mean(as.numeric(`Solar Radiation`), na.rm = TRUE), 2),
              across(any_of(optional_cols), mean, na.rm = TRUE)) %>%
    mutate(across(any_of(optional_cols), round, 2)) %>%
    rename(any_of(optional_col_lookup))


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
    mutate(Longitude = as.numeric(str_trim(Longitude, side = "both")),
           Latitude = as.numeric(str_trim(Latitude, side = "both")),
           Elevation = as.numeric(str_trim(Elevation, side = "both")))


  ## stop and examine outputs
  # browser()
  write_csv(df_wx, paste0("data/processed/", fname, "_wx.csv"))
  write_csv(df_wind, paste0("data/processed/", fname, "_wind.csv"))
  print(glue::glue("-------------------------------------------- Finished processing {fname}"))

  # browser()

}


