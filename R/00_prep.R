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

# Stand alone script for preparing weather station data from the site ready for next steps
# Updating all wind seasor records

library(tidyverse)

## reading in updated files with new wind records
updates <- dir("G:/!Workgrp/Research/JWang/WxUpdates_Hly2018/csv/", pattern = "csv")

## station list to be updated
originals <- c("Blackhawk.csv", "BoulderCr.csv", "Bulkley_1113677.csv", "Canoe.csv", "CPF_1113682.CSV",
               "CrystalLk_1305871.csv", "Dunster10099920.csv", "Endako_11597013.csv", "George_1177893.csv",
               "Hourglass_9702605.csv", "Kluskus_10424986.csv", "MacJxn_2289305.csv", "McbridePk.csv",
               "MiddleforkWx.csv", "NondaWx.csv", "PinkMtnWx.csv", "SaxtonLakeWx.csv", "Thompson.csv", "WillowBowron_1095439.csv")

## adding new records to originals, then output to directory
for (i in 1:length(updates)) {
  df <- read_csv(paste0("G:/!Workgrp/Research/JWang/WxUpdates_Hly2018/csv/", updates[i]))
  df_orig <- read_csv(paste0("G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/hourly/", originals[i]))

  print(updates[i])
  print(names(df))

  df_orig <- select(df_orig, Site, Date, Day, WS, WD)

  df_out <- df %>%
    select(grep("Date|Day|Wind.Speed|Wind.speed|wind.speed|Wind.Direction|Wind.direction|wind.direction",
                colnames(df))) %>%
    rename("Date" = names(df[grep("Date|date", colnames(df))]),
           "Day" = names(df[grep("Day|day", colnames(df))]),
           "WS" = names(df[grep("Wind.Speed|Wind.speed|wind.speed", colnames(df))]),
           "WD" = names(df[grep("Wind.Direction|Wind.direction|wind.direction", colnames(df))]))

  print("summary of cleaned df")
  print(summary(df_out))
  print(summary(df_orig))

  df_out <- full_join(df_orig, df_out)

  ## filling in all rows with Site names
  df_out$Site <- df_orig$Site[1]

  df_out <- select(df_out, Site, Date, Day, WS, WD)

  print("summary of final df")
  print(summary(df_out))
  print(head(df_out))
  print(head(df_orig))

  browser()
  write_csv(df_out, paste0("G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/hourly/updates/", updates[i]))

}


