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

# The data are collected from Northern B.C. climate research stations
library(tidyverse)
library(glue)

YEAR = year(today())

## concat new data with full dataset
wxstn <- read.csv("r/shiny/data/wxstn_df.csv") %>%
  select(Site, Longitude,	Latitude, Elevation, Date, Rain_sum, Pressure_avg, Temp_max, Temp_min, Temp_avg, RH_avg,
         DP_avg, WS_avg, GS_max, WD_avg, SR_avg, WC_avg_5cm, WC_avg_15cm, WC_avg_30cm, ST_avg, W_avg, SD_avg) %>%
  mutate(Date = as.Date(Date))

##
## QC !!
##
## validate lat long elev are numeric
is.numeric(wxstn$Latitude)
is.numeric(wxstn$Longitude)
is.numeric(wxstn$Elevation)


## validate there is no NA in the wxstn Site
print(unique(wxstn$Site))


## reading in updated files with new wind records
wx_updated <- dir("data/processed", pattern = "_wx.csv", full.names = TRUE)

## update wxstn df
for (i in 1:length(wx_updated)) {
  df <- read_csv(wx_updated[i]) %>%
    rename("Date" = "Day") %>%
    mutate(Date = as.Date(Date))

  site_name <- df$Site[1]

  ## filter out calculated variables from orig wxstn of the matching site
  orig_wx <- wxstn %>%
    filter(Site == site_name)

  ## join updated and orig tables
  if (nrow(orig_wx) > 0) {
    print(paste("----------------------- Data exist in wxstn for", site_name))

    print(range(df$Date))
    print(range(orig_wx$Date))
    print(dim(wxstn))

    ## check to see if the new data have old dates inclusive and replace old data
    if (((min(range(df$Date)) - 1) == min(range(orig_wx$Date))) |
        min(range(df$Date)) == min(range(orig_wx$Date))) {
      print(paste("------------- Truncating original wxstn data for", site_name))
      print(head(wxstn %>%
                   filter(Site == site_name)))

      ## keep the rest of the sites' data from wxstn, then append new ones
      wxstn <- wxstn %>%
        filter(Site != site_name)
    }
  }

  print(paste("----- Concatenating wxstn data with new data from", site_name))

  print(dim(orig_wx))
  print(dim(wxstn))

  wxstn <- plyr::join_all(list(wxstn, df), type = 'full')


  print(paste("------------------ Orig row count for this site:", nrow(orig_wx)))
  print(paste("------------------ Final row count for this site:", nrow(wxstn %>%
                                                             filter(Site == site_name))))

}


wxstn <- wxstn %>%
  arrange((Site))

## join site coordinates
write.csv(wxstn, glue("data/wxstn_{YEAR}.csv"), row.names = FALSE)

## reading in hourly weather records
hourly <- dir("data/processed/", pattern = "wind.csv", full.names = TRUE)

for(i in hourly) {
  wind_ls <- purrr::map(hourly, read.csv)
}

## merging all hourly dataframes for wind plots
hourly_df <- plyr::join_all(wind_ls, type = "full")

## only keeping relevant wind columns
wind_df <- hourly_df[, c("Site", "Day", "WS", "WD")]

## deleting NAs in Dates that do not contain any wind records
wind_df <- wind_df[complete.cases(wind_df$Day), ]

write.csv(wind_df, glue("data/hourly_{YEAR}.csv"), row.names = FALSE)

