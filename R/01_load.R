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

## concat new data with full dataset
wxstn <- read.csv("data/wxstn_df.csv") %>%
  select(Site, Longitude,	Latitude, Elevation, Date, Rain_sum, Pressure_avg, Temp_max, Temp_min, Temp_avg, RH_avg,
         DP_avg, WS_avg, GS_max, WD_avg, SR_avg) %>%
  mutate(Date = as.Date(Date))

wxstn_sites <- read.csv("data/wxstn_sites.csv")

## reading in updated files with new wind records
wx_updated <- dir("data/processed", pattern = "_wx.csv", full.names = TRUE)

## list of names according to original wxstn naming
site_dict = tibble(orig = 'BoulderCr', change_to = 'Boulder Creek') %>%
  add_row(orig = 'Bulkley PGTIS 1', change_to = 'Bulkley') %>%
  add_row(orig = 'ChiefLk', change_to = 'Chief Lake') %>%
  add_row(orig = 'CPF PGTIS 3', change_to = 'Central Plateau Finlay') %>%
  add_row(orig = 'CrystalLk', change_to = 'Crystal Lake') %>%
  add_row(orig = 'George', change_to = 'George Lake') %>%
  add_row(orig = 'HudsonBayMtn2', change_to = 'Hudson Bay Mountain') %>%
  add_row(orig = 'MacJxn', change_to = 'Mackenzie Junction') %>%
  add_row(orig = 'SaxtonLakeWx', change_to = 'Saxton Lake') %>%
  add_row(orig = 'Sunbeam', change_to = 'McBride Peak') %>%
  add_row(orig = 'Middlefork', change_to = 'Middlefork Creek') %>%
  add_row(orig = 'NondaWx', change_to = 'Nonda') %>%
  add_row(orig = 'PinkMtnWx', change_to = 'Pink Mountain') %>%
  add_row(orig = 'WillowBowron PGTIS 2', change_to = 'Willow-Bowron')


for (i in 1:length(wx_updated)) {
  df <- read_csv(wx_updated[i]) %>%
    rename("Date" = "Day") %>%
    mutate(Date = as.Date(Date))

  fname <- str_match(basename(wx_updated[i]), "(.*)\\..*$")[ , 2]
  site_name <- gsub('_wx', '', fname)

  ## get the correct site name
  if (site_name %in% site_dict$orig) {
    site_name <- as.character(site_dict[site_dict$orig == site_name, 'change_to'])
    df$Site <- site_name
  }


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

wxstn[wxstn$Site == 'McBride Peak', 'Site'] <- 'Sunbeam'
wxstn[wxstn$Site == 'Cassiat Mt', 'Site'] <- 'Cassiar Mt'

wxstn <- wxstn %>%
  arrange((Site))


## join site coordinates
write.csv(wxstn, "data/wxstn_2022.csv", row.names = FALSE)

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

write.csv(wind_df, "data/hourly.csv", row.names = FALSE)

