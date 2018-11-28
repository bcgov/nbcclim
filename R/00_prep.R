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


library(tidyverse)

updates <- dir("../data/wxstn/WxUpdates/csv/", pattern = "csv")

for (i in 18:length(updates)) {
  df <- read_csv(paste0("../data/wxstn/WxUpdates/csv/", updates[i]), na = "NA")

  df_out <- df %>%
    select(grep("Day|Date|Time|Rain|rain|Pressure|pressure|Temp|temp|RH|Rh|rh|Dew|dew|Wind.Speed|Wind.speed|wind.speed|Gust|gust|Wind.Direction|Wind.direction|wind.direction|Solar|solar",
                colnames(df))) %>%
    rename("Rain_sum" = names(df[grep("Rain|rain", colnames(df))]),
           "Temp_avg" = names(df[grep("Temp|temp", colnames(df))]),
           "GS_max" = names(df[grep("Gust|gust", colnames(df))])) %>%
    group_by(Day) %>%
    mutate(Rain_sum = sum(Rain_sum, na.rm = TRUE),
           Temp_max = max(Temp_avg, na.rm = TRUE),
           Temp_min = min(Temp_avg, na.rm = TRUE),
           GS_max = max(GS_max, na.rm = TRUE)) %>%
    gather(key = "sensor", value = "value", -grep("Date|Time", names(df)), -Day) %>%
    filter(complete.cases(value) & !is.infinite(value)) %>%
    group_by(Day, sensor) %>%
    add_tally() %>%
    filter(n == 24) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    spread("sensor", "value") %>%
    rename("Date" = "Day",
           "Pressure_avg" = names(df[grep("Pressure|pressure", colnames(df))]),
           "RH_avg" = names(df[grep("RH|Rh|rh", colnames(df))]),
           "DP_avg" = names(df[grep("Dew|dew", colnames(df))]),
           "WS_avg" = names(df[grep("Wind.Speed|Wind.speed|wind.speed", colnames(df))]),
           "WD_avg" = names(df[grep("Wind.Direction|Wind.direction|wind.direction", colnames(df))]),
           "SR_avg" = names(df[grep("Solar|solar", colnames(df))])
           ) %>%
    select(Date, Rain_sum, Pressure_avg, Temp_max, Temp_min, Temp_avg, RH_avg,
           DP_avg, WS_avg, GS_max, WD_avg, SR_avg)

  print(summary(df_out))
  print(head(df_out))

  # write_csv(df_out, paste0("../data/wxstn/WxUpdates/csv/summaries/", updates[i]))

}


