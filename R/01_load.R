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


library(plyr) # for joining all dataframes

# Data cleanning
## reading in daily weather records from individual stations
# daily <- list.files(path = "G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv", pattern="*.csv")
# for(i in daily) {
#   assign(unlist(strsplit(i, "[.]"))[1], read.csv((paste0("G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/", i))))
# }
#
# ## merging all dataframes into one
# wxstn_df <- join_all(list(Blackhawk, BowronPitWx, BoulderCr, Bulkley_1113677, Canoe, CoalmineWx, CPF_1113682, CrystalLk_1305871, Dunster10099920, Endako_11597013,
#                           George_1177893, `Gunnel_1-2combined`, Gunnel3, Hourglass_9702605, HudsonBayMtn, Kluskus_10424986, MacJxn_2289305, McbridePk,
#                           MiddleforkWx, NondaWx, PGTIS_AMAT, PinkMtnWx, SaxtonLakeWx, Thompson, WillowBowron_1095439), type = "full")
#
# # ## formatting Month column
# wxstn_df$Month <- substr(wxstn_df$Date, 1, 7)
#
# write.csv(wxstn_df, "G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/wxstn.csv", row.names = FALSE)
#
# ## reading in hourly weather records
# rm(list=ls())
# hourly <- list.files(path = "G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/hourly/", pattern="*.csv")
# for(i in hourly) {
#   assign(unlist(strsplit(i, "[.]"))[1], read.csv((paste0("G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/hourly/", i))))
# }
#
# ## merging all hourly dataframes for wind plots
# hourly_df <- join_all(list(Blackhawk, BoulderCr, Bulkley_1113677, Canoe, CPF_1113682, CrystalLk_1305871, Dunster10099920, Endako_11597013,
#                           George_1177893, `Gunnel_1-2combined`, Hourglass_9702605, Kluskus_10424986, MacJxn_2289305, McbridePk,
#                           MiddleforkWx, NondaWx, PinkMtnWx, SaxtonLakeWx, Thompson, WillowBowron_1095439), type = "full")
#
# ## only keeping relevant wind columns
# wind_df <- hourly_df[, c("Site", "Date", "Day", "WS", "WD")]

write.csv(wind_df, "G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/hourly/hourly.csv", row.names = FALSE)

rm(list=ls())

wxstn_df <- read.csv("G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/wxstn.csv")
wind_df <- read.csv("G:/!Workgrp/Research/JWang/ClimateData/WxStns/csv/hourly/hourly.csv")
