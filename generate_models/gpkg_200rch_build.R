# Author: Bryce Oldemeyer
# Purpose: Create gpkg files from extrapolation output
# Created: 5/17/2022
# Last Modified: 
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(survey)
library(data.table)

#-----------------------------------------------------------------
# load extrapolation output & rch_200
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

cov_choice = c("","No_elev")[2]

in_path = 'S:/main/data/qrf/gitrepo_data/input/'
out_path = 'S:/main/data/qrf/gitrepo_data/output/'

load(paste0(in_path,"rch_200.rda"))

# -----------------------------------------------------------------
# Log RF extrapolation 
#load(paste0(out_path,'modelFit/extrap_200rch_RF_log_', mod_choice, '.rda'))

# Split and append each one subsequently to help speed it up

#rch_200_cap = rch_200 %>%
#  select(UniqueID, GNIS_Name, reach_leng:HUC8_code,
#         chnk, chnk_use, chnk_ESU_DPS:chnk_NWR_NAME,
#         sthd, sthd_use, sthd_ESU_DPS:sthd_NWR_NAME) %>%
#  left_join(all_preds %>%
#              select(-HUC8_code)) %>%
#  filter(reach_leng < 500)

#rch_200_cap %>%
#  mutate_at(vars(HUC6_name),
#            list(fct_explicit_na)) %>%
#  tabyl(HUC6_name) %>%
#  adorn_totals()

#rch_200_cap_split = rch_200_cap %>%
#  group_split(HUC6_name)

#for(i in 1:length(rch_200_cap_split)) {
#  cat(paste("Working on group", i, "of", length(rch_200_cap_split),
#            "with", nrow(rch_200_cap_split[[i]]), " rows\n"))
  
#  st_write(rch_200_cap_split[[i]],
#           dsn = paste0(out_path,'gpkg/Rch_Cap_RF_log_', mod_choice, '.gpkg'),
#           driver = 'GPKG',
#           append = if_else(i == 1, F, T))
#}


# ------------------------------------------------------------------

# RF extrapolation
load(paste0(out_path,'modelFit/extrap_200rch_RF_',mod_choice,"_",cov_choice,'.rda'))

# Split and append each one subsequently to help speed it up

rch_200_cap = rch_200 %>%
  select(UniqueID, GNIS_Name, reach_leng:HUC8_code,
         chnk, chnk_use, chnk_ESU_DPS:chnk_NWR_NAME,
         sthd, sthd_use, sthd_ESU_DPS:sthd_NWR_NAME) %>%
  left_join(all_preds %>%
              select(-HUC8_code)) %>%
  filter(reach_leng < 500)

rch_200_cap %>%
  mutate_at(vars(HUC6_name),
            list(fct_explicit_na)) %>%
  tabyl(HUC6_name) %>%
  adorn_totals()

rch_200_cap_split = rch_200_cap %>%
  group_split(HUC6_name)

for(i in 1:length(rch_200_cap_split)) {
  cat(paste("Working on group", i, "of", length(rch_200_cap_split),
            "with", nrow(rch_200_cap_split[[i]]), " rows\n"))
  
  st_write(rch_200_cap_split[[i]],
           dsn = paste0(out_path,'gpkg/Rch_Cap_RF_No_elev_', mod_choice, '.gpkg'),
           driver = 'GPKG',
           append = if_else(i == 1, F, T))
}

##### ----- FOR WRITING AS SHAPE FILE

for(i in 1:length(rch_200_cap_split)) {
  cat(paste("Working on group", i, "of", length(rch_200_cap_split),
            "with", nrow(rch_200_cap_split[[i]]), " rows\n"))
  
  st_write(rch_200_cap_split[[i]],
           dsn = paste0(out_path,'gpkg/Rch_Cap_RF_No_elev_', mod_choice, '.shp'),
           append = if_else(i == 1, F, T))
}
