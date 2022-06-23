# Author: Bryce Oldemeyer 
# Purpose: Compare old extrapolations to new 2022 extrapolations for 8 Upper Salmon watersheds
# Created: 5/17/2022
# Last Modified: 
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(sf)
library(here)
library(tidyverse)
library(magrittr)
library(ggmap)
library(nngeo)
library(rgdal)
#-----------------------------------------------------------------
#

in_path = 'S:/main/data/qrf/gitrepo_data/input/'
out_path = 'S:/main/data/qrf/gitrepo_data/output/'

# set default crs
WS_crs = st_crs(4326) # WGS84

#-----------------------------------------------------------------
# read in HUC12 watershed boundaries from NAS, majority of PNW
huc12_sf = st_read(paste0("S:/main/data/habitat/watershed_boundaries/WBDHU12.shp")) %>%
  st_transform(WS_crs)

# read in old extrapolations
sum_juv_sf_old_extrap = st_read("S:/main/data/qrf/extrapolations/Rch_Cap_RF_juv_summer.gpkg") %>%
  st_transform(WS_crs)
win_juv_sf_old_extrap = st_read("S:/main/data/qrf/extrapolations/Rch_Cap_RF_juv_winter.gpkg") %>%
  st_transform(WS_crs)
redds_sf_old_extrap = st_read("S:/main/data/qrf/extrapolations/Rch_Cap_RF_redds.gpkg") %>%
  st_transform(WS_crs)

# read in new 2022 RF extrapolations
sum_juv_sf_new_extrap = st_read(paste0(out_path,'gpkg/Rch_Cap_RF_Dash_juv_summer.gpkg')) %>%
  st_transform(WS_crs)
win_juv_sf_new_extrap = st_read(paste0(out_path,'gpkg/Rch_Cap_RF_Dash_juv_winter.gpkg')) %>%
  st_transform(WS_crs)
redds_sf_new_extrap = st_read(paste0(out_path,'gpkg/Rch_Cap_RF_Dash_redds.gpkg')) %>%
  st_transform(WS_crs)

# read in Upper Salmon watershed boundaries from NAS -- THE Upper_Salmon_WBD.shp FILE WAS MANUALLY CREATED BY BO AND ADDED TO THE GPKG FOLDER --
US_huc_sf = st_read(paste0(out_path,'gpkg/Upper_Salmon_WBD.shp')) %>%
  st_transform(WS_crs) %>%
  mutate(HUC8 = str_sub(HUC12, 1, 8),
         HUC10 = str_sub(HUC12, 1, 10),
         wtrshd = layer)

#ggplot(data = US_huc_sf) +  geom_sf()

# Summer parr - old extrap
US_sum_sf_old_extrap = sum_juv_sf_old_extrap %>%
  st_intersection(US_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(US_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12,
                   wtrshd),
          join = st_covered_by,
          left = F)

#ggplot(US_sum_sf_old_extrap) + geom_sf()

# Winter presmolt - old extrap
US_win_sf_old_extrap = win_juv_sf_old_extrap %>%
  st_intersection(US_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(US_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12,
                   wtrshd),
          join = st_covered_by,
          left = F)

#ggplot(US_win_sf_old_extrap) + geom_sf()

# Redds - old extrap
US_redds_sf_old_extrap = redds_sf_old_extrap %>%
  st_intersection(US_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(US_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12,
                   wtrshd),
          join = st_covered_by,
          left = F)

#ggplot(US_redds_sf_old_extrap) + geom_sf()

# -------------------------

# Summer parr - new extrap
US_sum_sf_new_extrap = sum_juv_sf_new_extrap %>%
  st_intersection(US_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(US_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12,
                   wtrshd),
          join = st_covered_by,
          left = F)

#ggplot(US_sum_sf_new_extrap) + geom_sf()

# Winter presmolt - new extrap
US_win_sf_new_extrap = win_juv_sf_new_extrap %>%
  st_intersection(US_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(US_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12,
                   wtrshd),
          join = st_covered_by,
          left = F)

#ggplot(US_win_sf_new_extrap) + geom_sf()

# Redds - new extrap
US_redds_sf_new_extrap = redds_sf_new_extrap %>%
  st_intersection(US_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(US_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12,
                   wtrshd),
          join = st_covered_by,
          left = F)

#ggplot(US_redds_sf_new_extrap) + geom_sf()

# get trimmed 200m reach layer for the Upper Salmon watersheds; contains a bunch of habitat metrics for the 200m reaches
load(paste0(in_path, 'rch_200.rda'))

# trim down 200m reach layer
US_rch_sf = rch_200 %>%
  filter(UniqueID %in% US_sum_sf_new_extrap$UniqueID) %>%
  st_transform(WS_crs)

#ggplot(US_rch_sf) + geom_sf()

# save data for this repository
save(US_sum_sf_old_extrap,
     US_win_sf_old_extrap,
     US_redds_sf_old_extrap,
     US_sum_sf_new_extrap,
     US_win_sf_new_extrap,
     US_redds_sf_new_extrap,
     file = paste0(out_path,'gpkg/extrap_compare/US_qrf_extraps_comp.rda'))

save(US_huc_sf,
     US_rch_sf,
     file = paste0(out_path,'gpkg/extrap_compare/US_spatial.rda'))

# save geopackages for use in QGIS
st_write(US_sum_sf_old_extrap,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_sum_sf_old_extrap.gpkg'),
         append = F)
st_write(US_win_sf_old_extrap,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_win_sf_old_extrap.gpkg'),
         append = F)
st_write(US_redds_sf_old_extrap,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_redds_sf_old_extrap.gpkg'),
         append = F)
st_write(US_sum_sf_new_extrap,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_sum_sf_new_extrap.gpkg'),
         append = F)
st_write(US_win_sf_new_extrap,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_win_sf_new_extrap.gpkg'),
         append = F)
st_write(US_redds_sf_new_extrap,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_redds_sf_new_extrap.gpkg'),
         append = F)
st_write(US_rch_sf,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_rch_sf.gpkg'),
         append = F)
st_write(US_huc_sf,
         dsn = paste0(out_path,'gpkg/extrap_compare/US_huc_sf.gpkg'),
         append = F)



