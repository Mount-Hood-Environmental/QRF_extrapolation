# Author: Mark Roes, Bryce Oldemeyer
# Purpose: Update existing RF extrapolations with QRF predictions when new DASH habitat data is available
# Created: 7/14/2020
# Last Modified:
# Notes: 

#-----------------------------------------------------------------
#libraries
library(tidyverse)
library(sf)
library(quantregForest)
library(fuzzyjoin)

#-----------------------------------------------------------------
# load model fits and DASH data to be added
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[1]

cov_choice = c("Reduced","CovLW","Dash","No_elev")[4]

in_path = 'S:/main/data/qrf/gitrepo_data/input/'
dash_path = 'S:/main/data/habitat/DASH/prepped/'
mod_path = 'S:/main/data/qrf/gitrepo_data/output/modelFit/'

#load imputation function
source("R/impute_missing_data.r")

#load QRF modelfit and preds
load(paste0(mod_path, mod_choice,'_',cov_choice, '.rda'))

#load RF extrapolation modelfit and preds
#load(paste0(mod_path,'extrap_200rch_RF_', mod_choice,'_',cov_choice, '.rda'))




#load in new habitat data
#hab_new = st_read(paste0(dash_path,''))
hab_rds = read_rds(paste0(dash_path,"dash_hr_18-21.rds")) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  mutate(point = st_centroid(geometry)) %>%
  mutate(LWFreq_Wet = (n_lwd_wetted/hr_length_m)*100,
         CU_Freq = cu_freq,
         DpthThlwg_Avg = hr_thlwg_dpth_avg_m,
         FishCovSome = fish_cov_total,
         FstNT_Freq = run_freq,
         FstTurb_Freq = fst_turb_freq,
         PoolResidDpth = hr_avg_resid_pool_dpth_m,
         Sin = hr_sin_cl,
         SubEstBldr = sub_est_bldr,
         SubEstCbl = sub_est_cbl,
         SubEstGrvl = sub_est_gravl,
         SubEstSandFines = sub_est_sand_fines,
         WetBraid = hr_braidedness,
         id = 1:n())


# #Deal with fh data (only need geometry and avg_aug_temp)
# if(mod_choice == "juv_summer") {
#   load(paste0(in_path,'fh_sum_champ_2017_0522.rda'))
#   fh = fh_sum_champ_2017 %>%
#     st_as_sf(coords = c("Lon","Lat"),
#              crs = "+proj=longlat +datum=WGS84") %>%
#     select(avg_aug_temp)
# 
# } else if (mod_choice == "redds") {
#   load(paste0(in_path,'fh_redds_champ_2017_0522.rda'))
#   fh = fh_redds_champ_2017 %>%
#     select(Species, Site, Watershed, LON_DD, LAT_DD, 
#            Lgth_Wet, Area_Wet)
# } else if (mod_choice == "juv_winter") {
#   load(paste0(in_path,'fh_win_champ_2017_0522.rda'))
#   fh = fh_win_champ_2017 %>%
#     select(Species, Site, Watershed, LON_DD, LAT_DD, 
#            VisitID,
#            AreaTotal, Lgth_Wet, Area_Wet)
# }

#-----------------------------------------------------------------
# Make predictions with new data
#-----------------------------------------------------------------
# extrap_hab = pred_hab_df %>%
#   st_as_sf(coords = c("LON_DD","LAT_DD"),
#             crs = "+proj=longlat +datum=WGS84")
  
load(paste0(in_path,"rch_200.rda")) 

temps = rch_200 %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  select(S2_02_11, end_elev, region, p_accum, slope) %>%
  rename(avg_aug_temp = S2_02_11)

impute_covars = c("Sin", "end_elev", "region", "slope")
pred_quant = 0.9

#This chunk takes a minute
new_preds = hab_rds %>%
  st_join(temps,
          left = T, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(region = as.factor(region)) %>%
  impute_missing_data(covars = c("avg_aug_temp", "PoolResidDpth"),
                    impute_vars = impute_covars,
                    method = 'missForest') %>%
  select(site_name, year, id, unique(pull(sel_hab_mets, Metric)), hr_length_m) %>%
  mutate(WetBraid = ifelse(WetBraid > 2, 2, WetBraid)) %>%
  mutate(chnk_per_m = predict(qrf_mods[['Chinook']],
                              newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                              what = pred_quant),
         chnk_per_m = exp(chnk_per_m) - dens_offset) %>%
  mutate(sthd_per_m = predict(qrf_mods[['Steelhead']],
                              newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                              what = pred_quant),
         sthd_per_m = exp(sthd_per_m) - dens_offset) %>%
  mutate(chnk_cap = chnk_per_m * hr_length_m,
         sthd_cap = sthd_per_m * hr_length_m) %>%
  left_join(hab_rds %>% select(id, geometry)) %>%
  st_as_sf()

save(new_preds,
     file = paste0(out_path,'modelFit/extrap_200rch_RF_', mod_choice,'_',cov_choice, '.rda')

#-----------------------------------------------------------------
# Replace old (no-champ) extraps
#-----------------------------------------------------------------

