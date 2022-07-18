# Author: Mark Roes, Bryce Oldemeyer
# Purpose: Update existing RF extrapolations with QRF predictions when new DASH habitat data is available
# Created: 7/14/2020
# Last Modified:
# Notes: 

#-----------------------------------------------------------------
#libraries
library(tidyverse)

#-----------------------------------------------------------------
# load model fits and DASH data to be added
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[1]

cov_choice = c("Reduced","CovLW","Dash")[3]

dash_path = 'S:/main/data/qrf/gitrepo_data/input/'
mod_path = 'S:/main/data/qrf/gitrepo_data/output/modelFit/'

#load QRF modelfit and preds
load(paste0(mod_path, mod_choice,'_',cov_choice, '.rda'))

#load RF extrapolation modelfit and preds
load(paste0(mod_path,'extrap_200rch_RF_', mod_choice,'_',cov_choice, '.rda'))

#load in new habitat data
#hab_new =

#-----------------------------------------------------------------
# Make predictions with new data
#-----------------------------------------------------------------

pred_hab_sites = qrf_mod_df %>%
  left_join(hab_new) %>%
  mutate(chnk_per_m = predict(qrf_mods[['Chinook']],
                              newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                              what = pred_quant),
         chnk_per_m = exp(chnk_per_m) - dens_offset,
         chnk_per_m2 = chnk_per_m * Lgth_Wet / Area_Wet) %>%
  mutate(sthd_per_m = predict(qrf_mods[['Steelhead']],
                              newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                              what = pred_quant),
         sthd_per_m = exp(sthd_per_m) - dens_offset,
         sthd_per_m2 = sthd_per_m * Lgth_Wet / Area_Wet)


#-----------------------------------------------------------------
# Expand new estimates across reaches, replace old (no-champ) extraps
#-----------------------------------------------------------------

