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
library(patchwork)

#-----------------------------------------------------------------
# load model fits and DASH data to be added
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

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
         DpthResid = hr_avg_resid_pool_dpth_m,
         DpthThlwgExit = hr_thlwg_dpth_avg_m,
         Sin = hr_sin_cl,
         SubEstBldr = sub_est_bldr,
         SubEstCbl = sub_est_cbl,
         SubEstCandBldr = sub_est_cbl + sub_est_bldr,
         SubEstGrvl = sub_est_gravl,
         SubEstSandFines = sub_est_sand_fines,
         FishCovLW = fish_cov_lwd,
         WetBraid = hr_braidedness,
         Q = hr_discharge_cfs,
         id = 1:n())

#-----------------------------------------------------------------
# Make predictions with new data
#-----------------------------------------------------------------

#Append temperatures from rch_200 file
load(paste0(in_path,"rch_200.rda")) 

temps = rch_200 %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  select(S2_02_11, end_elev, region, p_accum, slope) %>%
  rename(avg_aug_temp = S2_02_11)

impute_covars = c("Sin", "end_elev", "region", "slope")
pred_quant = 0.9

#Make predictions
new_preds = hab_rds %>%
  st_join(temps,
          left = T, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(region = as.factor(region)) %>%
  impute_missing_data(covars = c("avg_aug_temp", "PoolResidDpth", "DpthResid", "Q"),
                    impute_vars = impute_covars,
                    method = 'missForest') %>%
  select(site_name, year, LWFreq_Wet:id, avg_aug_temp, hr_length_m) %>%
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



#-----------------------------------------------------------------
# Summaries
#-----------------------------------------------------------------
pred_caps = new_preds %>%
  group_by(site_name, year) %>%
  summarize(length_m = sum(hr_length_m),
            chnk_sitecap = sum(chnk_cap),
            chnk_density_m = weighted.mean(chnk_per_m, hr_length_m),
            sthd_sitecap = sum(sthd_cap),
            sthd_densit_m = weighted.mean(sthd_per_m, hr_length_m)) %>%
  ungroup()

#----- Save preds
save(new_preds,
     pred_caps,
     file = paste0('S:/main/data/qrf/DASH_estimates/', mod_choice,'_',cov_choice, '.rda'))



#---- Histograms of habitat data


hab_plots = new_preds %>%
  mutate(site = gsub("[[:digit:]]","", site_name)) %>%
  group_by(site, year) %>%
  summarize(LWFreq_Wet = weighted.mean((LWFreq_Wet/hr_length_m)*100,hr_length_m),
            CU_Freq = weighted.mean(CU_Freq,hr_length_m),
            DpthThlwg_Avg = weighted.mean(DpthThlwg_Avg,hr_length_m),
            FishCovSome = weighted.mean(FishCovSome,hr_length_m),
            FstNT_Freq = weighted.mean(FstNT_Freq, hr_length_m),
            FstTurb_Freq = weighted.mean(FstTurb_Freq, hr_length_m),
            PoolResidDpth = weighted.mean(PoolResidDpth, hr_length_m),
            Sin = weighted.mean(Sin, hr_length_m),
            SubEstBldr = weighted.mean(SubEstBldr, hr_length_m),
            SubEstCbl = weighted.mean(SubEstCbl, hr_length_m),
            SubEstGrvl = weighted.mean(SubEstGrvl, hr_length_m),
            SubEstSandFines = weighted.mean(SubEstSandFines, hr_length_m),
            WetBraid = weighted.mean(WetBraid, hr_length_m),
            avg_aug_temp = weighted.mean(avg_aug_temp, hr_length_m),
            Q = weighted.mean(Q, hr_length_m)) %>%
  pivot_longer(cols = c(LWFreq_Wet:Q),
               names_to ="hab_feature",
               values_to="value") %>%
  ggplot(aes(x = site, y = value, color = site))+
  geom_point()+
  facet_wrap(vars(hab_feature), scales = "free", ncol = 1) +
  theme(legend.position = 'bottom',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

tiff(hab_plots, filename = 'output/figures/DASH/DASH_hab_plots.tiff', compression = 'lzw', units = 'in', width = 10, height = 20, res = 300)
hab_plots
dev.off()
