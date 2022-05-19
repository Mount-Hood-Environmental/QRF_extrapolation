#RF 200rch extrapolation model output figures

library(tidyverse)

mod_path = 'S:/main/data/qrf/gitrepo_data/output/modelFit/'

mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds',
               'juv_winter')[4]

cov_choice = c("Reduced")[1]

load(paste0(mod_path,"extrap_200rch_RF_",cov_choice,'_',mod_choice,'.rda'))

