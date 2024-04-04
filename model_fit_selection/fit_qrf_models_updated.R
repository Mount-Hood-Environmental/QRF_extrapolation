# Author: Kevin See
# Purpose: Fit QRF models
# Created: 10/19/2020
# Last Modified: 06/2022
# Modified by: Mark Roes, Bryce Oldemeyer
# Notes: fish density is fish / m for summer juvs and redds, but fish / m^2 for winter juvs

#-----------------------------------------------------------------
# load needed libraries
#library(QRFcapacity)
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(quantregForest)
library(readxl)
library(elevatr)

# set default theme for ggplot
theme_set(theme_bw())

#Set file paths
in_path = 'S:/main/data/qrf/gitrepo_data/input/'
out_path = 'S:/main/data/qrf/gitrepo_data/output/'

#load necessary functions (for now, will remove when package is available)
source("R/impute_missing_data.r")
source("R/plot_partial_dependence.r")
source("R/plot_partial_dependence_v2.r")

#Load in fish-habitat data
data("fish_hab_list")
#load habitat dictionary
data("hab_dict")

#-----------------------------------------------------------------
# which life stage, species, and covariates to fit? 
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[1]

cov_choice = c("Reduced", "CovLW", "Dash", "No_elev")[4]

#-----------------------------------------------------------------
# determine which set of fish/habitat data to use
if(mod_choice == "juv_summer") {
    fish_hab = fish_hab_list$Summer_CHaMP 
} else if(mod_choice == "juv_summer_dash") {
  fish_hab = fish_hab_list$Summer_DASH 
} else if(mod_choice == "redds") {
  fish_hab = fish_hab_list$Redds 
} else if(mod_choice == "juv_winter") {
  fish_hab = fish_hab_list$Winter 
}
 
#-----------------------------------------------------------------
#

if(mod_choice != "juv_winter") {
  load(paste0(in_path, "rch_200.rda"))
  load(paste0(in_path, "champ_site_rch.rda"))
  
  chnk_sites = champ_site_rch %>%
    inner_join(rch_200 %>%
                 select(UniqueID, chnk)) %>%
    filter(chnk) %>%
    pull(Site) %>%
    as.character()
  
  # add Big Springs and Little Springs sites in the Lemhi
  chnk_sites = c(chnk_sites,
                 fish_hab %>%
                   filter(grepl('Big0Springs', Site) | grepl('Little0Springs', Site)) %>%
                   pull(Site) %>%
                   unique()) %>%
    unique()
  
  
  # only keep Chinook data from sites in Chinook domain
  fish_hab %<>%
    filter(Species == 'Steelhead' |
             (Species == 'Chinook' & (Site %in% chnk_sites | fish_dens > 0))) 
  
}

#-----------------------------------------------------------------
# select which habitat metrics to use in QRF model
#-----------------------------------------------------------------

if(mod_choice == "juv_summer"){
  mod_cov_select<-read_xlsx("model_fit_selection/ModelCovSelected.xlsx",
                            range = cell_cols("A:E"),
                            sheet = "CHaMP_Summer")
  sel_hab_mets = crossing(Species = c('Chinook', 
                                                'Steelhead'),
                          mod_cov_select %>%
                            filter(!!as.symbol(cov_choice) == 1) %>%
                            select(Metric))
}else if(mod_choice == "redds") {
  mod_cov_select<-read_xlsx("model_fit_selection/ModelCovSelected.xlsx",
                            range = cell_cols("A:E"),
                            sheet = "CHaMP_Redds")
  sel_hab_mets = crossing(Species = c('Chinook', 
                                                'Steelhead'),
                          mod_cov_select %>%
                            filter(!!as.symbol(cov_choice) == 1) %>%
                            select(Metric))
}else if(mod_choice == "juv_winter") {
  mod_cov_select<-read_xlsx("model_fit_selection/ModelCovSelected.xlsx",
                            range = cell_cols("A:F"),
                            sheet = "CHaMP_Winter")
  sel_hab_mets = crossing(Species = c('Chinook', 
                                                'Steelhead'),
                          mod_cov_select %>%
                            filter(!!as.symbol(cov_choice) == 1) %>%
                            select(Metric))
}
#-----------------------------------------------------------------
# Fit QRF model
#-----------------------------------------------------------------

# Fill in missing elevation data using elevtR() package based on USGS - 3DEP dataset

elev_DEM = fish_hab %>%
  select(x = LON_DD,
         y = LAT_DD)

# elevtr() needs a data frame with x & y coordinates

out = get_elev_point(locations = as.data.frame(elev_DEM), units ="meters", src = "epqs", prj = 4326) #WGS84

fish_hab = fish_hab %>%
  mutate(Elev_M_DEM = out@data[["elevation"]],
         Elev_M = coalesce(Elev_M, Elev_M_DEM)) 

# Impute covariates

all_covars = sel_hab_mets %>%
  pull(Metric)

impute_covars = c('Watershed', 'Elev_M', 'Sin', 'Year', 'CUMDRAINAG')

if(mod_choice == "redds"){
  impute_covars = impute_covars[!grepl('Year', impute_covars)]
}

qrf_mod_df = fish_hab %>%
  mutate(id = 1:n()) %>%
  select(id,
         all_of(impute_covars),
         all_of(all_covars)) %>%
  impute_missing_data(covars = all_covars,
                      impute_vars = impute_covars,
                      method = 'missForest') %>%
  left_join(fish_hab %>%
              mutate(id = 1:n()) %>%
              select(id, 
                     Species, Site, Watershed,
                     LON_DD, LAT_DD,
                     fish_dens)) %>%
  select(id, Species, Site, Watershed, LON_DD, LAT_DD, fish_dens, one_of(all_covars))

if(mod_choice %in% c('juv_summer',
                     'juv_summer_dash')) {
  qrf_mod_df %<>%
    left_join(fish_hab %>%
                mutate(id = 1:n()) %>%
                select(id, 
                       VisitID,
                       Year)) %>%
    select(Species:Watershed,
           Year,
           VisitID,
           everything(),
           -id)
} else if(mod_choice == "redds") {
  qrf_mod_df %<>%
    left_join(fish_hab %>%
                mutate(id = 1:n()) %>%
                select(id, 
                       maxYr)) %>%
    select(Species:Watershed,
           maxYr,
           everything(),
           -id)
} else if(mod_choice == "juv_winter") {
  qrf_mod_df %<>%
    left_join(fish_hab %>%
                mutate(id = 1:n()) %>%
                select(id, 
                       VisitID,
                       Year, ChUnitNumber, SiteUnit)) %>%
    select(Species:Watershed,
           VisitID,
           Year, ChUnitNumber, SiteUnit,
           everything(),
           -id)
}


# fit the QRF model
# set the density offset (to accommodate 0s)
range(fish_hab$fish_dens)
summary(fish_hab$fish_dens)
dens_offset = if_else(mod_choice == 'redds',
                      0,
                      0.005)

# fit random forest models


qrf_mods = qrf_mod_df %>%
  split(list(.$Species)) %>%
  map(.f = function(z) {
    
    covars = sel_hab_mets %>%
      filter(Species == unique(z$Species)) %>%
      pull(Metric)

set.seed(3)
qrf_mod = quantregForest(x = z %>%
                           select(one_of(all_covars)),
                         y = z %>%
                           mutate(across(fish_dens,
                                         ~ log(. + dens_offset))) %>%
                           pull(fish_dens),
                         keep.inbag = T,
                         ntree = 1000)
    return(qrf_mod)
  })

# save some results
save(sel_hab_mets,
     qrf_mod_df,
     dens_offset,
     qrf_mods,
     file = paste0(out_path,'modelFit/',mod_choice,'_', cov_choice,'.rda'))


#-----------------------------------------------------------------
# create a few figures
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

cov_choice = c("Reduced", "CovLW", "Dash", "No_elev")[4]
#cov_choice = c('QRF2',
#               'QRF2_trimmed')[2]

load(paste0(out_path,'modelFit/', mod_choice,'_', cov_choice,'.rda'))

###
rel_imp_p = tibble(Species = names(qrf_mods),
                   qrf_mod = qrf_mods) %>%
  mutate(rel_imp = map(qrf_mod,
                       .f = function(x) {
                         as_tibble(x$importance,
                                   rownames = 'Metric') %>%
                           mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
                           left_join(hab_dict %>%
                                       select(Metric = ShortName,
                                              Name)) %>%
                           mutate(across(c(Metric, Name),
                                         ~ fct_reorder(., relImp))) %>%
                           arrange(Metric) %>%
                           distinct()
                       })) %>%
  unnest(cols = rel_imp) %>%
  select(-qrf_mod) %>%
  mutate(across(c(Metric, Name),
                fct_reorder,
                .x = relImp)) %>%
  ggplot(aes(x = Name,
             y = relImp,
             fill = Species)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(x = 'Metric',
       y = 'Relative importance')

# # partial dependence plots
# #Summer, winter
# # for Chinook
# if(mod_choice != 'redds'){
# chnk_pdp = plot_partial_dependence(qrf_mods[['Chinook']],
#                                      qrf_mod_df %>%
#                                        filter(Species == 'Chinook'),
#                                      data_dict = hab_dict,
#                                      # log_transform = F,
#                                      log_offset = dens_offset,
#                                      scales = "free_x") +
#                                      #scales = 'free') +
#     labs(title = 'Chinook', y = "Prediction per m")
# 
# # for steelhead
# sthd_pdp = plot_partial_dependence(qrf_mods[['Steelhead']],
#                                      qrf_mod_df %>%
#                                        filter(Species == 'Steelhead'),
#                                      data_dict = hab_dict,
#                                      # log_transform = F,
#                                      log_offset = dens_offset,
#                                      scales = "free_x") +
#                                      #scales = 'free') +
#     labs(title = 'Steelhead', y = "Prediction per m")
# }
# 
# 

if(mod_choice == 'juv_summer'){
  chnk_pdp = plot_partial_dependence(qrf_mods[['Chinook']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Chinook'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     scales = "free_x",
                                     km = F) +
    labs(title = 'Chinook, juvenile summer', y = "Prediction (per m)")

  # for steelhead
  sthd_pdp = plot_partial_dependence(qrf_mods[['Steelhead']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Steelhead'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     scales = "free_x",
                                     km = F) +
    labs(title = 'Steelhead, juvenile summer', y = "Prediction (per m)")
}


if(mod_choice == 'juv_winter'){
  chnk_pdp = plot_partial_dependence(qrf_mods[['Chinook']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Chinook'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     scales = "free_x",
                                     km = F) +
    labs(title = 'Chinook, juvenile winter', y = "Prediction (per m)")
  
  # for steelhead
  sthd_pdp = plot_partial_dependence(qrf_mods[['Steelhead']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Steelhead'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     scales = "free_x",
                                     km = F) +
    labs(title = 'Steelhead, juvenile winter', y = "Prediction (per m)")
}

if(mod_choice == 'redds'){
  chnk_pdp = plot_partial_dependence(qrf_mods[['Chinook']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Chinook'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     scales = "free_x",
                                     km = T) +
    labs(title = 'Chinook, redds', y = "Prediction (per km)")
  
  # for steelhead
  sthd_pdp = plot_partial_dependence(qrf_mods[['Steelhead']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Steelhead'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     scales = "free_x",
                                     km = T) +
    labs(title = 'Steelhead, redds', y = "Prediction (per km)")
}


pdf(paste0("output/figures/", mod_choice,'_', cov_choice,".pdf"), width = 10, height = 8)
rel_imp_p
chnk_pdp
sthd_pdp
dev.off()


#Save figures
save(rel_imp_p,
     chnk_pdp,
     sthd_pdp,
     file = paste0("output/figures/", mod_choice,'_',cov_choice,'_figs',".rda"))
