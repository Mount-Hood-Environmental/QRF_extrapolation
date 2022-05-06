# Author: Kevin See
# Purpose: Fit QRF models
# Created: 10/19/2020
# Last Modified: 10/19/2020
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
               'juv_summer_dash',
               'redds',
               'juv_winter')[4]

species_choice = c('Chinook',
               'Steelhead')[1]

cov_choice = c("QRF2",
               "QRF2_trimmed")[2]

#-----------------------------------------------------------------
# determine which set of fish/habitat data to use
if(mod_choice == "juv_summer") {
    fish_hab = fish_hab_list$Summer_CHaMP %>%
    filter(Species == species_choice)
} else if(mod_choice == "juv_summer_dash") {
  fish_hab = fish_hab_list$Summer_DASH %>%
    filter(Species == species_choice)
} else if(mod_choice == "redds") {
  fish_hab = fish_hab_list$Redds %>%
    filter(Species == species_choice)
} else if(mod_choice == "juv_winter") {
  fish_hab = fish_hab_list$Winter %>%
    filter(Species == species_choice)
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
                            range = cell_cols("A:D"),
                            sheet = paste0("CHaMP_Summer_",species_choice))
  sel_hab_mets = crossing(Species = species_choice,
                          mod_cov_select %>%
                            filter(!!as.symbol(cov_choice) == 1) %>%
                            select(Metric))
}else if(mod_choice == "redds") {
  mod_cov_select<-read_xlsx("model_fit_selection/ModelCovSelected.xlsx",
                            range = cell_cols("A:D"),
                            sheet = paste0("CHaMP_Redds_",species_choice))
  sel_hab_mets = crossing(Species = species_choice,
                          mod_cov_select %>%
                            filter(!!as.symbol(cov_choice) == 1) %>%
                            select(Metric))
}else if(mod_choice == "juv_winter") {
  mod_cov_select<-read_xlsx("model_fit_selection/ModelCovSelected.xlsx",
                            range = cell_cols("A:D"),
                            sheet = paste0("CHaMP_Winter_",species_choice))
  sel_hab_mets = crossing(Species = species_choice,
                          mod_cov_select %>%
                            filter(!!as.symbol(cov_choice) == 1) %>%
                            select(Metric))
}
#-----------------------------------------------------------------
# Fit QRF model
#-----------------------------------------------------------------

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
set.seed(3)

qrf_mod = quantregForest(x = qrf_mod_df %>%
                           select(one_of(all_covars)),
                         y = qrf_mod_df %>%
                           mutate(across(fish_dens,
                                         ~ log(. + dens_offset))) %>%
                           pull(fish_dens),
                         keep.inbag = T,
                         ntree = 1000)

# save some results
save(sel_hab_mets,
     qrf_mod_df,
     dens_offset,
     qrf_mod,
     file = paste0(out_path,'modelFit/',cov_choice,'_', mod_choice,'_', species_choice,'.rda'))


#-----------------------------------------------------------------
# create a few figures
#-----------------------------------------------------------------
#mod_choice = c('juv_summer',
#               'juv_summer_dash',
#               'redds',
#               'juv_winter')[1]

#species_choice = c('Chinook',
#                   'Steelhead')[1]

#cov_choice = c('QRF2',
#               'QRF2_trimmed')[2]

load(paste0(out_path,'modelFit/',cov_choice,'_', mod_choice,'_', species_choice,'.rda'))

###
rel_imp_p = as_tibble(qrf_mod$importance,
              rownames = 'Metric') %>%
      mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
      left_join(hab_dict %>%
                  select(Metric = ShortName,
                         Name)) %>%
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., relImp))) %>%
      arrange(Metric) %>%
      distinct() %>%
      ggplot(aes(x = Name,
                 y = relImp)) +
      geom_col(fill = 'gray40') +
      coord_flip() +
      labs(x = 'Metric',
           y = 'Relative Importance')

# partial dependence plots
pdp = plot_partial_dependence(qrf_mod,
              qrf_mod_df,
              data_dict = hab_dict,
              # log_transform = F,
              log_offset = dens_offset,
              # scales = "free_x") +
              scales = 'free') +
  labs(title = paste0(mod_choice,"_",species_choice,"_",cov_choice)) +
  theme(strip.text = element_text(size = 7))

pdf(paste0("output/figures/", mod_choice,'_', species_choice,'_', cov_choice,".pdf"), width = 10, height = 8)
rel_imp_p
pdp
dev.off()


# partial dependence plots v2 -- Times out for me. BO
pdp2 = plot_partial_dependencev2(qrf_mod,
                              qrf_mod_df,
                              data_dict = hab_dict,
                              # log_transform = F,
                              log_offset = dens_offset,
                              # scales = "free_x") +
                              scales = 'free') +
  labs(title = paste0(mod_choice,"_",species_choice,"_",cov_choice)) +
  theme(strip.text = element_text(size = 7))

pdp2

