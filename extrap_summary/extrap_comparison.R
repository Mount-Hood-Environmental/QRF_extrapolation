# Author: Bryce Oldemeyer (credit Kevin See for base script)
# Purpose: Compare old extrapolations to new 2022 extrapolations by CHaMP watershed
# Created: 5/17/2022
# Last Modified: 
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(QRFcapacity)
library(here)

# set default theme for ggplot
theme_set(theme_bw())

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

cov_choice = c("Reduced")[1]

in_path = 'S:/main/data/qrf/gitrepo_data/input/'
out_path = 'S:/main/data/qrf/gitrepo_data/output/'

load(paste0(out_path,'modelFit/extrap_200rch_RF_',cov_choice,"_", mod_choice, '.rda'))

#-----------------------------------------------------------------
#
