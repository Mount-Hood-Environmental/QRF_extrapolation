# Author: Bryce Oldemeyer 
# Purpose: Compare old extrapolations to new 2022 extrapolations at eight upper salmon watersheds
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

load(paste0(out_path,'gpkg/extrap_compare/US_qrf_extraps_comp.rda'))

#-----------------------------------------------------------------
#
