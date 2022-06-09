#Author: Bryce Oldemeyer Mark Roes
#Purpose: Basic habitat dictionary for GAA's used in 2022 RF extrapolation
#Created: 5/17/2022
#Modified:

#---------------------------------------------------------------------------
library(tidyverse)
library(here)

# GAA table
gaa_hab_dict = tibble(
  ShortName = c("slope",
             #"rel_slope",
             "Sinuosity",
             #"regime",
             "alp_accum",
             "fines_accu",
             "flow_accum",
             "grav_accum",
             "p_accum",
             "fp_cur",
             "S2_02_11",
             "DistPrin1",
             "NatPrin1",
             "NatPrin2",
             "end_elev"),
  Name = c(
    "Gradient %",
   # "Relative slope",
    "Sinuosity",
    "Alpine accumulation",
    "Fines accumulation",
    "Flow accumulation",
    "Gravel accumulation",
    "Precipitation accumulation",
    "Floodplain width",
    "Avg Aug stream temperature",
    "Disturbance PCA 1",
    "Natural PCA 1",
    "Natural PCA 2",
    "Elevation"),
  DescriptiveText = c(
    "Stream gradient (%).",
   # "Relative slope. Reach slope minus upstream slope.",
    "Reach sinuosity. 1 = straight, 1 < sinuous.",
    #"Flow regime. 1 = mixed. 2 = snow dominated, 3 = rain dominated.",
    "Number of upstream cells in alpine terrain.",
    "Number of upstream cells in fine grain lithologies.",
    "Number of upstream DEM cells flowing into reach.",
    "Number of upstream cells in gravel producing lithologies.",
    "Number of upstream cells weighted by average annual precipitation.",
    "Current unmodified floodplain width.",
    "Historical composite scenario representing 10 year average August mean stream temperatures for 2002-2011 (Isaak et al. 2017).",
    "Disturbance Classification PCA 1 Score (Whittier et al. 2011).",
    "Natural Classification PCA 1 Score (Whittier et al. 2011).",
    "Natural Classification PCA 2 Score (Whittier et al. 2011).",
    "Elevation at downstream end of reach"
  ))

#Save habitat dict to repo
save(gaa_hab_dict, file = here("data/gaa_hab_dict.rda"))
