# Authors: Bryce Oldemeyer
# Purpose: Prep QRF model covariate tables for 2022 modified QRF models
# Created: 4/10/2022
# Last Modified: 4/11/2022
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(magrittr)
library(here)

#-----------------------------------------------------------------
file_path = 'S:/main/data/qrf/gitrepo_data/output/'

#-----------------------------------------------------------------
# load QRF model fits
load(paste0(file_path,'modelFit/juv_summer_Reduced.rda'))
juv_sum_chnk_covs = qrf_mods$Chinook$importance
juv_sum_sthd_covs = qrf_mods$Steelhead$importance

load(paste0(file_path,'modelFit/juv_winter_Reduced.rda'))
juv_win_chnk_covs = qrf_mods$Chinook$importance
juv_win_sthd_covs = qrf_mods$Steelhead$importance

load(paste0(file_path,'modelFit/redds_Reduced.rda'))
redds_chnk_covs = qrf_mods$Chinook$importance
redds_sthd_covs = qrf_mods$Steelhead$importance

# clean up environment
rm(qrf_mod_df,
   qrf_mods,
   sel_hab_mets)

load(here("data/hab_dict.rda"))

# Create Habitat Covariate Table
# Juvenile Summer Chinook
QRF_new_hab_cov_tbl = as_tibble(juv_sum_chnk_covs, rownames = "Covariate") %>%
  mutate(`Juv Sum Chnk` = dense_rank(desc(IncNodePurity))) %>%
  select(-IncNodePurity) %>%
  # Juvenile Summer Steelhead
  full_join(as_tibble(juv_sum_sthd_covs, rownames = "Covariate") %>%
            mutate(`Juv Sum Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Juvenile Winter Chinook
  full_join(as_tibble(juv_win_chnk_covs, rownames = "Covariate") %>%
            mutate(`Juv Win Chnk` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Juvenile Winter Steelhead
  full_join(as_tibble(juv_win_sthd_covs, rownames = "Covariate") %>%
            mutate(`Juv Win Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Redds Chinook
  full_join(as_tibble(redds_chnk_covs, rownames = "Covariate") %>%
            mutate(`Redds Chnk` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Redds Steelhead
  full_join(as_tibble(redds_sthd_covs, rownames = "Covariate") %>%
            mutate(`Redds Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  left_join(hab_dict %>%
              select(ShortName,
                     Name,
                     DescriptiveText,
                     MetricCategory),
            by = c("Covariate" = "ShortName")) %>%
  distinct() %>%
  # some cleaning
  filter(!Covariate %in% c("Discharge", "LWCount")) %>%
  select(Covariate,
         Name,
         MetricCategory,
         everything(),
         DescriptiveText) %>%
  mutate(
    MetricCategory = if_else(Covariate == "DpthResid"  & is.na(MetricCategory), "Size", MetricCategory),
    MetricCategory = if_else(Covariate == "DpthThlwgExit"  & is.na(MetricCategory), "Size", MetricCategory),
    MetricCategory = if_else(Covariate == "Tier1" & is.na(MetricCategory), "ChannelUnit", MetricCategory)
  ) %>%
  arrange(MetricCategory,
          Covariate) %>%
  rename(`Metric Category` = MetricCategory,
         Description = DescriptiveText)

# save results
save(QRF_new_hab_cov_tbl,
     file = here("data/QRF_new_hab_cov_tbl.rda"))
