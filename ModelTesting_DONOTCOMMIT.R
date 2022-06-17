library(tidyverse)

out_path = 'S:/main/data/qrf/gitrepo_data/output/'

mod_choice = c("juv_summer",
               "juv_winter",
               "redds")[3]

load(paste0(out_path,"modelFit/extrap_200rch_RF_",mod_choice,".rda"))



quantile(filter(all_preds, chnk == TRUE)$chnk_per_m)
quantile(filter(all_preds, chnk == TRUE)$chnk_per_m_se)
quantile(filter(all_preds, chnk == TRUE)$chnk_per_m2)
quantile(filter(all_preds, chnk == TRUE)$chnk_per_m2_se)

quantile(filter(all_preds, chnk == TRUE)$chnk_per_m_se/
           filter(all_preds, chnk == TRUE)$chnk_per_m, na.rm = T)

quantile(filter(all_preds, chnk == TRUE)$chnk_per_m2_se/
           filter(all_preds, chnk == TRUE)$chnk_per_m2, na.rm = T)


quantile(filter(all_preds, sthd == TRUE)$sthd_per_m)
quantile(filter(all_preds, sthd == TRUE)$sthd_per_m_se)
quantile(filter(all_preds, sthd == TRUE)$sthd_per_m2)
quantile(filter(all_preds, sthd == TRUE)$sthd_per_m2_se)

quantile(filter(all_preds, sthd == TRUE)$sthd_per_m_se/
           filter(all_preds, sthd == TRUE)$sthd_per_m, na.rm = T)

quantile(filter(all_preds, sthd == TRUE)$sthd_per_m2_se/
           filter(all_preds, sthd == TRUE)$sthd_per_m2, na.rm = T)




quantile(filter(US_redds_df_old_extrap, chnk == TRUE)$chnk_per_m)
quantile(filter(US_redds_df_old_extrap, chnk == TRUE)$chnk_per_m_se)

quantile(filter(US_redds_df_new_extrap, chnk == TRUE)$chnk_per_m)
quantile(filter(US_redds_df_new_extrap, chnk == TRUE)$chnk_per_m_se)



#### Checking the gpkg data ####

in_path = 'S:/main/data/qrf/gitrepo_data/input/'
out_path = 'S:/main/data/qrf/gitrepo_data/output/'

# load .rda created from the "extrap_comparison_data_prep.R" script
load(paste0(out_path,'gpkg/extrap_compare/US_qrf_extraps_comp.rda'))

# Project area polygons and 200m layer (with habitat attributes) within project area
load(paste0(out_path,'gpkg/extrap_compare/US_spatial.rda'))

# convert old sf objects to tibbles
US_sum_df_old_extrap = US_sum_sf_old_extrap %>%
  st_drop_geometry() %>%
  as_tibble()

US_win_df_old_extrap = US_win_sf_old_extrap %>%
  st_drop_geometry() %>%
  as_tibble()

US_redds_df_old_extrap = US_redds_sf_old_extrap %>%
  st_drop_geometry() %>%
  as_tibble()

# convert new sf objects to tibbles
US_sum_df_new_extrap = US_sum_sf_new_extrap %>%
  st_drop_geometry() %>%
  as_tibble()

US_win_df_new_extrap = US_win_sf_new_extrap %>%
  st_drop_geometry() %>%
  as_tibble()

US_redds_df_new_extrap = US_redds_sf_new_extrap %>%
  st_drop_geometry() %>%
  as_tibble()

# the spatial domain for the species and life stages of the old extrapolations were modified from Morgon Bond's 
# designation to include local knowledge from biologists. To keep things consistent, we will use the same spatial domain
# for the new extrapolations. 

# redds
US_redds_sf_new_extrap = US_redds_sf_new_extrap %>%
  select(-chnk, -sthd) %>%
  st_join(US_redds_sf_old_extrap %>%
            select(chnk, sthd),
          join = st_covered_by,
          left = F)

# summer juv
US_sum_sf_new_extrap = US_sum_sf_new_extrap %>%
  select(-chnk, -sthd) %>%
  st_join(US_sum_sf_old_extrap %>%
            select(chnk, sthd),
          join = st_covered_by,
          left = F)

# winter juv
US_win_sf_new_extrap = US_win_sf_new_extrap %>%
  select(-chnk, -sthd) %>%
  st_join(US_win_sf_old_extrap %>%
            select(chnk, sthd),
          join = st_covered_by,
          left = F)

# create comparison tibbles for mapping

US_sum_sf_comp_extrap = bind_cols(setorder(US_sum_sf_old_extrap[,-c(21:32)], UniqueID), setorder(US_sum_df_new_extrap, UniqueID)[,c(21:32)] - setorder(US_sum_df_old_extrap, UniqueID)[,c(21:32)])

US_win_sf_comp_extrap = bind_cols(setorder(US_win_sf_old_extrap[,-c(21:32)], UniqueID), setorder(US_win_df_new_extrap, UniqueID)[,c(21:32)] - setorder(US_win_df_old_extrap, UniqueID)[,c(21:32)])

US_redds_sf_comp_extrap = bind_cols(setorder(US_redds_sf_old_extrap[,-c(21:32)], UniqueID), setorder(US_redds_df_new_extrap, UniqueID)[,c(21:32)] - setorder(US_redds_df_old_extrap, UniqueID)[,c(21:32)])



#Check quantiles

quantile(filter(US_redds_df_new_extrap, chnk == TRUE)$chnk_per_m_se/
           filter(US_redds_df_new_extrap, chnk == TRUE)$chnk_per_m, na.rm = T)

quantile(filter(US_redds_df_new_extrap, chnk == TRUE)$chnk_per_m2_se/
           filter(US_redds_df_new_extrap, chnk == TRUE)$chnk_per_m2, na.rm = T)

