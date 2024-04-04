#RF 200rch extrapolation model output figures

library(tidyverse)
library(quantregForest)

mod_path = 'S:/main/data/qrf/gitrepo_data/output/modelFit/'

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

cov_choice = c("Reduced","CovLW","Dash","No_elev")[4]

log_mod = c("log_","")[2] #log-response model or no

load(paste0(mod_path,"extrap_200rch_RF_",log_mod,mod_choice,'_',cov_choice,'.rda'))

source("R/plot_partial_dependence.r")
data("gaa_hab_dict")


#Rel imp.

chk_ri = tibble(Response = model_rf_df$response[1:2],
             qrf_mod = model_rf_df$mod_no_champ[1:2]) %>%
  mutate(rel_imp = map(qrf_mod,
                       .f = function(x) {
                         as_tibble(x$importance,
                                   rownames = 'Metric') %>%
                           mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
                           left_join(gaa_hab_dict %>%
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
             fill = Response)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1",
                    labels = c(bquote(Capacity/m), bquote(Capacity/m^2))) +
  coord_flip() +
  labs(title = 'Chinook',
       x = 'Metric',
       y = 'Relative importance')

stl_ri = tibble(Response = model_rf_df$response[3:4],
                qrf_mod = model_rf_df$mod_no_champ[3:4]) %>%
  mutate(rel_imp = map(qrf_mod,
                       .f = function(x) {
                         as_tibble(x$importance,
                                   rownames = 'Metric') %>%
                           mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
                           left_join(gaa_hab_dict %>%
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
             fill = Response)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1",
                    labels = c(bquote(Capacity/m), bquote(Capacity/m^2))) +
  coord_flip() +
  labs(title = 'Steelhead',
       x = 'Metric',
       y = 'Relative importance')

# PDPS for summer, winter ####
if(mod_choice == 'juv_summer'){
chk_m = plot_partial_dependence(model_rf_df$mod_no_champ[[1]],
                                model_rf_df$data[[1]],
                                data_dict = gaa_hab_dict,
                                log_transform = F,
                                log_offset = 0,
                                scales = 'free_x')+
  labs(title = "Chinook, juvenile summer", y = "Prediction (per meter)")

chk_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[2]],
                                 model_rf_df$data[[2]],
                                 data_dict = gaa_hab_dict,
                                 log_transform = F,
                                 log_offset = 0,
                                 scales = 'free_x')+
  labs(title = "Chinook, juvenile summer", y = bquote('Prediction per meter'^2))
  
stl_m = plot_partial_dependence(model_rf_df$mod_no_champ[[3]],
                                model_rf_df$data[[3]],
                                data_dict = gaa_hab_dict,
                                log_transform = F,
                                log_offset = 0,
                                scales = 'free_x')+
  labs(title = "Steelhead, juvenile summer", y = "Prediction (per meter)")
  
stl_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[4]],
                                 model_rf_df$data[[4]],
                                 data_dict = gaa_hab_dict,
                                 log_transform = F,
                                 log_offset = 0,
                                 scales = 'free_x')+
  labs(title = "Steelhead, juvenile summer", y = bquote('Prediction per meter'^2))
}

# PDPS for summer, winter ####
if(mod_choice == 'juv_winter'){
  chk_m = plot_partial_dependence(model_rf_df$mod_no_champ[[1]],
                                  model_rf_df$data[[1]],
                                  data_dict = gaa_hab_dict,
                                  log_transform = F,
                                  log_offset = 0,
                                  scales = 'free_x')+
    labs(title = "Chinook, juvenile winter", y = "Prediction (per meter)")
  
  chk_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[2]],
                                   model_rf_df$data[[2]],
                                   data_dict = gaa_hab_dict,
                                   log_transform = F,
                                   log_offset = 0,
                                   scales = 'free_x')+
    labs(title = "Chinook, juvenile winter", y = bquote('Prediction per meter'^2))
  
  stl_m = plot_partial_dependence(model_rf_df$mod_no_champ[[3]],
                                  model_rf_df$data[[3]],
                                  data_dict = gaa_hab_dict,
                                  log_transform = F,
                                  log_offset = 0,
                                  scales = 'free_x')+
    labs(title = "Steelhead, juvenile winter", y = "Prediction (per meter)")
  
  stl_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[4]],
                                   model_rf_df$data[[4]],
                                   data_dict = gaa_hab_dict,
                                   log_transform = F,
                                   log_offset = 0,
                                   scales = 'free_x')+
    labs(title = "Steelhead, juvenile winter", y = bquote('Prediction per meter'^2))
}


#PDPs for redds (in kms) ####
if(mod_choice == 'redds'){
chk_m = plot_partial_dependence(model_rf_df$mod_no_champ[[1]],
                                model_rf_df$data[[1]],
                                data_dict = gaa_hab_dict,
                                log_transform = F,
                                log_offset = 0,
                                scales = 'free_x',
                                km = T)+
  labs(title = "Chinook, redds", y = "Prediction (per km)")

chk_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[2]],
                                 model_rf_df$data[[2]],
                                 data_dict = gaa_hab_dict,
                                 log_transform = F,
                                 log_offset = 0,
                                 scales = 'free_x',
                                 km = T)+
  labs(title = "Chinook, redds", y = bquote('Prediction per km'^2))

stl_m = plot_partial_dependence(model_rf_df$mod_no_champ[[3]],
                                model_rf_df$data[[3]],
                                data_dict = gaa_hab_dict,
                                log_transform = F,
                                log_offset = 0,
                                scales = 'free_x',
                                km = T)+
  labs(title = "Steelhead, redds", y = "Prediction (per km)")

stl_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[4]],
                                 model_rf_df$data[[4]],
                                 data_dict = gaa_hab_dict,
                                 log_transform = F,
                                 log_offset = 0,
                                 scales = 'free_x',
                                 km = T)+
  labs(title = "Steelhead, redds", y = bquote('Prediction per km'^2))

}

#Output ####
pdf(paste0("output/figures/","RF_extrap_", log_mod, mod_choice,'_', cov_choice,".pdf"), width = 10, height = 8)
chk_ri
chk_m
chk_m2
stl_ri
stl_m
stl_m2
dev.off()

save(chk_ri,
     chk_m,
     chk_m2,
     stl_ri,
     stl_m,
     stl_m2,
     file = paste0("output/figures/","RF_extrap_", log_mod, mod_choice,'_', cov_choice,".rda"))
