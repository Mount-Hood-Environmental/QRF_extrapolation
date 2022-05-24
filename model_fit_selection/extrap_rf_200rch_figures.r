#RF 200rch extrapolation model output figures

library(tidyverse)
library(quantregForest)

mod_path = 'S:/main/data/qrf/gitrepo_data/output/modelFit/'

mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds',
               'juv_winter')[4]

cov_choice = c("Reduced")[1]

load(paste0(mod_path,"extrap_200rch_RF_",mod_choice,'.rda'))

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
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(title = 'Chinook',
       x = 'Metric',
       y = 'Relative Importance')

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
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(title = 'Steelhead',
       x = 'Metric',
       y = 'Relative Importance')

#PDP
chk_m = plot_partial_dependence(model_rf_df$mod_no_champ[[1]],
                                model_rf_df$data[[1]],
                                data_dict = gaa_hab_dict,
                                log_transform = T)+
  labs(title = "Chinook", y = "Prediction per meter")

chk_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[2]],
                                 model_rf_df$data[[2]],
                                 data_dict = gaa_hab_dict,
                                 log_transform = T)+
  labs(title = "Chinook", y = bquote('Prediction per meter'^2))
  
stl_m = plot_partial_dependence(model_rf_df$mod_no_champ[[3]],
                                model_rf_df$data[[3]],
                                data_dict = gaa_hab_dict,
                                log_transform = T)+
  labs(title = "Steelhead", y = "Prediction per meter")
  
stl_m2 = plot_partial_dependence(model_rf_df$mod_no_champ[[4]],
                                 model_rf_df$data[[4]],
                                 data_dict = gaa_hab_dict,
                                 log_transform = T)+
  labs(title = "Steelhead", y = bquote('Prediction per meter'^2))


pdf(paste0("output/figures/","RF_extrap_", mod_choice,'_', cov_choice,".pdf"), width = 10, height = 8)
chk_ri
chk_m
chk_m2
stl_ri
stl_m
stl_m2
dev.off()
