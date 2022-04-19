#Plots for covariate selection
#Mark Roes
#2022
#-----------------------------------------------------------------
# load/install libraries

options(repos=structure(c(cran="https://ftp.osuosl.org/pub/cran/")))  
packages <- c("tidyverse","purrr", "ggplot2", "lemon") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  
lapply(packages, require, character.only=TRUE)

#Load data------------------------------------------------------------
data("MINE_stats")

#Species plots--------------------------------------------------------
#Chinook
mine_chnk_p = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      filter(species == "Chinook") %>%
      # filter(MetricCategory != 'Other') %>%
      # put the metric names in descending order by MIC
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., .x = MIC))) %>%
      arrange(desc(Metric)) %>%
      ggplot(aes(x = Name,
                 y = MIC)) +
      facet_wrap(~MetricCategory, scales = 'free_y') +
      geom_col() +
      coord_flip() +
      # scale_fill_viridis_d() +
      scale_fill_brewer(palette = 'Set3',
                        guide = guide_legend(nrow = 2)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 6)) +
      labs(title = unique(x$dataset),
           fill = 'Category')
  })


pdf(paste0('MINE_Chnk2.pdf'),
    width = 16,
    height = 9)
for(i in 1:length(mine_chnk_p)) {
  print(mine_chnk_p[[i]])
}
dev.off()


#Steelhead
mine_stlhd_p = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      filter(species == "Steelhead") %>%
      # filter(MetricCategory != 'Other') %>%
      # put the metric names in descending order by MIC
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., .x = MIC))) %>%
      arrange(desc(Metric)) %>%
      ggplot(aes(x = Name,
                 y = MIC)) +
      facet_wrap(~MetricCategory, scales = 'free_y') +
      geom_col() +
      coord_flip() +
      # scale_fill_viridis_d() +
      scale_fill_brewer(palette = 'Set3',
                        guide = guide_legend(nrow = 2)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 6)) +
      labs(title = unique(x$dataset),
           fill = 'Category')
  })


pdf(paste0('MINE_stlhd2.pdf'),
    width = 16,
    height = 9)
for(i in 1:length(mine_stlhd_p)) {
  print(mine_stlhd_p[[i]])
}
dev.off()

#MIC difference by species ------------------------------------------ 
#Calculate differences
stlhd = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      filter(species == "Steelhead")
    }) %>%
  bind_rows()
  

chnk = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      filter(species == "Chinook") 
    }) %>%
  bind_rows()
  
mic_diff = left_join(stlhd,chnk[,c(1:4,12)], by = c("MetricCategory","Metric","dataset")) %>%
  mutate(MIC_diff = abs(MIC.x - MIC.y)) %>%
  mutate(MIC_diff_direction = ifelse(MIC.x > MIC.y, "+","-" )) %>%
  mutate(MIC_rel_diff = MIC.y/MIC.x) %>%
  split(.$dataset)


#Generate figures - DEPRECATED
mic_diff_s = mic_diff %>%
  map(.f = function(x) {
    x %>%
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., .x = MIC.x))) %>%
      arrange(desc(Metric)) %>%
      ggplot() +
      geom_col(aes(x = Name,
                 y = MIC_diff)) +
      geom_col(aes(x= Name,
                 y = MIC.x), alpha = 0.5)+
      facet_wrap(~MetricCategory, scales = 'free_y') +
      coord_flip() +
      scale_fill_brewer(palette = 'Set3',
                        guide = guide_legend(nrow = 2)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 6)) +
      labs(title = unique(x$dataset),
           fill = 'Category',
           x = element_blank(),
           y = "Steelhead MIC and |MIC Difference with Chinook|")
  })


mic_diff_c = mic_diff %>%
  map(.f = function(x) {
    x %>%
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., .x = MIC.y))) %>%
      arrange(desc(Metric)) %>%
      ggplot() +
      geom_col(aes(x = Name,
                 y = MIC_diff)) +
      geom_col(aes(x= Name,
                 y = MIC.y), alpha = 0.5)+
      facet_wrap(~MetricCategory, scales = 'free_y') +
      coord_flip() +
      scale_fill_brewer(palette = 'Set3',
                        guide = guide_legend(nrow = 2)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 6)) +
      labs(title = unique(x$dataset),
           fill = 'Category',
           x = element_blank(),
           y = "Chinook MIC and |MIC Difference with Steelhead|")
  })


pdf(paste0('output/figures/MIC_diff.pdf'),
    width = 16,
    height = 9)

for(i in 1:length(mic_diff_s)) {
  print(mic_diff_s[[i]])
  print(mic_diff_c[[i]])
}

dev.off()

#Flagging covs with restoration utility-------------------------------
#Read in restoration info and bind to MIC data
resto = read_csv("data/Resto_inform_cov.csv")[,-c(1)] ; names(resto)[names(resto)=="ShortName"] = "Metric"

mic_dat_resto = mic_diff %>%
  bind_rows() %>%
  left_join(select(resto, Metric, RestoInform) , by = c("Metric")) %>%
  split(.$dataset)

#Output .rda
save(mic_dat_resto, file = "data/mic_dat_resto.rda")

#Add this info into a figure

mic_resto_s = mic_dat_resto %>%
  map(.f = function(x) {
    x %>%
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., .x = MIC.x))) %>%
      arrange(desc(Metric)) %>%
      ggplot() +
      theme_bw()+
      geom_col(aes(x= Name,
                   y = MIC.x,
                   fill = as.factor(RestoInform)))+
      geom_col(aes(x = Name,
                   y = MIC_diff,
                   fill = MIC_diff_direction), color = "gray15") +
      scale_fill_manual(labels = c("Chinook MIC higher", "Chinook MIC lower", "Not informative", "Informative for restoration"),
                        values = c("chartreuse3", "firebrick3", "gray50","cornflowerblue"))+
      facet_wrap(~MetricCategory, scales = 'free_y') +
      coord_flip() +
      #scale_fill_brewer(palette = 'Set3',
      #                  guide = guide_legend(nrow = 2)) +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            axis.text = element_text(size = 6)) +
      labs(title = unique(x$dataset),
           fill = 'Category',
           x = element_blank(),
           y = "Steelhead MIC")
  })

pdf(paste0('output/figures/MIC_resto.pdf'),
    width = 16,
    height = 9)

for(i in 1:length(mic_resto_s)) {
  print(mic_resto_s[[i]])
}

dev.off()