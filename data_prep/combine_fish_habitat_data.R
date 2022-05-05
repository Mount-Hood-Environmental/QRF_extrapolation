# Author: Kevin See
# Purpose: Combine fish and habitat data
# Created: 9/11/2019
# Last Modified: 10/24/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
#library(QRFcapacity)
# library(maptools)


#Set file paths
in_path = 'S:/main/data/qrf/gitrepo_data/input/'
out_path = 'S:/main/data/qrf/gitrepo_data/output/'

#-----------------------------------------------------------------
# summer juvenile data 2011-2017
#-----------------------------------------------------------------
load(paste0(in_path,"fish_sum_est.rda"))
load(paste0(in_path,"champ_site_2011_17.rda"))
load(paste0(in_path,"champ_cu.rda"))
# for temperature metrics
load(paste0(in_path,"champ_temps.rda"))

#add champ_cu avg pool max_depths to site-scale habitat data

champ_site_2011_17 <- champ_site_2011_17 %>%
  left_join( champ_cu <- champ_cu %>%
               group_by(Site, VisitID) %>%
               mutate(Dpth_Max_Pool = ifelse(
                 Tier2 %in% c("Scour Pool", "Plunge Pool", "Dam Pool", "Beaver Pool"), Dpth_Max, NA)) %>%
               summarise(Dpth_Max_Avg = mean(Dpth_Max_Pool, na.rm =T)) %>%
               na_if(-Inf)
             )
  
  
#Join habitat and fish data
fh_sum_champ_2017 = fish_sum_est %>%
  filter(Valid) %>%
  filter(!is.na(FishSiteLength)) %>%
  rename(fishSampDate = SampleDate) %>%
  select(-Stream) %>%
  inner_join(champ_site_2011_17 %>%
               rename(habSampDate = SampleDate,
                      Year = VisitYear) %>%
               filter(VisitObjective == 'Primary Visit',
                      VisitStatus == 'Released to Public')) %>%
  unite(siteSppYr, Site, Species, Year, remove = F) %>%
  mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
         time_diff = abs(as.integer(time_diff))) %>%
  mutate(fish_dens = N / FishSiteLength) %>%
  group_by(siteSppYr) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  filter(time_diff == min(time_diff, na.rm = T)) %>%
  ungroup()


# add some temperature data
fh_sum_champ_2017 %<>%
  left_join(fh_sum_champ_2017 %>%
              select(VisitID, Year) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(VisitID, avg_aug_temp = S2_02_11))) %>%
  left_join(fh_sum_champ_2017 %>%
              select(VisitID, Year) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(Site:VisitID, S1_93_11:S36_2015) %>%
                          gather(scenario, aug_temp, S1_93_11:S36_2015) %>%
                          mutate(Year = str_sub(scenario, -4)) %>%
                          mutate_at(vars(Year),
                                    list(as.numeric)) %>%
                          filter(!is.na(Year)) %>%
                          select(Site:VisitID, Year, aug_temp)))
  

# for each site, pull out the year with the highest fish density
fh_sum_champ_2017 %<>%
  unite(siteSpp, Site, Species, remove = F) %>%
  group_by(siteSpp) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-siteSppYr, -siteSpp, -time_diff)

save(fh_sum_champ_2017, file = "data/fh_sum_champ_2017.rda")
#use_data(fh_sum_champ_2017,
#         version = 2,
#         overwrite = T)

#-----------------------------------------------------------------
# summer juvenile data with DASH metrics 2014-2017
#-----------------------------------------------------------------
load(paste0(in_path,"fish_sum_est.rda"))
load(paste0(in_path,"champ_dash.rda"))
# for temperature metrics
load(paste0(in_path,"champ_temps.rda"))

fh_sum_dash_2014_17 = fish_sum_est %>%
  filter(Valid) %>%
  filter(!is.na(FishSiteLength)) %>%
  filter(Year >= 2014) %>%
  rename(fishSampDate = SampleDate) %>%
  select(-Stream) %>%
  inner_join(champ_dash %>%
               rename(habSampDate = SampleDate,
                      Year = VisitYear) %>%
               filter(VisitObjective == 'Primary Visit',
                      VisitStatus == 'Released to Public')) %>%
  unite(siteSppYr, Site, Species, Year, remove = F) %>%
  mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
         time_diff = abs(as.integer(time_diff))) %>%
  mutate(fish_dens = N / FishSiteLength) %>%
  group_by(siteSppYr) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  filter(time_diff == min(time_diff, na.rm = T)) %>%
  ungroup()

# # join with NorWeST temperature data
# temp_df = read_csv('data/prepped/VisitID_Norwest.csv') %>%
#   select(-starts_with('X')) %>%
#   rename(NorWeST_area = NrWST_r) %>%
#   select(-c(FID_2:TAILWAT, Y_COORD:BFI, GNIS_NA:COMID))
# # correct some names that ArcGIS messed up
# names(temp_df)[-c(1:9, 46)] = names(int_crb_temp)[c(19:54)]
# 
# fh_sum_dash_2014_17 %<>%
#   left_join(temp_df %>%
#               inner_join(champ_dash %>%
#                            select(VisitID, Site, VisitYear)) %>%
#               select(VisitID, Site, VisitYear, Avg_aug_temp = S2_02_11))

# add some temperature data
fh_sum_dash_2014_17 %<>%
  left_join(fh_sum_dash_2014_17 %>%
              select(VisitID, Year) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(VisitID, avg_aug_temp = S2_02_11))) %>%
  left_join(fh_sum_dash_2014_17 %>%
              select(VisitID, Year) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(Site:VisitID, S1_93_11:S36_2015) %>%
                          gather(scenario, aug_temp, S1_93_11:S36_2015) %>%
                          mutate(Year = str_sub(scenario, -4)) %>%
                          mutate_at(vars(Year),
                                    list(as.numeric)) %>%
                          filter(!is.na(Year)) %>%
                          select(Site:VisitID, Year, aug_temp)))


# for each site, pull out the year with the highest fish density
fh_sum_dash_2014_17 %<>%
  unite(siteSpp, Site, Species, remove = F) %>%
  group_by(siteSpp) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-siteSppYr, -siteSpp, -time_diff)

use_data(fh_sum_dash_2014_17,
         version = 2,
         overwrite = T)


#-----------------------------------------------------------------
# redd data, using CHaMP 2011-2017
#-----------------------------------------------------------------
# use habitat data averaged across all CHaMP surveys

load(paste0(in_path,"redds_site_max.rda"))
load(paste0(in_path,"champ_site_2011_17.rda"))
load(paste0(in_path,"champ_site_2011_17_avg.rda"))
load(paste0(in_path,"champ_cu.rda"))


# # match year with max redds with CHaMP survey in closest year
# fh_redds_champ_2017 = redds_site_max %>%
#   mutate(fishSampDate = ymd(paste0(maxYr, '0715'))) %>%
#   inner_join(champ_site_2011_17 %>%
#                rename(habSampDate = SampleDate,
#                       Year = VisitYear) %>%
#                filter(VisitObjective == 'Primary Visit',
#                       VisitStatus == 'Released to Public'),
#              by = c('Site',
#                     'Watershed')) %>%
#   mutate(siteSpp = paste(Site, Species, sep = '_')) %>%
#   mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
#          time_diff = abs(as.integer(time_diff))) %>%
#   group_by(siteSpp) %>%
#   filter(time_diff == min(time_diff, na.rm = T)) %>%
#   ungroup()
# 
# fh_redds_champ_2017 %>%
#   group_by(Species, maxYr) %>%
#   summarise(nSurv = n(),
#             nDiffYr = sum(maxYr != year(habSampDate))) %>%
#   filter(nDiffYr < nSurv)
# 
# redds_site_max %>%
#   anti_join(fh_redds_champ_2017)


# match year with max redds to average CHaMP values and average max depth from cu data
fh_redds_champ_2017 = redds_site_max %>%
  inner_join(champ_site_2011_17_avg) %>%
  mutate(fish_dens = maxReddsPerKm / 1000) %>%
  left_join( champ_cu <- champ_cu %>%
               group_by(Site, VisitID) %>%
               mutate(Dpth_Max_Pool = ifelse(
                 Tier2 %in% c("Scour Pool", "Plunge Pool", "Dam Pool", "Beaver Pool"), Dpth_Max, NA)) %>%
               summarise(Dpth_Max_Avg = mean(Dpth_Max_Pool, na.rm=T)) %>%
               na_if(-Inf) %>%
               ungroup() %>%
               group_by(Site) %>%
               summarise(Dpth_Max_Avg = mean(Dpth_Max_Avg, na.rm=T))
  )
  

redds_site_max %>%
  anti_join(fh_redds_champ_2017)

# add average august temperature data
fh_redds_champ_2017 %<>%
  left_join(fh_redds_champ_2017 %>%
              select(Site, Watershed) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(Site, Watershed, avg_aug_temp = S2_02_11) %>%
                          distinct()))


# save as R data object
save(fh_redds_champ_2017, file = "data/fh_redds_champ_2017.rda")
#use_data(fh_redds_champ_2017,
#         version = 2,
#         overwrite = T)


#-----------------------------------------------------------------
# winter juvenile data
#-----------------------------------------------------------------
load(paste0(in_path,"fish_win_est.rda"))
load(paste0(in_path,"champ_cu.rda"))
load(paste0(in_path,"champ_site_2011_17.rda"))
#dash_covs = read_csv("data/DASH_compat_cov.csv")


# focus on latest channel unit data
# make some things match the fish data
cu_df = champ_cu %>%
  mutate(Tier1 = recode(Tier1,
                        'Fast-NonTurbulent/Glide' = 'Run',
                        'Fast-Turbulent' = 'Riffle',
                        'Slow/Pool' = 'Pool',
                        'Small Side Channel' = 'SSC')) %>%
  left_join(champ_site_2011_17 %>%
              filter(VisitObjective == 'Primary Visit',
                     VisitStatus == 'Released to Public') %>%
              #by = c("Site","VisitID")) %>%
              #select(Watershed, VisitYear,
              #       SampleDate,
              #       Channel_Type, 
              #       FishCovNone,
              #       c(dash_covs[dash_covs$DASH_comp==1,]$Metric[-c(10,19,34,35)])) %>%
              select(Site, Watershed, VisitID, VisitYear,
                     SampleDate,
                     Channel_Type, Elev_M, CUMDRAINAG, 
                     LON_DD, LAT_DD,
                     DistPrin1,
                     NatPrin1,
                     NatPrin2,
                     Discharge = Q,
                     Lgth_Wet,
                     Area_Wet,
                     CU_Freq,
                     Sin,
                     SubD50,
                     WetBraid,
                     Grad,
                     Elev_M,
                     Cond)) %>%
              # select(VisitID, 
              #        Site, 
              #        Watershed,
              #        SampleDate,
              #        CUMDRAINAG,
              #        SubD50,
              #        Sin,
              #        CU_Freq)) %>%
  group_by(Site, ChUnitNumber) %>%
  filter(SampleDate == max(SampleDate, na.rm = T)) %>%
  ungroup() %>%
  mutate(FishCovSome = 100 - FishCovNone,
         UcutArea_Pct = UcutArea_Pct * 100) %>%
  # add cobble and boulder substrate together
  mutate(SubEstCandBldr = SubEstCbl + SubEstBldr) %>%
  mutate(LWDens = LWCount/AreaTotal)


fh_win_champ_2017 = fish_win_est %>%
  rename(fishSampDate = SampleDate,
         Tier1_fish = Tier1,
         Tier2_fish = Tier2,
         Discharge_fish = Q) %>%
  select(-Stream) %>%
  inner_join(cu_df %>%
               rename(habSampDate = SampleDate) %>%
               mutate_at(vars(ChUnitNumber),
                         list(as.character))) %>%
  mutate(fish_dens = N / AreaTotal) %>%
  mutate_at(vars(Watershed, Tier1, Tier2),
            list(as.factor))

save(fh_win_champ_2017, file = "data/fh_win_champ_2017.rda")
#usethis::use_data(fh_win_champ_2017,
#                  version = 2,
#                  overwrite = T)



cu_fish_summary = fish_win_est %>%
  group_by(Site) %>%
  summarise(cu_count = length(unique(ChUnitNumber))) %>%
  left_join( dat <- champ_site_2011_17[,c(2,145)]) %>%
  mutate(cu_diff = ChnlUnitTotal_Ct - cu_count)




#-----------------------------------------------------------------------------
#Combine all data into a list, alter some metrics
#-----------------------------------------------------------------------------

fish_hab_list = list('Redds' = fh_redds_champ_2017 %>%
                       mutate_at(vars(Watershed),
                                 list(as.factor)) %>%
                       # what kind of redd density metric should we use?
                       mutate(fish_dens = maxReddsPerMsq),
                     'Winter' = fh_win_champ_2017 %>%
                       filter(!is.na(fish_dens)) %>%
                       mutate_at(vars(Watershed, Year, Tier1),
                                 list(as.factor)),
                     'Summer_CHaMP' = fh_sum_champ_2017 %>%
                       mutate_at(vars(Watershed, Year),
                                 list(as.factor)),
                     'Summer_DASH' = fh_sum_dash_2014_17 %>%
                       mutate_at(vars(Watershed, Year),
                                 list(as.factor)))

# alter a few metrics consistently across all datasets
fish_hab_list %<>%
  map(.f = function(x) {
    # scale some metrics by site length
    x %>%
      mutate_at(vars(starts_with('LWVol'),
                     ends_with('_Vol')),
                list(~ . / Lgth_Wet * 100))
    
    # add a metric showing "some" riparian canopy
    if("RipCovCanNone" %in% names(x)) {
      x %<>%
        mutate(RipCovCanSome = 100 - RipCovCanNone)
    }
    
    # add a metric showing "some" fish cover
    if("FishCovNone" %in% names(x)) {
      x %<>%
        mutate(FishCovSome = 100 - FishCovNone)
    }
    
    return(x)
  })

save(fish_hab_list, file = "data/fish_hab_list.rda")
