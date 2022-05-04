#Author: Mark Roes
#Purpose: Prepare and modify habitat dictionary for QRF model
#Created: 5/4/2022
#Modified:

in_path = 'S:/main/data/qrf/gitrepo_data/input/'

#Read in 2017 habitat dictionary

#-----------------------------------------------------------------
# and the appropriate habitat dictionary to go with it
load(paste0(in_path,"hab_dict_2017.rda"))
hab_dict = hab_dict_2017

# change some of the descriptions for large wood volume
hab_dict %<>%
  mutate(DescriptiveText = if_else(grepl("^LWVol", ShortName),
                                   paste0(str_remove(DescriptiveText, ".$"),
                                          ", scaled by site length."),
                                   DescriptiveText),
         UnitOfMeasure = if_else(grepl("^LWVol", ShortName),
                                 paste0(UnitOfMeasure,
                                        " per 100 meters"),
                                 UnitOfMeasure),
         UnitOfMeasureAbbrv = if_else(grepl("^LWVol", ShortName),
                                      paste0(UnitOfMeasureAbbrv,
                                             "/100m"),
                                      UnitOfMeasureAbbrv)) %>%
  # add description for some riparian canopy
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "RipCovCanNone") %>%
              mutate(ShortName = "RipCovCanSome",
                     Name = "Riparian Cover: Some Canopy",
                     DescriptiveText = "Percent of riparian canopy with some vegetation.")) %>%
  # add description for no riparian ground cover
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "RipCovGrnd") %>%
              mutate(ShortName = "RipCovGrndNone",
                     Name = "Riparian Cover: No Ground",
                     DescriptiveText = "Percent of groundcover with no vegetation.")) %>%
  # add description for some fish cover
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "FishCovNone") %>%
              mutate(ShortName = "FishCovSome",
                     Name = "Fish Cover: Some Cover",
                     DescriptiveText = "Percent of wetted area with some form of fish cover")) %>%
  #Add LW density
  rbind(c("LWdens",
          "Large Wood Density",
          NA,
          "Visit Metric",
          "Large Wood per sq meter",
          "square-meter",
          "m2",
          "Cover",
          NA))




#---------------------------------------------------------------------------
#Save habitat dict to repo
save(hab_dict, file = "data/hab_dict.rda")