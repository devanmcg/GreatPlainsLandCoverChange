pacman::p_load(tidyverse, sf)

source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')
Zpal <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")



# Get Great Plains shp 
gp_sf <- 
  read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3', 
          'us_eco_l3_state_boundaries') %>%
  filter(NA_L1NAME == 'GREAT PLAINS')

albersEAC <- st_crs(gp_sf)
save(albersEAC, file = './albersEAC.Rdata')


gp_nps <- 
  bind_rows(
    read_sf('S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/R5', 
            'PADUS2_1Proclamation_Region5') %>%
      filter(Mang_Name == 'NPS'), 
    read_sf('S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/R6', 
            'PADUS2_1Proclamation_Region6') %>%
    filter(Mang_Name == 'NPS'),  
  read_sf('S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/R7', 
          'PADUS2_1Proclamation_Region7') %>%
  filter(Mang_Name == 'NPS') ) %>%
    st_intersection(gp_sf)


gp_nps <- 
  gp_nps %>%
    mutate(area = st_area(.) / 1000) %>% 
    group_by(Unit_Nm) %>%
    summarise(area = sum(area)) %>%
    mutate(area = as.numeric(area)) %>%
    arrange((area)) %>%
    rename(unit = Unit_Nm) %>%
    dplyr::filter(area >= 500) %>%
    dplyr::filter(! unit %in% c('Padre Island National Seashore'))


# gp_nps %>% save(file='./SpatialData/gp_nps.Rdata')

# Wrangling layer for vegetation analysis
gp_nps_sf <- read_sf('./SpatialData/GreatPlainsNPS', 
                     'gp_nps_LL') %>%
  select(-area) %>%
  filter(! unit %in% c("Niobrara National Scenic River", 
                       "Lake Meredith National Recreation Area",            
                       "Missouri National Recreation River") ) 
gp_nps_sf <- 
  gp_nps_sf %>% 
  filter(unit == "Theodore Roosevelt National Park") %>%
  st_cast(to= "POLYGON", 
          group_or_split=TRUE, 
          warn = FALSE) %>%
  mutate(unit = paste0(unit, c(' Elkhorn', ' South', ' North'))) %>%
  bind_rows(filter(gp_nps_sf, unit != "Theodore Roosevelt National Park"))

gp_nps_sf %>%
  st_write('./SpatialData/GreatPlainsNPS/NPSunitsForAnalysis/gp_nps_LL.shp')
  
