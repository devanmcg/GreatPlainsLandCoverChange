pacman::p_load(tidyverse, sf)
load('./albersEAC.Rdata')

# NPS shapefiles 
gp_nps_aEAC <- read_sf('./SpatialData/GreatPlainsNPS/NPSunitsForAnalysis', 
                     'gp_nps_LL') %>%
                st_transform(albersEAC)

samp_sf <- read_sf('./SpatialData/GreatPlainsNPS/SamplePoints', 
                              'NPSSamplePoints') %>%
              st_transform(albersEAC)

{
  pacman::p_load(foreach, doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]) 
  registerDoParallel(cl)
  begin = Sys.time()
  
  PointsTopoData <- 
    foreach(p=1:length(unique(samp_sf$unit)), 
            .combine=bind_rows, 
            .packages = c('tidyverse', 'sf')) %dopar% {

      p_sf <-  
        samp_sf %>%
        filter(unit == unique(samp_sf$unit)[p]) 
      dem <- 
        p_sf %>%
          as_Spatial()  %>%
          elevatr::get_elev_raster(z = 13, 
                                   clip = 'bbox', 
                                   verbose = F, 
                                   override_size_check = T) 
       
     full_join(
      terra::terrain(dem, 'slope') %>% 
       terra::rast() %>%
       terra::extract(., 
                      terra::vect(p_sf), 
                      fun = 'mean', 
                      method = 'bilinear', 
                      na.rm = TRUE, 
                      df = TRUE) %>%
          as_tibble() %>%
          rename(point = ID), 
      terra::terrain(dem, 'aspect', 'degrees') %>%
         terra::rast() %>%
         terra::extract(., 
                        terra::vect(p_sf), 
                        fun = 'mean', 
                        method = 'bilinear', 
                        na.rm = TRUE, 
                        df = TRUE) %>%
         as_tibble() %>%
         rename(point = ID) , 
      by = 'point') %>%
       mutate(unit = unique(p_sf$unit)) 
            }
    beepr::beep() 
    stopCluster(cl)
    Sys.time() - begin
}   

# save(PointsTopoData, file = './SpatialData/GreatPlainsNPS/PointsTopoData.Rdata')

