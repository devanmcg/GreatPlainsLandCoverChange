pacman::p_load(tidyverse, sf)
load('./albersEAC.Rdata')

# NPS shapefiles 
gp_nps_shp <- rgdal::readOGR('./SpatialData/GreatPlainsNPS/NPSunitsForAnalysis', 
                            'gp_nps_LL') 
gp_nps_sf <- read_sf('./SpatialData/GreatPlainsNPS/NPSunitsForAnalysis', 
                     'gp_nps_LL') %>%
                st_transform(albersEAC)

pacman::p_load(foreach, doParallel)
cores=detectCores()

{
  cl <- makeCluster(cores[1]) 
  registerDoParallel(cl)
  begin = Sys.time()
  
    foreach(p=1:length(gp_nps_sf$unit)) 
    %dopar% {
      pacman::p_load(tidyverse, sf)
      p_sf <-  
        gp_nps_sf %>%
        slice(p) 
      p_sf %>%
        as_Spatial()  %>%
        elevatr::get_elev_raster(z = 14, 
                                 clip = 'bbox', 
                                 verbose = F, 
                                 override_size_check = T) %>%
        raster::writeRaster(paste0('D:/DEM/NPS/', 
                                   str_replace_all(p_sf$unit, " ", "_"), '.tif'))
            }
    beepr::beep() 
    stopCluster(cl)
    Sys.time() - begin
}   


