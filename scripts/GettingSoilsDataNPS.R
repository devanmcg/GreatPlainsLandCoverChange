# pacman::p_load(RODBC, aqp, soilDB)

pacman::p_load(tidyverse, sf, foreach, doSNOW)

{
  gp_nps_ll <- read_sf('./SpatialData/GreatPlainsNPS/NPSunitsForAnalysis', 
                           'gp_nps_LL')
  
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  AllUnits <-
    foreach(p=1:length(unique(gp_nps_ll$unit)), 
            .combine=bind_rows, 
            .packages=c('tidyverse', 'sf')) %dopar% {
              
              # Get just one feature from shp
              p_sf <-  
                gp_nps_ll %>%
                filter( unit == unit[p])   
# Fetch map units for the feature
  mukey_sf <-
    p_sf %>%
      as_Spatial() %>%
       soilDB::SDA_spatialQuery(
                        what = 'geom', 
                        db = "SSURGO", 
                        geomIntersection = TRUE) %>%
        st_as_sf()
# Define bbox to query from shp
  aoi <- 
    p_sf %>%
     as_Spatial() %>%
      rgeos::writeWKT()

# Create SQL query strings...
# ...one for ecological sites:
es_q = paste0(
  "SELECT 
    component.mukey, component.cokey, component.comppct_r, component.compname, 
    coecoclass.ecoclassname 
  FROM component 
    INNER JOIN coecoclass ON component.cokey = coecoclass.cokey
  WHERE mukey IN (
    SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", aoi, "'))")
#... another for soil texture:
tex_q = paste0(
  "SELECT
    muname, mu.mukey, -- attributes from table 'mapunit'
    comppct_r, compname, c.cokey, -- attributes from table 'component'
    ch.chkey, -- attributes from table 'chorizon'
    sandtotal_r, silttotal_r, claytotal_r --total sand, silt and clay fractions from table 'chorizon'
  FROM sacatalog sac
    INNER JOIN legend l ON l.areasymbol = sac.areasymbol AND l.areatypename = 'Non-MLRA Soil Survey Area'
    INNER JOIN mapunit mu ON mu.lkey = l.lkey
    INNER JOIN component c ON c.mukey = mu.mukey
    INNER JOIN chorizon ch ON ch.cokey = c.cokey
  WHERE mu.mukey IN (
    SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", aoi, "'))" )
# Query the Soil Data Mart
unit_data <- 
  full_join(
    # Query ecological sites
       soilDB::SDA_query(es_q) %>% 
                    as_tibble()  %>%
         select(-cokey) %>%
         group_by(mukey) %>%
         slice(which.max(comppct_r)), 
    # Query soil texture
       soilDB::SDA_query(tex_q) %>% 
                  as_tibble() %>%
       group_by(mukey, comppct_r, compname) %>%
         summarize(across(sandtotal_r:claytotal_r, ~ mean(., na.rm = T))) %>%
         ungroup() %>%
         group_by(mukey) %>%
         slice(which.max(comppct_r)), 
    by = c("mukey", "comppct_r", "compname")) %>%
 ungroup() %>% 
 rename(site = ecoclassname) %>%
  mutate(unit = unique(p_sf$unit)) %>%
 full_join(mukey_sf, .) 
          }

  stopCluster(cl)
  
  AllUnits <- 
    AllUnits %>%
    rename(sand = sandtotal_r, 
           silt = silttotal_r, 
           clay = claytotal_r)
  beepr::beep() 
  Sys.time() - begin
}



AllUnits %>%
  st_write('./SpatialData/GreatPlainsNPS/SoilTextureEcoSite/nps_soils.shp', 
           append = FALSE)

AllUnits <-
  read_sf('./SpatialData/GreatPlainsNPS/SoilTextureEcoSite', 
  'nps_soils')

# Reduce dimensionality of soil texture data 
d <- 
  AllUnits %>% 
  as_tibble() %>% 
  select(sand, silt, clay) %>%
  filter(! is.na(.))
vegan::rda(d ~1)$CA$u %>%
  as_tibble() %>%
  select(PC1) %>%
  bind_cols(AllUnits, .)


