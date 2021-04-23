{
  pacman::p_load(tidyverse, sf, stars, rgdal, gdalUtils, raster)
  source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')
  utm13N <- "+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs"
  LL <- "+proj=longlat +datum=NAD83 +no_defs"
}

# use gdal to calculate topography from DEM
  dem_fp = 'C:/LocalProjects/FtKeoghMapping/SpatialData/DEM'
  ftk_dem <- raster(file.path(dem_fp, 'ftk_cropped.tif'))
  # Aspect
    aspect <- gdaldem(
                  mode = 'aspect',
                  input_dem = file.path(dem_fp, 'ftk_cropped.tif'),
                    output = file.path(dem_fp, 'aspect.tif'), 
                    output_Raster = TRUE)
  # Slope
    slope <- gdaldem(
                  mode = 'slope',
                  input_dem = file.path(dem_fp, 'ftk_cropped.tif'),
                  output = file.path(dem_fp, 'slope.tif'), 
                  alg = 'ZevenbergenThorne', 
                  p = TRUE,
                  s = 111120, 
                  output_Raster = TRUE)
  # Topographic roughness
    roughness <- gdaldem(
                  mode = 'roughness',
                  input_dem = file.path(dem_fp, 'ftk_cropped.tif'),
                  output = file.path(dem_fp, 'roughness.tif'), 
                  output_Raster = TRUE)
    dem_mods <- stack(aspect, slope, roughness)
    writeRaster(dem_mods, file.path(dem_fp, 'dem_mods.tif'))

# RAP data 

  rap_fp = 'S:/DevanMcG/GIS/SpatialData/RAP/FortKeogh'
  tree_1985 <- raster(file.path(rap_fp, 'VegCover1985.tif'), band = 6)
  tree_2020 <- raster(file.path(rap_fp, 'VegCover2020.tif'), band = 6)
  plot(tree_1985) 
  plot(tree_2020) 

  tree_diff <- tree_2020 - tree_1985 
  writeRaster(tree_diff, 'C:/LocalProjects/FtKeoghMapping/SpatialData/VegCover/tree_diff_85_20.tif', 
              format = 'GTiff', overwrite =T)
  plot(tree_diff)
  
  # Get Fort Keogh's ecological sites
    es_sf <-
      read_sf('S:/DevanMcG/GIS/SpatialData/FtKeogh/soils/eco_sites/ftk_es.shp', 
                        'ftk_es') %>%
        dplyr::select(MUSYM, site) %>%
          st_transform(4326) 
  # Query tree cover difference within each feature...
    es_diff <- raster::extract(tree_diff, 
                               es_sf, 
                               method = 'bilinear', 
                               fun=mean, 
                               sp = TRUE)
  # ... and convert to sf
    es_diff_sf <- es_diff %>%
                    st_as_sf() %>%
                      rename(TreeDiff = layer)

# Map changes in tree cover by ecological site 
    ggplot(es_diff_sf) + theme_map() + 
      geom_sf(aes(fill = TreeDiff), 
              color = "lightgrey") +
      scale_fill_gradient2('Mean tree cover change\n1985-2020 (%)',  
                           na.value = "#FDE725FF", 
                           low = Zpal[1], 
                           mid = "white", 
                           midpoint = 0, 
                           high = Zpal[200]) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0))

# Summarize difference in tree cover by ecological site  
    es_diff_sf %>%
      as_tibble() %>% 
      group_by(site) %>%
      summarize(Mean = mean(TreeDiff), 
                se = sd(TreeDiff)/sqrt(n())) %>%
      filter(site != 'None') %>%
      ggplot() + theme_bw(14) +
      geom_hline(yintercept = 0, lty = 3) + 
      geom_errorbar(aes(x = reorder(site, -Mean, max), 
                        ymin = Mean-se, 
                        ymax = Mean+se), 
                    width = 0.25, 
                    size = 1, 
                    color = "darkblue") + 
      geom_point(aes(x = site, 
                     y= Mean), 
                 pch = 21, size = 4, 
                 stroke = 1.25,
                 fill = "lightblue", 
                 col = "darkblue") +
      labs(x = "Ecological site", 
           y = "Mean change in tree cover\n1985-2020 (%)") +
      theme(axis.text.x = element_text(color = "black",
                                       hjust = 1, angle = 33), 
            panel.grid.major.x = element_blank())
    