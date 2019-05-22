## code to prepare `rasters` for visualization

# libraries ####
library(raster)
library(stars)
library(tidyverse)
library(leaflet)

# aggregated raster creation ####
# basal_area_raster <- raster::raster(
#   'data-raw/AB.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/AB_aggregated20.grd')
basal_area_raster <- raster::raster('data-raw/AB_aggregated20.grd')

# total_aerial_biomass_raster <- raster::raster(
#   'data-raw/BAT.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/BAT_aggregated20.grd')
total_aerial_biomass_raster <- raster::raster('data-raw/BAT_aggregated20.grd')

# leaf_biomass_raster <- raster::raster(
#   'data-raw/BF.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/BF_aggregated20.grd')
leaf_biomass_raster <- raster::raster('data-raw/BF_aggregated20.grd')

# total_aerial_carbon_raster <- raster::raster(
#   'data-raw/CAT.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/CAT_aggregated20.grd')
total_aerial_carbon_raster <- raster::raster('data-raw/CAT_aggregated20.grd')

# dbh_raster <- raster::raster(
#   'data-raw/DBH.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/DBH_aggregated20.grd')
dbh_raster <- raster::raster('data-raw/DBH_aggregated20.grd')

# hm_raster <- raster::raster(
#   'data-raw/HM.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/HM_aggregated20.grd')
hm_raster <- raster::raster('data-raw/HM_aggregated20.grd')

# rec_raster <- raster::raster(
#   'data-raw/REC.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/REC_aggregated20.grd')
rec_raster <- raster::raster('data-raw/REC_aggregated20.grd')

# vae_raster <- raster::raster(
#   'data-raw/VAE.tif'
# ) %>%
#   raster::aggregate(fact = 20) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/VAE_aggregated20.grd')
vae_raster <- raster::raster('data-raw/VAE_aggregated20.grd')

lidar_stack <- raster::stack(
  basal_area_raster, dbh_raster, hm_raster, leaf_biomass_raster, rec_raster,
  total_aerial_biomass_raster, total_aerial_carbon_raster, vae_raster
)

# leaflet tests ####
# palette <- colorNumeric(
#   viridis::viridis(100),
#   # raster::values(basal_area_raster),
#   raster::values(lidar_stack$DBH),
#   na.color = 'transparent'
# )
#
# leaflet() %>%
#   leaflet::setView(0.74, 41.70, zoom = 8) %>%
#   leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief, group = 'Relief') %>%
#   leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Imaginery') %>%
#   addRasterImage(lidar_stack$DBH, project = FALSE, colors = palette, opacity = 0.8, group = 'lidar') %>%
#   leaflet::addLayersControl(
#     baseGroups = c('Relief', 'Imaginery'),
#     overlayGroups = c('lidar'),
#     options = leaflet::layersControlOptions(collapsed = TRUE)
#   ) %>%
#   addLegend(pal = palette, values = raster::values(lidar_stack$DBH))

# db writing ####
# conn
conn <- RPostgreSQL::dbConnect(
  'PostgreSQL', host = 'localhost', dbname = 'lidargis', user = 'ifn',
  password = rstudioapi::askForPassword()
)
# pgPostGIS(conn)

rpostgis::pgWriteRast(
  conn, 'lidar_stack', lidar_stack, overwrite = TRUE
)

# lidar stack tests ####
# dbh <- rpostgis::pgGetRast(conn, 'lidar_stack', bands = 2)

# disconnect the db ####
RPostgreSQL::dbDisconnect(conn)


# usethis::use_data(
#   basal_area_raster, dbh_raster, hm_raster, leaf_biomass_raster, rec_raster,
#   total_aerial_biomass_raster, total_aerial_carbon_raster, vae_raster,
#   overwrite = TRUE
# )
