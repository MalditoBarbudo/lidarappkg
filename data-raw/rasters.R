## code to prepare `rasters`

# libraries
library(raster)
library(stars)
library(tidyverse)
library(leaflet)

# read data
regions_polygons <- sf::read_sf('../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  # sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_region = NOMCOMAR, geometry) %>%
  dplyr::filter(admin_region %in% c('Alt Camp', 'Anoia')) #%>%
  # sf::as_Spatial()

# basal_area_raster <- raster::raster(
#   'data-raw/AB.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/AB_aggregated100.grd')
basal_area_raster <- raster::raster('data-raw/AB_aggregated100.grd')

# total_aerial_biomass_raster <- raster::raster(
#   'data-raw/BAT.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/BAT_aggregated100.grd')
total_aerial_biomass_raster <- raster::raster('data-raw/BAT_aggregated100.grd')

# leaf_biomass_raster <- raster::raster(
#   'data-raw/BF.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/BF_aggregated100.grd')
leaf_biomass_raster <- raster::raster('data-raw/BF_aggregated100.grd')

# total_aerial_carbon_raster <- raster::raster(
#   'data-raw/CAT.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/CAT_aggregated100.grd')
total_aerial_carbon_raster <- raster::raster('data-raw/CAT_aggregated100.grd')

# dbh_raster <- raster::raster(
#   'data-raw/DBH.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/DBH_aggregated100.grd')
dbh_raster <- raster::raster('data-raw/DBH_aggregated100.grd')

# hm_raster <- raster::raster(
#   'data-raw/HM.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/HM_aggregated100.grd')
hm_raster <- raster::raster('data-raw/HM_aggregated100.grd')

# rec_raster <- raster::raster(
#   'data-raw/REC.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/REC_aggregated100.grd')
rec_raster <- raster::raster('data-raw/REC_aggregated100.grd')

# vae_raster <- raster::raster(
#   'data-raw/VAE.tif'
# ) %>%
#   raster::aggregate(fact = 100) %>%
#   projectRasterForLeaflet('bilinear') %>%
#   raster::writeRaster('data-raw/VAE_aggregated100.grd')
vae_raster <- raster::raster('data-raw/VAE_aggregated100.grd')

lidar_stack <- raster::stack(
  basal_area_raster, dbh_raster, hm_raster, leaf_biomass_raster, rec_raster,
  total_aerial_biomass_raster, total_aerial_carbon_raster, vae_raster
)

# leaflet tests
palette <- colorNumeric(
  viridis::viridis(100),
  # raster::values(basal_area_raster),
  raster::values(lidar_stack$DBH),
  na.color = 'transparent'
)

leaflet() %>%
  leaflet::setView(0.74, 41.70, zoom = 8) %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief, group = 'Relief') %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Imaginery') %>%
  addRasterImage(lidar_stack$DBH, project = FALSE, colors = palette, opacity = 0.8, group = 'lidar') %>%
  leaflet::addLayersControl(
    baseGroups = c('Relief', 'Imaginery'),
    overlayGroups = c('lidar'),
    options = leaflet::layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend(pal = palette, values = raster::values(lidar_stack$DBH))


## TODO check stars for calculating
lidar_stars <- stars::read_stars(
  list.files('data-raw', '.tif$', full.names = TRUE)[-6], proxy = TRUE
)

lidar_stars %>%
  merge() %>%
  magrittr::extract(regions_polygons) %>%
  # plot(reset = FALSE)
  stars::st_apply(3, mean, na.rm = TRUE) %>%
  stars::st_as_stars() -> foo

ggplot() +
  geom_stars(data = foo) +
  coord_equal() +
  scale_fill_viridis_c(na.value = 'transparent') +
  theme_void()

## TODO check velox, but mind about memory


usethis::use_data(
  lidar_stack,
  overwrite = TRUE
)
