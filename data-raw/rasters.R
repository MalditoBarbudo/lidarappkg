## code to prepare `rasters`

# libraries
library(raster)
# library(stars)
library(tidyverse)
library(leaflet)

# read data
# regions_polygons <- sf::read_sf('../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   # sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   dplyr::select(admin_region = NOMCOMAR, geometry)
#
#
# basal_area_stars <- stars::read_stars(
#   'data-raw/AB.tif', proxy = TRUE
# ) %>%
#   aggregate(regions_polygons)
#
#
#
# plot(basal_area_stars_leaflet)

basal_area_raster <- raster::raster(
  'data-raw/AB.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/AB_aggregated100.grd')

total_aerial_biomass_raster <- raster::raster(
  'data-raw/BAT.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/BAT_aggregated100.grd')

leaf_biomass_raster <- raster::raster(
  'data-raw/BF.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/BF_aggregated100.grd')

total_aerial_carbon_raster <- raster::raster(
  'data-raw/CAT.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/CAT_aggregated100.grd')

dbh_raster <- raster::raster(
  'data-raw/DBH.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/DBH_aggregated100.grd')

hm_raster <- raster::raster(
  'data-raw/HM.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/HM_aggregated100.grd')

rec_raster <- raster::raster(
  'data-raw/REC.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/REC_aggregated100.grd')

vae_raster <- raster::raster(
  'data-raw/VAE.tif'
) %>%
  raster::aggregate(fact = 100) %>%
  projectRasterForLeaflet('bilinear') %>%
  raster::writeRaster('data-raw/VAE_aggregated100.grd')

# leaflet tests
palette <- colorNumeric(
  viridis::viridis(100),
  # raster::values(basal_area_raster),
  raster::values(rec_raster),
  na.color = 'transparent'
)

leaflet() %>%
  leaflet::setView(0.74, 41.70, zoom = 8) %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief, group = 'Relief') %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Imaginery') %>%
  # addRasterImage(basal_area_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'basal_area') %>%
  # addRasterImage(dbh_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'dbh') %>%
  # addRasterImage(hm_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'hm') %>%
  # addRasterImage(leaf_biomass_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'leaf_biomass') %>%
  addRasterImage(rec_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'rec') %>%
  # addRasterImage(total_aerial_biomass_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'total_aerial_biomass') %>%
  # addRasterImage(total_aerial_carbon_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'total_aerial_carbon') %>%
  # addRasterImage(vae_raster, project = FALSE, colors = palette, opacity = 0.8, group = 'vae') %>%
  leaflet::addLayersControl(
    baseGroups = c('Relief', 'Imaginery'),
    overlayGroups = c('basal_area', 'dbh', 'hm', 'leaf_biomass', 'rec',
                      'total_aerial_biomass', 'total_aerial_carbon', 'vae'),
    options = leaflet::layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend(pal = palette, values = raster::values(rec_raster))









# ggplot() +
#   geom_stars(data = basal_area_raster)


# usethis::use_data("rasters")
