## Script to create the postgis tables for the rasters

# libraries
library(rpostgis)
library(sf)
library(tidyverse)
library(lidarappkg)

# conn
conn <- RPostgreSQL::dbConnect(
  'PostgreSQL', host = 'localhost', dbname = 'lidargis', user = 'ifn',
  password = rstudioapi::askForPassword()
)
# pgPostGIS(conn)

## tables creation ####
# Read the rasters, write the tables and get the raster list for tests
# list.files('data-raw', '.tif$', full.names = TRUE) %>%
#   purrr::map(raster::raster) %>%
#   magrittr::set_names(value = list.files('data-raw', '.tif$')) %>%
#   purrr::iwalk(
#     ~ rpostgis::pgWriteRast(
#       conn,
#       name = c('public', tolower(stringr::str_remove(.y, '\\.tif'))),
#       raster = .x,
#       blocks = 50, overwrite = TRUE
#     )
#   ) -> lidar_rasters

## pre-calculated data for known polygons ####

# comarcas
sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMCOMAR, geometry) %>%
  lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
  rmapshaper::ms_simplify(0.01) -> lidar_comarcas
sf::st_write(lidar_comarcas, conn, overwrite = TRUE)
# municipios
sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMMUNI, geometry) %>%
  lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
  rmapshaper::ms_simplify(0.01) -> lidar_municipios
sf::st_write(lidar_municipios, conn, overwrite = TRUE)
# provincias
sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMPROV, geometry) %>%
  lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
  rmapshaper::ms_simplify(0.01) -> lidar_provincias
sf::st_write(lidar_provincias, conn, overwrite = TRUE)
# veguerias
sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpv1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMVEGUE, geometry) %>%
  lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
  rmapshaper::ms_simplify(0.01) -> lidar_veguerias
sf::st_write(lidar_veguerias, conn, overwrite = TRUE)
# cataluña
sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/catalunya.shp'
) %>%
  dplyr::select(poly_id = NOM_CA, geometry) %>%
  lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
  rmapshaper::ms_simplify(0.01) -> lidar_catalunya
sf::st_write(lidar_catalunya, conn, overwrite = TRUE)



## Tests ####
# regions_polygons <- sf::read_sf('../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
#   # rmapshaper::ms_simplify(0.01) %>%
#   # sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   dplyr::select(admin_region = NOMCOMAR, geometry) %>%
#   # dplyr::filter(admin_region %in% c('Alt Camp', 'Anoia')) %>%
#   sf::st_set_crs(3043)
#
# # this write the temp table with the polygons
# sf::st_write(regions_polygons, conn, overwrite = TRUE)
#
# # the query
# query <- "
# WITH
# -- our features of interest
#    feat AS (SELECT admin_region As comarca_id, geometry As geom FROM regions_polygons AS b),
# -- clip band 1 of raster tiles to boundaries of builds
# -- then get stats for these clipped regions
#    b_stats AS
# 	(SELECT  comarca_id, geom, (stats).*
# FROM (SELECT comarca_id, geom, ST_SummaryStats(ST_Clip(rast,1,geom,true)) As stats
#     FROM public.ab
# 		INNER JOIN feat
# 	ON ST_Intersects(feat.geom,rast)
#  ) As foo
#  )
# -- finally summarize stats
# SELECT comarca_id, geom, SUM(mean*count)/SUM(count) As mean_tiles
# 	FROM b_stats
#  WHERE count > 0
# 	GROUP BY comarca_id, geom
# 	ORDER BY comarca_id;
# "
#
# # get the res
# foo <- st_read(conn, query = query)
# foo

# disconnect the db
RPostgreSQL::dbDisconnect(conn)
