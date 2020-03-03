## Script to create the postgis tables for the rasters

# libraries
library(rpostgis)
library(sf)
library(tidyverse)
library(lfcdata)

lidardb <- lidar()

# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(), host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'ifn',
  password = rstudioapi::askForPassword()
)
# pgPostGIS(conn)

## tables creation ####
# Read the rasters, write the tables and get the raster list for tests
list.files('data-raw', '.tif$', full.names = TRUE) %>%
  purrr::map(raster::raster) %>%
  magrittr::set_names(value = list.files('data-raw', '.tif$')) %>%
  purrr::iwalk(
    ~ rpostgis::pgWriteRast(
      conn,
      name = c('public', tolower(stringr::str_remove(.y, '\\.tif'))),
      raster = .x,
      blocks = 50, overwrite = TRUE
    )
  ) -> lidar_rasters
# Indexes
# c('ab', 'bat', 'bf', 'cat', 'dbh', 'hm', 'rec', 'vae') %>%
#   purrr::walk(
#     ~dbExecute(
#       conn,
#       glue::glue("CREATE INDEX {.x}_rast_st_convexhull_idx ON {.x} USING gist( ST_ConvexHull(rast) );")
#     )
#   )

## pre-calculated data for known polygons ####


# comarcas
comarcas_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMCOMAR, geometry) %>%
  sf::st_set_crs(value = 3043)

comarcas_sf <- lidardb %>%
  lidar_clip_and_stats(
    comarcas_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  comarcas_sf, lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_counties',
  overwrite = TRUE
)

# provincias
provincias_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMPROV, geometry) %>%
  sf::st_set_crs(value = 3043)

provincias_sf <- lidardb %>%
  lidar_clip_and_stats(
    provincias_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  provincias_sf, lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_provinces',
  overwrite = TRUE
)

# municipios
municipios_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMMUNI, geometry) %>%
  sf::st_set_crs(value = 3043) %>%
  dplyr::slice(172)

municipios_sf <- lidardb %>%
  lidar_clip_and_stats(
    municipios_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  municipios_sf, lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_municipalities',
  overwrite = TRUE
)


# # provincias
# sf::read_sf(
#   '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp'
# ) %>%
#   dplyr::select(poly_id = NOMPROV, geometry) %>%
#   lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
#   rmapshaper::ms_simplify(0.01) -> lidar_provincias
# sf::st_write(lidar_provincias, conn, overwrite = TRUE)
# # municipios
# admin_thes <- sf::read_sf('../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp') %>%
#   dplyr::as_tibble() %>%
#   dplyr::select(-geometry) %>%
#   dplyr::left_join(
#     sf::read_sf('../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp') %>%
#       dplyr::as_tibble() %>%
#       dplyr::select(NOMPROV, CODIPROV)
#   ) %>%
#   dplyr::left_join(
#     sf::read_sf('../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
#       dplyr::as_tibble() %>%
#       dplyr::select(NOMCOMAR, CODICOMAR)
#   ) %>%
#   dplyr::select(
#     poly_id = NOMMUNI, Counties = NOMCOMAR, Provinces = NOMPROV
#   )
# sf::read_sf(
#   '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp'
# ) %>%
#   dplyr::select(poly_id = NOMMUNI, geometry) %>%
#   lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   dplyr::left_join(admin_thes, by = 'poly_id') %>%
#   dplyr::select(poly_id, Counties, Provinces, everything()) -> lidar_municipios
# sf::st_write(lidar_municipios, conn, overwrite = TRUE)
# veguerias
# sf::read_sf(
#   '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpv1_20180101_0.shp'
# ) %>%
#   dplyr::select(poly_id = NOMVEGUE, geometry) %>%
#   lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
#   rmapshaper::ms_simplify(0.01) -> lidar_veguerias
# sf::st_write(lidar_veguerias, conn, overwrite = TRUE)
# # cataluña
# sf::read_sf(
#   '../../01_nfi_app/NFIappkg/data-raw/shapefiles/catalunya.shp'
# ) %>%
#   dplyr::select(poly_id = NOM_CA, geometry) %>%
#   lidar_clip(lidar_db = conn, poly_id = 'poly_id', safe = FALSE) %>%
#   rmapshaper::ms_simplify(0.01) -> lidar_catalunya
# sf::st_write(lidar_catalunya, conn, overwrite = TRUE)

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
RPostgres::dbDisconnect(conn)
