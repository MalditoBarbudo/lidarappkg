## Script to create the postgis tables for the rasters

# libraries
library(rpostgis)
library(sf)
library(tidyverse)
library(lfcdata)

lidardb <- lidar()

# conn
# conn <- RPostgres::dbConnect(
#   RPostgres::Postgres(), host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
#   user = 'ifn',
#   password = rstudioapi::askForPassword()
# )
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
# Indexes
# c('ab', 'bat', 'bf', 'cat', 'dbh', 'hm', 'rec', 'vae') %>%
#   purrr::walk(
#     ~dbExecute(
#       conn,
#       glue::glue("CREATE INDEX {.x}_rast_st_convexhull_idx ON {.x} USING gist( ST_ConvexHull(rast) );")
#     )
#   )

## pre-calculated data for known polygons ####
# catalunya
catalunya_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/catalunya.shp'
) %>%
  dplyr::select(poly_id = NOM_CA, geometry) %>%
  sf::st_set_crs(value = 3043)

catalunya_sf <- lidardb %>%
  lidar_clip_and_stats(
    catalunya_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  catalunya_sf %>% rmapshaper::ms_simplify(0.05),
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_catalonia',
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
  provincias_sf %>% rmapshaper::ms_simplify(0.05),
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_provinces',
  overwrite = TRUE
)

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
  comarcas_sf %>% rmapshaper::ms_simplify(0.05),
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_counties',
  overwrite = TRUE
)

# municipios
municipios_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMMUNI, geometry) %>%
  sf::st_set_crs(value = 3043)

municipios_sf <- lidardb %>%
  lidar_clip_and_stats(
    municipios_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  municipios_sf %>% rmapshaper::ms_simplify(0.05),
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_municipalities',
  overwrite = TRUE
)

# veguerias
veguerias_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpv1_20180101_0.shp'
) %>%
  dplyr::select(poly_id = NOMVEGUE, geometry) %>%
  sf::st_set_crs(value = 3043)

veguerias_sf <- lidardb %>%
  lidar_clip_and_stats(
    veguerias_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  veguerias_sf %>% rmapshaper::ms_simplify(0.05),
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_vegueries',
  overwrite = TRUE
)

# ENPES
enpes_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/enpe_2017.shp'
) %>%
  dplyr::select(poly_id = nom, geometry) %>%
  sf::st_set_crs(value = 3043)

enpes_sf <- lidardb %>%
  lidar_clip_and_stats(
    enpes_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  enpes_sf,
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_enpes',
  overwrite = TRUE
)

# PEIN
pein_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/pein_2017.shp'
) %>%
  dplyr::select(poly_id = nom, geometry) %>%
  sf::st_set_crs(value = 3043)

pein_sf <- lidardb %>%
  lidar_clip_and_stats(
    pein_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  pein_sf,
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_pein',
  overwrite = TRUE
)

# xn2000
xn2000_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/xn2000_2017.shp'
) %>%
  dplyr::select(poly_id = nom_n2, geometry) %>%
  sf::st_set_crs(value = 3043)

xn2000_sf <- lidardb %>%
  lidar_clip_and_stats(
    xn2000_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

sf::st_write(
  xn2000_sf,
  lidardb$.__enclos_env__$private$pool_conn,
  layer = 'lidar_xn2000',
  overwrite = TRUE
)

# disconnect the db
# RPostgres::dbDisconnect(conn)
