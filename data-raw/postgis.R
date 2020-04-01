## Script to create the postgis tables for the rasters

# libraries
library(rpostgis)
library(sf)
library(tidyverse)
library(lfcdata)
library(tictoc)

lidardb <- lidar()


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
tic()
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
# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)
catalunya_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_catalonia',
    overwrite = TRUE
  )
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# provincias
tic()
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

# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

provincias_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_provinces',
    overwrite = TRUE
  )
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# comarcas
tic()
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

# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

sf::st_write(
  comarcas_sf %>% rmapshaper::ms_simplify(0.01, keep_shapes = TRUE),
  conn,
  layer = 'lidar_counties',
  overwrite = TRUE
)
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# municipios
tic()
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

# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

municipios_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_municipalities',
    overwrite = TRUE
  )
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# veguerias
tic()
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

# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

veguerias_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_vegueries',
    overwrite = TRUE
  )
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# ENPES
tic()
enpes_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/enpe_2017.shp'
) %>%
  dplyr::select(poly_id = nom, geometry) %>%
  sf::st_set_crs(value = 3043) %>%
  dplyr::mutate(dummy = poly_id) %>%
  dplyr::group_by(poly_id) %>%
  dplyr::summarise(dummy = dplyr::first(dummy)) %>%
  dplyr::select(-dummy) %>%
  sf::st_cast('MULTIPOLYGON')

enpes_sf <- lidardb %>%
  lidar_clip_and_stats(
    enpes_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

# conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

enpes_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_enpes',
    overwrite = TRUE
  )
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# PEIN
tic()
pein_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/pein_2017.shp'
) %>%
  dplyr::select(poly_id = nom, geometry) %>%
  sf::st_set_crs(value = 3043) %>%
  dplyr::mutate(dummy = poly_id) %>%
  dplyr::group_by(poly_id) %>%
  dplyr::summarise(dummy = dplyr::first(dummy)) %>%
  dplyr::select(-dummy) %>%
  sf::st_cast('MULTIPOLYGON')

pein_sf <- lidardb %>%
  lidar_clip_and_stats(
    pein_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

pein_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_pein',
    overwrite = TRUE
  )
# disconnect the db
RPostgres::dbDisconnect(conn)
toc()

# xn2000
tic()
xn2000_polys <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/xn2000_2017.shp'
) %>%
  dplyr::select(poly_id = nom_n2, geometry) %>%
  sf::st_set_crs(value = 3043) %>%
  dplyr::mutate(dummy = poly_id) %>%
  dplyr::group_by(poly_id) %>%
  dplyr::summarise(dummy = dplyr::first(dummy)) %>%
  dplyr::select(-dummy) %>%
  sf::st_cast('MULTIPOLYGON')

xn2000_sf <- lidardb %>%
  lidar_clip_and_stats(
    xn2000_polys, 'poly_id',
    variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
  )

conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.uab.cat', dbname = 'lidargis',
  user = 'guest',
  password = 'guest'
)

xn2000_sf %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  sf::st_write(
    conn,
    layer = 'lidar_xn2000',
    overwrite = TRUE
  )

# disconnect the db
RPostgres::dbDisconnect(conn)
toc()
