#' Clip and summarise polygons
#'
#' lidar_clip clips polygons on to the lidar variables rasters to get the mean value
#'
#' @param sf sf object with the polygons to clip
#' @param vars character vector with the lidar variables to summarise
#' @param lidar_db dbi connection
#' @param poly_id sf column name containing the polygon identificator, as character
#' @param safe logical indicating if memory and time safeguards are active
#' @param ... not implemented
#'
#' @return a sf data frame with the variables mean (each var a column) for each polygon
#'   (rows) as well as the geometry column
#'
#' @export
lidar_clip <- function(
  sf, vars = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'), lidar_db, poly_id,
  safe = TRUE,
  ...
) {

  # check if sf
  if (!inherits(sf, 'sf')) {
    stop('sf is not and sf object')
  }
  # check if vars
  if (!all(vars %in% c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'))) {
    stop('one or more vars provided are not allowed. See help ?lidar_clip for more detail')
  }

  # first step, load the temp sf table
  user_polygons <- sf %>%
    sf::st_transform(crs = 3043) %>%
    sf::st_set_crs(3043) %>% {
      temp_data <- .
      if ('geom' %in% names(temp_data)) {
        temp_data <- dplyr::rename(temp_data, geometry = geom)
      }
      temp_data
    }

  ## Important checks for area, number of features... ####
  if (isTRUE(safe)) {
    # area check
    user_area <- sf::st_area(user_polygons) %>% sum() %>% as.numeric()
    if (user_area > 500000000) {
      stop(glue::glue(
        'Polygon area (or polygons sum of areas) are above the maximum value ({user_area} > 500 km2)'
      ))
    }
    # feature number
    user_features <- sf::st_geometry(user_polygons) %>% length()
    if (user_features > 10) {
      stop(glue::glue(
        'Number of features (polygons) is above the maximum value ({user_features} > 10 polygons)'
      ))
    }
  }

  # second step, write the temp table to the db, only after the checks are done
  sf::st_write(user_polygons, lidar_db, overwrite = TRUE)

  # build the query/queries with glue to be able to insert the poly id column and
  # the variable rasters
  lidar_query <- glue::glue(
  "WITH
     feat AS (SELECT {poly_id} As poly_id, geometry FROM user_polygons AS b),
     b_stats AS (SELECT poly_id, geometry, (stats).* FROM (
       SELECT poly_id, geometry, ST_SummaryStats(ST_Clip(rast,1,geometry, true)) As stats
         FROM public.{tolower(vars)}
       INNER JOIN feat
       ON ST_Intersects(feat.geometry,rast)
     ) As foo)
   SELECT poly_id, geometry, SUM(mean*count)/SUM(count) As mean_{vars} FROM b_stats
     WHERE count > 0
   GROUP BY poly_id, geometry
   ORDER BY poly_id;")

  # get and join the results
  res <- lidar_query %>%
    purrr::map(
      ~ sf::st_read(lidar_db, query = .x) %>% dplyr::as_tibble()
    ) %>%
    purrr::reduce(dplyr::left_join, by = c('poly_id')) %>%
    dplyr::select(poly_id, starts_with('mean_'), geometry = geometry.x) %>%
    sf::st_as_sf()

  return(res)
}
