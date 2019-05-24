# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' catalonia_poly
#'
#' return the data pre calculated for catalonia
#'
catalonia_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_catalunya')
}

#' provinces_poly
#'
#' return the data pre calculated for provinces
#'
provinces_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_provincias')
}

#' counties_poly
#'
#' return the data pre calculated for counties
#'
counties_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_comarcas')
}

#' municipalities_poly
#'
#' return the data pre calculated for municipalities
#'
municipalities_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_municipios')
}

#' veguerias_poly
#'
#' return the data pre calculated for veguerias
#'
veguerias_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_veguerias')
}


#' file_poly
#'
#' return the data calculated on-the-fly for the file loaded
#'
file_poly <- function(lidar_db, file, poly_id) {

  shiny::validate(
    shiny::need(file, 'no file selected')
  )

  # browser()

  # check for input file format

  ## csv (wkt) not working as it does not store the crs
  # if (stringr::str_detect(file$type, 'csv')) {
  #   sf::st_read(file$datapath, as_tibble = TRUE) %>%
  #     lidar_clip(lidar_db = lidar_db, poly_id = names(.)[1])
  # }
  if (stringr::str_detect(file$type, 'zip')) {
    tmp_folder <- tempdir()
    utils::unzip(file$datapath, exdir = tmp_folder)

    sf::st_read(
      list.files(tmp_folder, '.shp', recursive = TRUE, full.names = TRUE),
      as_tibble = TRUE
    ) %>%
      lidar_clip(lidar_db = lidar_db, poly_id = names(.)[1])
  } else {
    # gpkg
    sf::st_read(file$datapath, as_tibble = TRUE) %>%
      lidar_clip(lidar_db = lidar_db, poly_id = names(.)[1])
  }
}


#' drawed_poly
#'
#' return the data calculated on-the-fly for the drawed poly from leaflet
drawed_poly <- function(lidar_db, custom_polygon) {

  shiny::validate(
    shiny::need(custom_polygon, 'no custom polygon drawed')
  )

  custom_polygon[['features']][[1]][['geometry']][['coordinates']] %>%
    purrr::flatten() %>%
    purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
    dplyr::bind_rows() %>%
    {list(as.matrix(.))} %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = "+proj=longlat +datum=WGS84") %>%
    dplyr::mutate(poly_id = 'custom_polygon') %>%
    lidar_clip(lidar_db = lidar_db, poly_id = 'poly_id')
}
