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

#' requested_polys
#'
#' return the requested precalculated polys
requested_poly <- function(lidardb, poly_table, variable) {
  lidardb$get_data(poly_table, variable)
}

#' file_poly
#'
#' return the data calculated on-the-fly for the file loaded
#'
file_poly <- function(lidardb, file, lang) {

  shiny::validate(
    shiny::need(file, translate_app('file_need', lang))
  )

  # browser()

  # check for input file format (csv (wkt) not working as it does not store the crs)
  if (stringr::str_detect(file$type, 'zip')) {
    tmp_folder <- tempdir()
    utils::unzip(file$datapath, exdir = tmp_folder)

    user_polygons <- sf::st_read(
      list.files(tmp_folder, '.shp', recursive = TRUE, full.names = TRUE),
      as_tibble = TRUE
    )

    poly_id <- names(user_polygons)[1]

    lidardb$clip_and_stats(user_polygons, poly_id, 'all')
  } else {
    # gpkg
    user_polygons <- sf::st_read(file$datapath, as_tibble = TRUE)
    poly_id <- names(user_polygons)[1]
    lidardb$clip_and_stats(user_polygons, poly_id, 'all')
  }
}

#' drawed_poly
#'
#' return the data calculated on-the-fly for the drawed poly from leaflet
drawed_poly <- function(lidardb, custom_polygon, lang) {

  shiny::validate(
    shiny::need(
      custom_polygon, translate_app('custom_poly_need', lang)
    ), errorClass = 'drawed_polygon_warn'
  )

  user_polygons <- custom_polygon[['features']][[1]][['geometry']][['coordinates']] %>%
    purrr::flatten() %>%
    purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
    dplyr::bind_rows() %>%
    {list(as.matrix(.))} %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = "+proj=longlat +datum=WGS84") %>%
    dplyr::mutate(poly_id = 'custom_polygon')

  lidardb$clip_and_stats(user_polygons, 'poly_id', 'all')
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  app_translations

  id %>%
    purrr::map_chr(
      ~ app_translations %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )
}
