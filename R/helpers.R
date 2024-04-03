# Call this function with an input (such as `textInput("text", NULL, "Search")`)
# if you want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[4]][[1]]$children[[1]][[1]]$children[[1]][[3]][[2]] <-
    htmltools::tagAppendChild(navbar[[4]][[1]]$children[[1]][[1]]$children[[1]][[3]][[2]], form)

  return(navbar)
}

#' requested_polys
#'
#' return the requested precalculated polys
requested_poly <- function(lidardb, poly_table, variable = 'all') {
  lidardb$get_data(poly_table, variable) |>
    sf::st_transform(crs = '+proj=longlat +datum=WGS84')
}

#' file_poly
#'
#' return the data calculated on-the-fly for the file loaded
#'
file_poly <- function(lidardb, file, lang) {

  shiny::validate(
    shiny::need(file, 'no file yet')
  )

  # check for input file format (csv (wkt) not working as it does not store the
  # crs)
  if (stringr::str_detect(file$type, 'zip')) {
    tmp_folder <- tempdir()
    utils::unzip(file$datapath, exdir = tmp_folder)

    user_polygons <- sf::st_read(
      list.files(tmp_folder, '.shp', recursive = TRUE, full.names = TRUE),
      as_tibble = TRUE
    ) |>
      sf::st_transform(crs = "+proj=longlat +datum=WGS84")

  } else {
    # gpkg
    user_polygons <- sf::st_read(file$datapath, as_tibble = TRUE) |>
      sf::st_transform(crs = "+proj=longlat +datum=WGS84")
    # poly_id <- names(user_polygons)[1]
    # # ensure polygon id is character (factors fuck it all)
    # user_polygons[[poly_id]] <- as.character(user_polygons[[poly_id]])
    # lidardb$clip_and_stats(user_polygons, poly_id, 'all')
  }

  # check for poly_id
  if (!"poly_id" %in% names(user_polygons)) {
    warning('No poly_id variable found in spatial file, using first variable found as id')
    user_polygons$poly_id <- as.character(user_polygons[[1]])

    # shiny::showModal(shiny::modalDialog(
    #   shiny::h4(translate_app("poly_id_missing_title", lang, app_translations)),
    #   translate_app("poly_id_missing_message", lang, app_translations),
    #   footer = shiny::modalButton(
    #     translate_app('dismiss', lang, app_translations)
    #   ),
    #   size = 'l', easyClose = TRUE
    # ))
    shiny::showNotification(
      ui = shiny::tagList(
        shiny::h4(translate_app("poly_id_missing_title", lang, app_translations))
      ),
      action = shiny::tagList(
        translate_app("poly_id_missing_message", lang, app_translations)
      ),
      duration = 15,
      type = "warning"
    )

  } else {
    # ensure polygon id is character (factors fuck it all)
    user_polygons$poly_id <- as.character(user_polygons$poly_id)
  }


  return(lidardb$clip_and_stats(user_polygons, "poly_id", 'all'))
}

#' drawed_poly
#'
#' return the data calculated on-the-fly for the drawed poly from leaflet
drawed_poly <- function(lidardb, custom_polygon) {

  shiny::validate(
    shiny::need(
      custom_polygon, 'no drawn polygon yet'
    ), errorClass = 'drawed_polygon_warn'
  )

  to_matrix_list <- function(data) {
    list(as.matrix(data))
  }

  user_polygons <-
    custom_polygon[['features']][[1]][['geometry']][['coordinates']] |>
    purrr::flatten() |>
    purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) |>
    dplyr::bind_rows() |>
    to_matrix_list() |>
    sf::st_polygon() |>
    sf::st_sfc() |>
    sf::st_sf(crs = "+proj=longlat +datum=WGS84") |>
    dplyr::mutate(poly_id = 'drawn polygon')

  lidardb$clip_and_stats(user_polygons, 'poly_id', 'all')
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang, app_translations) {

  # recursive call for vectors
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_app(.id, lang, app_translations)
      }
    )
    return(res)
  }

  # get id translations
  id_row <- app_translations |>
    dplyr::filter(text_id == id)

  # return raw id if no matching id found
  if (nrow(id_row) < 1) {
    warning(glue::glue("{id} not found in thesaurus"))
    return(id)
  }

  # get the lang translation
  return(dplyr::pull(id_row, glue::glue("translation_{lang}")))
}
