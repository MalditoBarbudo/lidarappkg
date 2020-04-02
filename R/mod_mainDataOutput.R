#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data as tbl_sql
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

' @title mod_mainData server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#' @param app_translations dictionary
#' @param data_reactives,map_reactives reactives needed to retrieve the data
#' @param lidardb access to lidar database
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  lang, app_translations,
  data_reactives, map_reactives,
  lidardb
) {

  # reactives ####
  data_polys <- shiny::reactive({

    progress <- shiny::Progress$new(session, min = 5, max = 100)
    on.exit(progress$close())
    progress$set(
      message = translate_app(
        'poly_data_progress_mes', lang(), app_translations
      )
    )

    shiny::validate(
      shiny::need(data_reactives$poly_type_sel, 'no poly yet')
    )

    poly_type <- data_reactives$poly_type_sel
    lidar_var <- shiny::isolate(data_reactives$lidar_var_sel)
    user_file <- shiny::isolate(data_reactives$user_file_sel)

    progress$set(value = 25)

    res <- switch(
      poly_type,
      'aut_community' = requested_poly(
        lidardb, 'lidar_catalonia'
      ),
      'province' = requested_poly(
        lidardb, 'lidar_provinces'
      ),
      'region' = requested_poly(
        lidardb, 'lidar_counties'
      ),
      'municipality' = requested_poly(
        lidardb, 'lidar_municipalities'
      ),
      'vegueria' = requested_poly(
        lidardb, 'lidar_vegueries'
      ),
      "natural_interest_area" = requested_poly(
        lidardb, 'lidar_pein'
      ),
      "special_protection_natural_area" = requested_poly(
        lidardb, 'lidar_enpes'
      ),
      "natura_network_2000" = requested_poly(
        lidardb, 'lidar_xn2000'
      ),
      'drawn_poly' = drawed_poly(
        lidardb, map_reactives$lidar_map_draw_all_features
      ),
      'file' = file_poly(lidardb, user_file)
    )

    progress$set(value = 85)

    return(res)
  })

  data_visible <- shiny::reactive({
    shiny::validate(
      shiny::need(data_reactives$lidar_var_sel, 'no var yet')
    )

    progress <- shiny::Progress$new(session, min = 85, max = 100)
    on.exit(progress$close())
    progress$set(
      message = translate_app(
        'poly_visible_progress_mes', lang(), app_translations
      )
    )

    lidar_var <- data_reactives$lidar_var_sel
    res <- data_polys() %>%
      dplyr::select(
        poly_id, poly_km2,
        dplyr::matches(glue::glue("^{lidar_var}_"))
      )

    progress$set(value = 99)
    return(res)
  })

  data_raster <- shiny::reactive({
    shiny::validate(
      shiny::need(data_reactives$lidar_var_sel, 'no var yet')
    )

    progress <- shiny::Progress$new(session, min = 5, max = 100)
    on.exit(progress$close())
    progress$set(
      message = translate_app(
        'raster_progress_mes', lang(), app_translations
      )
    )

    progress$set(value = 15)
    lidar_var <- data_reactives$lidar_var_sel
    lidar_raster <- lidardb$get_lowres_raster(lidar_var, 'raster')
    progress$set(value = 99)
    return(lidar_raster)
  })

  ## reactives to return ####
  main_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    main_data_reactives$data_polys <- data_polys()
    main_data_reactives$data_visible <- data_visible()
    main_data_reactives$data_raster <- data_raster()
  })
  return(main_data_reactives)
}
