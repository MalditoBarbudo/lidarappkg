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

#' @title mod_mainData server function
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

  ## waiter/hostess progress ####
  # set a progress with waiter. We will use infinite TRUE, that way we dont
  # need to calculate any steps durations
  # 1. hostess progress
  hostess_raster <- waiter::Hostess$new(infinite = TRUE)
  hostess_raster$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))
  hostess_polys <- waiter::Hostess$new(infinite = TRUE)
  hostess_polys$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))

  # reactives ####
  data_polys <- shiny::reactive({

    # progress
    waiter_polys <- waiter::Waiter$new(
      id = 'mod_mapOutput-lidar_map',
      html = shiny::tagList(
        hostess_polys$get_loader(),
        shiny::h3(translate_app('poly_data_progress_mes', lang(), app_translations))
      ),
      color = '#E8EAEB'
    )

    waiter_polys$show()
    hostess_polys$start()
    on.exit(hostess_polys$close(), add = TRUE)
    on.exit(waiter_polys$hide(), add = TRUE)

    shiny::validate(
      shiny::need(data_reactives$poly_type_sel, 'no poly yet')
    )

    poly_type <- data_reactives$poly_type_sel
    lidar_var <- shiny::isolate(data_reactives$lidar_var_sel)
    user_file <- data_reactives$user_file_sel
    drawn_poly <- map_reactives$lidar_map_draw_all_features

    if (poly_type == 'file') {
      shiny::validate(
        shiny::need(user_file, 'no file yet')
      )
    }

    if (poly_type == 'drawn_poly') {

      if (is.null(drawn_poly) || length(drawn_poly[['features']]) < 1) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = translate_app('sweet_alert_nopoly_title', lang(), app_translations),
          text = translate_app('sweet_alert_nopoly_text', lang(), app_translations)
        )
      }

      shiny::validate(
        shiny::need(drawn_poly, 'no draw polys yet'),
        shiny::need(length(drawn_poly[['features']]) != 0, 'removed poly')
      )
    }

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
        lidardb, drawn_poly
      ),
      'file' = file_poly(lidardb, user_file, lang())
    )

    return(res)
  })

  data_visible <- shiny::reactive({
    shiny::validate(
      shiny::need(data_reactives$lidar_var_sel, 'no var yet')
    )

    lidar_var <- data_reactives$lidar_var_sel

    res <- data_polys() |>
      dplyr::select(
        poly_id, poly_km2,
        dplyr::matches(glue::glue("^{lidar_var}_"))
      )

    return(res)
  })

  data_raster <- shiny::reactive({
    shiny::validate(
      shiny::need(data_reactives$lidar_var_sel, 'no var yet')
    )

    # Sys.sleep(0.2) # we need this for the init progress
    waiter_raster <- waiter::Waiter$new(
      id = 'mod_mapOutput-lidar_map',
      html = shiny::tagList(
        hostess_raster$get_loader(),
        shiny::h3(translate_app('raster_progress_mes', lang(), app_translations))
      ),
      color = '#E8EAEB'
    )

    waiter_raster$show()
    hostess_raster$start()
    on.exit(hostess_raster$close())
    on.exit(waiter_raster$hide(), add = TRUE)

    lidar_var <- data_reactives$lidar_var_sel
    lidar_raster <- lidardb$get_lowres_raster(lidar_var, 'raster')

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
