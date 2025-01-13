#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
mod_mapOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    mapdeck::mapdeckOutput(ns("lidar_map"), height = 600),
    shiny::uiOutput(ns('map_container'))
  )
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#' @param app_translations dictionary
#' @param main_data_reactives,data_reactives reactives needed
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  lang, app_translations,
  main_data_reactives, data_reactives
) {

  ## waiter/hostess progress ####
  # set a progress with waiter. We will use infinite TRUE, that way we dont
  # need to calculate any steps durations
  # 1. hostess progress
  # hostess_map <- waiter::Hostess$new(infinite = TRUE)
  # # 2. waiter overlay related to map id
  # waiter_map <- waiter::Waiter$new(
  #   id = 'mod_mapOutput-lidar_map',
  #   color = '#E8EAEB'
  # )
  # attachNamespace("terra")

  ## renderUI ####
  output$map_container <- shiny::renderUI({

    # ns
    ns <- session$ns
    shiny::tags$div(
      id = 'cite',
      translate_app('main_panel_raster_siz_1', lang(), app_translations),
      translate_app('main_panel_raster_siz_2', lang(), app_translations)
    )
    #   )
  }) # end of renderUI

  # mapdeck output and observers
  output$lidar_map <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(
      ## debug
      # show_view_state = TRUE,
      # style = mapdeck::mapdeck_style('dark'),
      # style = "mapbox://styles/mapbox/dark-v10",
      style = "https://raw.githubusercontent.com/CartoDB/basemap-styles/refs/heads/master/mapboxgl/dark-matter-nolabels.json",
      location = c(1.744, 41.726), zoom = 7, pitch = 0, max_pitch = 60
    )
  })

  shiny::observe(
    x = {
      shiny::req(main_data_reactives$data_raster, cancelOutput = TRUE)
      shiny::req(main_data_reactives$data_visible, cancelOutput = TRUE)

      # data
      lidar_var_sel <- data_reactives$lidar_var_sel
      var_column <- glue::glue("{lidar_var_sel}_average")
      poly_3d <- data_reactives$poly_3d
      show_polys <- data_reactives$show_polys
      data_raster <- main_data_reactives$data_raster
      palette_fun <- scales::col_numeric(
        hcl.colors(10, "ag_GrnYl", alpha = ifelse(poly_3d, 1, 0.8)),
        c(data_raster[["min_value"]], data_raster[["max_value"]]),
        na.color = "#FFFFFF00", reverse = FALSE, alpha = TRUE
      )
      data_map <- main_data_reactives$data_visible |>
        dplyr::mutate(
          hex = palette_fun(.data[[var_column]]),
          tooltip = paste0(
            "<p>", poly_id, ": ", round(.data[[var_column]], 2), "</p>"
          ),
          fake_elevation = 20000 * .data[[var_column]] / max(.data[[var_column]], na.rm = TRUE)
        )

      # custom legend (to be able to show in natural order, high values up)
      legend_js <- mapdeck::legend_element(
        variables = rev(round(seq(
          data_raster[["min_value"]],
          data_raster[["max_value"]],
          length.out = 5
        ), 0)),
        colours = scales::col_numeric(
          hcl.colors(10, "ag_GrnYl", alpha = ifelse(poly_3d, 1, 0.8)),
          c(data_raster[["min_value"]], data_raster[["max_value"]]),
          na.color = "#FFFFFF00", reverse = TRUE, alpha = TRUE
        )(seq(
          data_raster[["min_value"]],
          data_raster[["max_value"]],
          length.out = 5
        )),
        colour_type = "fill", variable_type = "gradient",
        title = translate_app(lidar_var_sel, lang(), app_translations)
      ) |>
        mapdeck::mapdeck_legend()

      # show the polys
      if (show_polys) {

        # browser()
        stroke_colour_def <- "hex"
        elevation_def <- NULL
        elevation_scale_def <- 1
        opacity_custom <- ifelse(poly_3d, 1, 0.8)

        if (poly_3d) {
          stroke_colour_def <- NULL
          elevation_def <- "fake_elevation"
        }
        # map update
        mapdeck::mapdeck_update(map_id = session$ns("lidar_map")) |>
          mapdeck::clear_bitmap(layer_id = "lidar_raster") |>
          mapdeck::clear_legend(layer_id = "custom_legend") |>
          # mapdeck::clear_polygon(layer_id = "lidar_polys") |>
          mapdeck::add_polygon(
            data = data_map, layer_id = "lidar_polys",
            tooltip = "tooltip",
            id = "poly_id",
            stroke_colour = stroke_colour_def,
            fill_colour = "hex",
            fill_opacity = opacity_custom,
            auto_highlight = TRUE, highlight_colour = "#FDF5EB80",
            elevation = elevation_def, elevation_scale = elevation_scale_def,
            update_view = FALSE, focus_layer = FALSE,
            legend = legend_js
          )

      } else {
        # map update
        mapdeck::mapdeck_update(map_id = session$ns("lidar_map")) |>
          # mapdeck::clear_bitmap(layer_id = "lidar_raster") |>
          # mapdeck::clear_legend(layer_id = "custom_legend") |>
          mapdeck::clear_polygon(layer_id = "lidar_polys") |>
          mapdeck::add_bitmap(
            image = data_raster$base64_string, layer_id = "lidar_raster",
            bounds = c(
              data_raster$left_ext, data_raster$down_ext,
              data_raster$right_ext, data_raster$up_ext
            ),
            update_view = FALSE, focus_layer = FALSE,
            transparent_colour = "#00000000"
          ) |>
          # mapdeck::add_polygon(
          #   data = main_data_reactives$data_visible, layer_id = "lidar_polys",
          #   fill_opacity = 0,
          #   # id = "poly_id",
          #   legend = legend_js,
          #   update_view = FALSE, focus_layer = FALSE
          # )
          mapdeck::add_legend(legend = legend_js, layer_id = "custom_legend")
      }
    }
  )

  # reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$lidar_map_draw_all_features <-
      input$lidar_map_draw_all_features
    map_reactives$lidar_map_shape_click <- input$lidar_map_polygon_click
    map_reactives$lidar_map_click <- input$lidar_map_bitmap_click
  })
  return(map_reactives)
}