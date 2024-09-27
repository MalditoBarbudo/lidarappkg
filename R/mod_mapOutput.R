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
  # shiny::tagList(
  #   leaflet::leafletOutput(ns("lidar_map"), height = 600),
  #   shiny::uiOutput(ns('map_container'))
  # )
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
      # style = mapdeck::mapdeck_style('dark'),
      # style = "mapbox://styles/mapbox/dark-v10",
      style = "https://raw.githubusercontent.com/CartoDB/basemap-styles/refs/heads/master/mapboxgl/dark-matter-nolabels.json",
      location = c(1.744, 41.726), zoom = 7, pitch = 0
    )
  })

  shiny::observe(
    x = {
      shiny::req(main_data_reactives$data_raster, cancelOutput = TRUE)
      shiny::req(main_data_reactives$data_visible, cancelOutput = TRUE)

      # data
      lidar_var_sel <- data_reactives$lidar_var_sel
      var_column <- glue::glue("{lidar_var_sel}_average")
      show_polys <- data_reactives$show_polys
      data_raster <- main_data_reactives$data_raster
      palette_fun <- scales::col_numeric(
        hcl.colors(10, "rocket", alpha = 0.8),
        c(data_raster[["min_value"]], data_raster[["max_value"]]),
        na.color = "#FFFFFF00", reverse = FALSE, alpha = TRUE
      )
      data_map <- main_data_reactives$data_visible |>
        dplyr::mutate(
          hex = palette_fun(.data[[var_column]]),
          tooltip = paste0(
            "<p>", poly_id, ": ", round(.data[[var_column]], 2), "</p>"
          )
        )

      # custom legend (to be able to show in natural order, high values up)
      legend_js <- mapdeck::legend_element(
        variables = rev(round(seq(
          data_raster[["min_value"]],
          data_raster[["max_value"]],
          length.out = 5
        ), 0)),
        colours = scales::col_numeric(
          hcl.colors(10, "rocket", alpha = 0.8),
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
        stroke_colour_def <- "hex"
        elevation_def <- NULL
        elevation_scale_def <- 1

        # if (poly_3d) {
        #   stroke_colour_def <- NULL
        #   elevation_def <- var_column
        #   elevation_scale_def <- paste0(
        #     "1e",
        #     3 - (max(data_map[[var_column]], na.rm = TRUE) |> log10() |> round())
        #   ) |>
        #     as.numeric()
        # }
        # map update
        mapdeck::mapdeck_update(map_id = session$ns("lidar_map")) |>
          mapdeck::clear_bitmap(layer_id = "lidar_raster") |>
          mapdeck::clear_polygon(layer_id = "lidar_polys") |>
          mapdeck::add_polygon(
            data = data_map, layer_id = "lidar_polys",
            tooltip = "tooltip",
            id = "poly_id",
            stroke_colour = stroke_colour_def,
            fill_colour = "hex",
            fill_opacity = 0.8,
            auto_highlight = TRUE, highlight_colour = "#FDF5EB80",
            elevation = elevation_def, elevation_scale = elevation_scale_def,
            update_view = FALSE, focus_layer = FALSE,
            legend = legend_js
          )

      } else {
        # map update
        mapdeck::mapdeck_update(map_id = session$ns("lidar_map")) |>
          mapdeck::clear_bitmap(layer_id = "lidar_raster") |>
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
          mapdeck::add_polygon(
            data = main_data_reactives$data_visible, layer_id = "lidar_polys",
            fill_opacity = 0,
            legend = legend_js,
            update_view = FALSE, focus_layer = FALSE
          )
      }
    }
  )

  # output$lidar_map <- leaflet::renderLeaflet({

  #   shiny::req(main_data_reactives$data_raster, cancelOutput = TRUE)
  #   shiny::req(main_data_reactives$data_visible, cancelOutput = TRUE)

  #   data_raster <- main_data_reactives$data_raster
  #   data_map <- main_data_reactives$data_visible

  #   palette <- leaflet::colorNumeric(
  #     viridis::rocket(100),
  #     data_raster[[1]] |> as.numeric(),
  #     na.color = 'transparent'
  #   )

  #   palette_legend <- leaflet::colorNumeric(
  #     viridis::rocket(100),
  #     data_raster[[1]] |> as.numeric(),
  #     na.color = 'transparent',
  #     reverse = TRUE
  #   )

  #   # poly intermediates
  #   var_column <- glue::glue("{data_reactives$lidar_var_sel}_average")

  #   # proper map
  #   leaflet::leaflet() |>
  #     leaflet::setView(1.744, 41.726, zoom = 8) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Esri.WorldShadedRelief,
  #       group = 'Relief' |> translate_app(lang(), app_translations),
  #       # to avoid the raster disappear when changing base tiles
  #       options = leaflet::providerTileOptions(zIndex = -10)
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Esri.WorldImagery,
  #       group = 'Imaginery' |> translate_app(lang(), app_translations),
  #       # to avoid the raster disappear when changing base tiles
  #       options = leaflet::providerTileOptions(zIndex = -10)
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$OpenStreetMap,
  #       group = translate_app('OSM', lang(), app_translations),
  #       # to avoid the raster disappear when changing base tiles
  #       options = leaflet::providerTileOptions(zIndex = -10)
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Esri.WorldGrayCanvas,
  #       group = translate_app('WorldGrayCanvas', lang(), app_translations),
  #       # to avoid the raster disappear when changing base tiles
  #       options = leaflet::providerTileOptions(zIndex = -10)
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$CartoDB.PositronNoLabels,
  #       group = translate_app('PositronNoLabels', lang(), app_translations),
  #       # to avoid the raster disappear when changing base tiles
  #       options = leaflet::providerTileOptions(zIndex = -10)
  #     ) |>
  #     leaflet::addMapPane('polys', zIndex = 410) |>
  #     leaflet::addMapPane('rasters', zIndex = 420) |>
  #     leaflet::addLayersControl(
  #       baseGroups = c('Relief', 'Imaginery', 'OSM', 'WorldGrayCanvas', 'PositronNoLabels') |>
  #         translate_app(lang(), app_translations),
  #       overlayGroups = c('lidar', 'poly') |>
  #         translate_app(lang(), app_translations) |>
  #         purrr::map_chr(~ glue::glue(.x)),
  #       options = leaflet::layersControlOptions(
  #         collapsed = FALSE, autoZIndex = FALSE
  #       )
  #     ) |>

  #     # This hide the raster until the user click the checkbox. Now, both
  #     # layers (polygons and raster) are shown by default, so this is not
  #     # needed. Ledt it here just in case the default changes in the future.

  #     # leaflet::hideGroup(
  #     #   'lidar' |> translate_app(lang(), app_translations)
  #     # ) |>

  #     leaflet::removeImage('raster') |>
  #     leaflet::clearGroup(
  #       'poly' |>
  #         translate_app(lang(), app_translations) |>
  #         purrr::map_chr(~ glue::glue(.x))
  #     ) |>
  #     leaflet::addRasterImage(
  #       terra::rast(data_raster), project = TRUE, colors = palette, opacity = 1,
  #     group = 'lidar' |>
  #       translate_app(lang(), app_translations) |>
  #       purrr::map_chr(~ glue::glue(.x)),
  #       layerId = 'raster'
  #     ) |>
  #     leaflet::addPolygons(
  #       data = data_map,
  #       group = 'poly' |>
  #         translate_app(lang(), app_translations) |>
  #         purrr::map_chr(~ glue::glue(.x)),
  #       label = ~poly_id,
  #       layerId = ~poly_id,
  #       weight = 1, smoothFactor = 1,
  #       opacity = 1.0, fill = TRUE,
  #       color = '#6C7A89FF',
  #       fillColor = palette(data_map[[var_column]]),
  #       fillOpacity = 0.85,
  #       highlightOptions = leaflet::highlightOptions(
  #         color = "#CF000F", weight = 2,
  #         bringToFront = FALSE
  #       ),
  #       options = leaflet::pathOptions(
  #         pane = 'polys'
  #       )
  #     ) |>
  #     # hide polys
  #     leaflet::hideGroup(
  #       'poly' |>
  #         translate_app(lang(), app_translations) |>
  #         purrr::map_chr(~ glue::glue(.x))
  #     ) |>
  #     leaflet::addLegend(
  #       pal = palette_legend, values = data_raster[[1]] |> as.numeric(),
  #       title = var_column |>
  #         stringr::str_remove('_average') |>
  #         translate_app(lang(), app_translations),
  #       position = 'bottomright', opacity = 1,
  #       labFormat = leaflet::labelFormat(
  #         transform = function(x) {sort(x, decreasing = TRUE)}
  #       )
  #     ) |>
  #     # leaflet.extras plugins
  #     leaflet.extras::addDrawToolbar(
  #       targetGroup = 'poly' |>
  #         translate_app(lang(), app_translations) |>
  #         purrr::map_chr(~ glue::glue(.x)),
  #       position = 'topleft',
  #       polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
  #       markerOptions = FALSE, circleMarkerOptions = FALSE,
  #       polygonOptions = leaflet.extras::drawPolygonOptions(
  #         shapeOptions = leaflet.extras::drawShapeOptions()
  #       ),
  #       editOptions = leaflet.extras::editToolbarOptions(
  #         edit = TRUE, remove = TRUE
  #       ),
  #       singleFeature = TRUE
  #     )
  # }) # end of leaflet output

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
