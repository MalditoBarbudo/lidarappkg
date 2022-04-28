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
    leaflet::leafletOutput(ns("lidar_map"), height = 600),
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
  hostess_map <- waiter::Hostess$new(infinite = TRUE)
  # 2. waiter overlay related to map id
  waiter_map <- waiter::Waiter$new(
    id = 'mod_mapOutput-lidar_map',
    color = '#E8EAEB'
  )

  ## renderUI ####
  output$map_container <- shiny::renderUI({

    # ns
    ns <- session$ns
    # shiny::tagList(
    #   leaflet::leafletOutput(ns('nfi_map'), height = 600),
    shiny::tags$div(
      id = 'cite',
      translate_app('main_panel_raster_siz_1', lang(), app_translations),
      translate_app('main_panel_raster_siz_2', lang(), app_translations)
    )
    #   )
  }) # end of renderUI

  output$lidar_map <- leaflet::renderLeaflet({

    shiny::req(main_data_reactives$data_raster, cancelOutput = TRUE)
    shiny::req(main_data_reactives$data_visible, cancelOutput = TRUE)

    data_raster <- main_data_reactives$data_raster
    data_map <- main_data_reactives$data_visible

    palette <- leaflet::colorNumeric(
      viridis::plasma(100),
      raster::values(data_raster),
      na.color = 'transparent'
    )

    palette_legend <- leaflet::colorNumeric(
      viridis::plasma(100),
      raster::values(data_raster),
      na.color = 'transparent',
      reverse = TRUE
    )

    # poly intermediates
    var_column <- glue::glue("{data_reactives$lidar_var_sel}_average")

    # proper map
    leaflet::leaflet() %>%
      leaflet::setView(1.744, 41.726, zoom = 8) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief,
        group = 'Relief' %>% translate_app(lang(), app_translations)
      ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = 'Imaginery' %>% translate_app(lang(), app_translations)
      ) %>%
      leaflet::addMapPane('polys', zIndex = 410) %>%
      leaflet::addMapPane('rasters', zIndex = 420) %>%
      leaflet::addLayersControl(
        baseGroups = c('Relief', 'Imaginery') %>%
          translate_app(lang(), app_translations),
        overlayGroups = c('lidar', 'poly') %>%
          translate_app(lang(), app_translations) %>%
          purrr::map_chr(~ glue::glue(.x)),
        options = leaflet::layersControlOptions(
          collapsed = FALSE, autoZIndex = FALSE
        )
      ) %>%

      # This hide the raster until the user click the checkbox. Now, both
      # layers (polygons and raster) are shown by default, so this is not
      # needed. Ledt it here just in case the default changes in the future.

      # leaflet::hideGroup(
      #   'lidar' %>% translate_app(lang(), app_translations)
      # ) %>%

      leaflet::removeImage('raster') %>%
      leaflet::clearGroup(
        'poly' %>%
          translate_app(lang(), app_translations) %>%
          purrr::map_chr(~ glue::glue(.x))
      ) %>%
      leaflet::addRasterImage(
        data_raster, project = TRUE, colors = palette, opacity = 1,
        group = 'lidar' %>% translate_app(lang(), app_translations),
        layerId = 'raster'
      ) %>%
      leaflet::addPolygons(
        data = data_map,
        group = 'poly' %>%
          translate_app(lang(), app_translations) %>%
          purrr::map_chr(~ glue::glue(.x)),
        label = ~poly_id,
        layerId = ~poly_id,
        weight = 1, smoothFactor = 1,
        opacity = 1.0, fill = TRUE,
        color = '#6C7A89FF',
        fillColor = palette(data_map[[var_column]]),
        fillOpacity = 0.7,
        highlightOptions = leaflet::highlightOptions(
          color = "#CF000F", weight = 2,
          bringToFront = FALSE
        ),
        options = leaflet::pathOptions(
          pane = 'polys'
        )
      ) %>%
      leaflet::addLegend(
        pal = palette_legend, values = raster::values(data_raster),
        title = var_column %>%
          stringr::str_remove('_average') %>%
          translate_app(lang(), app_translations),
        position = 'bottomright', opacity = 1,
        labFormat = leaflet::labelFormat(
          transform = function(x) {sort(x, decreasing = TRUE)}
        )
      ) %>%
      # leaflet.extras plugins
      leaflet.extras::addDrawToolbar(
        targetGroup = 'poly' %>%
          translate_app(lang(), app_translations) %>%
          purrr::map_chr(~ glue::glue(.x)),
        position = 'topleft',
        polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
        markerOptions = FALSE, circleMarkerOptions = FALSE,
        polygonOptions = leaflet.extras::drawPolygonOptions(
          shapeOptions = leaflet.extras::drawShapeOptions()
        ),
        editOptions = leaflet.extras::editToolbarOptions(
          edit = TRUE, remove = TRUE
        ),
        singleFeature = TRUE
      )
  }) # end of leaflet output

  # reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$lidar_map_draw_all_features <-
      input$lidar_map_draw_all_features
    map_reactives$lidar_map_shape_click <- input$lidar_map_shape_click
    map_reactives$lidar_map_click <- input$lidar_map_click
  })
  return(map_reactives)
}
