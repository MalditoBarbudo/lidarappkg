#' @title mod_saveOutput and mod_save
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_saveOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::uiOutput(ns("save_container"))
}

#' mod_save
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#' @param app_translations dictionary
#' @param main_data_reactives reactives needed
#'
#' @export
#'
#' @rdname mod_saveOutput
mod_save <- function(
  input, output, session,
  lang, app_translations,
  main_data_reactives
) {

  ## renderUI ####
  output$save_container <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      # download buttons
      shiny::h4(
        translate_app('sidebar_h4_download', lang(), app_translations)
      ),
      shiny::actionButton(
        ns('download_trigger_btn'),
        translate_app('sidebar_h4_download', lang(), app_translations),
        icon = shiny::icon('download')
      )
    )
  }) # end of renderUI

  ## observers ####
  # download: modal to choose the data format and which data to save
  shiny::observeEvent(
    eventExpr = input$download_trigger_btn,
    handlerExpr = {

      ns <- session$ns
      lang_declared = lang()
      data_format_choices <- list(
        'GIS' = c('shp', 'wkt', 'gpkg') %>%
          magrittr::set_names(
            translate_app(., lang_declared, app_translations)
          ),
        'TABLE' = c('csv', 'xlsx') %>%
          magrittr::set_names(
            translate_app(., lang_declared, app_translations)
          )
      ) %>%
        magrittr::set_names(
          translate_app(names(.), lang_declared, app_translations)
        )
      data_length_choices <- c('visible', 'all_columns') %>%
        magrittr::set_names(translate_app(., lang_declared, app_translations))

      shiny::showModal(
        ui = shiny::modalDialog(
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                12,
                # format options
                shiny::selectInput(
                  ns('data_format'),
                  translate_app(
                    'data_format_label', lang_declared, app_translations
                  ),
                  choices = data_format_choices,
                  selected = 'gpkg'
                ),
                # length options
                shiny::radioButtons(
                  ns('data_length'),
                  translate_app(
                    'data_length_label', lang_declared, app_translations
                  ),
                  choices = data_length_choices,
                  selected = 'visible', width = '100%'
                )
              )
            )
          ),
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton(translate_app(
              'modal_dismiss_label', lang_declared, app_translations
            )),
            shiny::downloadButton(
              ns('download_data_with_options'),
              label = translate_app(
                'sidebar_h4_download', lang_declared, app_translations
              ),
              class = 'btn-success'
            )
          )
        )
      )
    }
  )

  ## save output ####
  # download handlers
  output$download_data_with_options <- shiny::downloadHandler(
    filename = function() {

      file_name <- switch(
        input$data_format,
        'shp' = 'lidar_data.zip',
        'wkt' = 'lidar_data.csv',
        'gpkg' = 'lidar_data.gpkg',
        'csv' = 'lidar_data.csv',
        'xlsx' = 'lidar_data.xlsx'
      )

      return(file_name)
    },
    content = function(file) {
      # data length
      if (input$data_length == 'visible') {
        result_data <- main_data_reactives$data_visible %>%
          sf::st_transform('+proj=longlat +datum=WGS84')
      } else {
        result_data <- main_data_reactives$data_polys %>%
          sf::st_transform('+proj=longlat +datum=WGS84')
      }
      # data format
      # shapefile
      if (input$data_format == 'shp') {
        tmp_dir <- tempdir()
        sf::st_write(
          result_data,
          file.path(tmp_dir, glue::glue("lidar_data_{Sys.Date()}.shp")),
          layer = glue::glue("lidar_data_{Sys.Date()}"),
          delete_layer = TRUE
        )
        shp_files <- list.files(tmp_dir, 'lidar_data_', full.names = TRUE)
        utils::zip(
          file.path(tmp_dir, 'shp_files.zip'),
          shp_files
        )
        file.copy(file.path(tmp_dir, 'shp_files.zip'), file)
        file.remove(file.path(tmp_dir, 'shp_files.zip'), shp_files)
      } else {
        # well known text
        if (input$data_format == 'wkt') {
          sf::write_sf(
            result_data, file, delete_layer = TRUE,
            layer_options = "GEOMETRY=AS_WKT"
          )
        } else {
          # geopackage
          if (input$data_format == 'gpkg') {
            sf::st_write(
              result_data, file, delete_dsn = TRUE
            )
          } else {
            # csv text (no geometry)
            if (input$data_format == 'csv') {
              result_data %>%
                dplyr::as_tibble() %>%
                dplyr::select(-geometry) %>%
                readr::write_csv(file)
            } else {
              # xlsx (no geometry)
              result_data %>%
                dplyr::as_tibble() %>%
                dplyr::select(-geometry) %>%
                writexl::write_xlsx(file)
            }
          }
        }
      }
    }
  )
}
