#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container')
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#' @param app_translations dictionary
#'
#' @export
mod_data <- function(
  input, output, session,
  lang, app_translantions
) {

  # renderUI ####
  output$mod_data_container <- shiny::renderUI({

    ns <- session$ns

    # precalculated choices
    lidar_var_sel_choices <-  c(
      'AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE'
    ) %>%
      magrittr::set_names(translate_app(., lang(), app_translations))

    poly_type_sel_choices <- c(
      "aut_community", "province", "vegueria", "region",
      "municipality", "natural_interest_area",
      "special_protection_natural_area", "natura_network_2000",
      "file", "drawn_poly"
    ) %>% magrittr::set_names(translate_app(., lang(), app_translations))

    # taglist to return
    shiny::tagList(
      # title
      shiny::h4(translate_app('sidebar_h4_title', lang(), app_translations)),
      # var input
      shiny::selectInput(
        ns('lidar_var_sel'),
        translate_app('lidar_var_sel_label', lang(), app_translations),
        choices = lidar_var_sel_choices,
        selected = lidar_var_sel_choices[1]
      ),

      # poly input
      shiny::selectInput(
        ns('poly_type_sel'),
        translate_app('poly_type_sel_label', lang(), app_translations),
        choices = poly_type_sel_choices,
        selected = poly_type_sel_choices[2]
      ),

      # hidden file selector div
      shinyjs::hidden(
        shiny::div(
          id = ns('file_sel_div'),
          shiny::fluidRow(
            shiny::column(
              7,
              shiny::fileInput(
                ns('user_file_sel'),
                translate_app('user_file_sel_label', lang(), app_translations),
                accept = c('zip', 'gpkg'),
                buttonLabel = translate_app(
                  'user_file_sel_button_label', lang(), app_translations
                ),
                placeholder = translate_app(
                  'user_file_sel_placeholder', lang(), app_translations
                )
              )
            ),
            shiny::column(
              5, align = 'center',
              shiny::p(translate_app('file_text', lang(), app_translations))
            )
          )
        )
      )
    ) # end of tagList
  }) # end of UI

  ## observers ####
  # observer to show the file upload div
  shiny::observe({
    shiny::validate(
      shiny::need(input$poly_type_sel, 'no poly type')
    )
    poly_type <- input$poly_type_sel

    if (poly_type == 'file') {
      shinyjs::show('file_sel_div')
    } else {
      shinyjs::hide('file_sel_div')
    }
  })

  ## reactives to return ####
  data_reactives <- shiny::reactiveValues()
  shiny::observe({
    data_reactives$lidar_var_sel <- input$lidar_var_sel
    data_reactives$poly_type_sel <- input$poly_type_sel
    data_reactives$user_file_sel <- input$user_file_sel
  })
  return(data_reactives)
}
