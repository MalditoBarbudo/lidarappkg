#' @title mod_tableOutput and mod_table
#'
#' @description Shiny module to generate the table
#'
#' @param id shiny id
#'
#' @export
mod_tableOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("lidar_table"), height = 600)
  )
}

#' mod_table server function
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
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  lang, app_translations,
  main_data_reactives
) {

  ## output table ####
  output$lidar_table <- DT::renderDT({

    shiny::validate(
      shiny::need(main_data_reactives$data_visible, 'no data yet')
    )

    main_data_reactives$data_visible %>%
      dplyr::as_tibble() %>%
      # dplyr::select(!dplyr::contains('_pixels'), -geometry) %>%
      dplyr::select(
        !dplyr::contains('_pixels'), !dplyr::one_of(c('geom', 'geometry'))
      ) %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = translate_app(names(.), lang(), app_translations),
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        selection = 'single',
        # extensions = 'Buttons',
        options = list(
          pageLength = 15,
          dom = 'tip',
          # buttons = I('colvis'),
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Montserrat'});",
            "}"
          )
        )
      )
  })

  # reactives to return ####
  table_reactives <- shiny::reactiveValues()
  shiny::observe({
    table_reactives$lidar_table_rows_selected <- input$lidar_table_rows_selected
  })
  return(table_reactives)

}
