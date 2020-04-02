#' @title mod_infoUI and mod_info
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_infoUI <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      shiny::h4(shiny::textOutput(ns('plot_title'))),
      shiny::plotOutput(ns("info_plot"), height = '500px')
    )
  )
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#' @param app_translations dictionary
#' @param map_reactives,main_data_reactives,data_reactives reactives needed
#'
#' @export
mod_info <- function(
  input, output, session,
  lang, app_translations,
  map_reactives, main_data_reactives, data_reactives
) {

  plot_generation <- shiny::reactive({

    click <- map_reactives$lidar_map_shape_click
    id_click <- click$id
    data_plot <- main_data_reactives$data_polys %>%
      dplyr::as_tibble() %>%
      dplyr::select(poly_id, dplyr::contains('_average'), -geometry)

    # plot list
    plot_list <- c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE') %>%
      magrittr::set_names(.,.) %>%
      purrr::map(
        function(x) {
          var_name <- glue::glue("{x}_average")
          data_plot %>%
            ggplot2::ggplot(ggplot2::aes(x = 0, y = !!rlang::sym(var_name))) +
            ggplot2::geom_point(
              data = ~ dplyr::filter(.x, poly_id != id_click),
              colour = '#647a8d', size = 4, alpha = 0.5,
              position = ggplot2::position_jitter(
                width = .2, height = 0, seed = 25
              )
            ) +
            ggplot2::geom_violin(fill = 'transparent') +
            ggplot2::geom_point(
              data = ~ dplyr::filter(.x, poly_id == id_click),
              colour = '#448714', size = 6
            ) +
            ggplot2::scale_x_continuous(breaks = NULL) +
            ggplot2::labs(
              x = '',
              y = translate_app(var_name, lang(), app_translations)
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              text = ggplot2::element_text(size = 14, color = '#647a8d'),
              axis.text = ggplot2::element_text(color = '#647a8d'),
              strip.text = ggplot2::element_text(color = '#647a8d'),
              panel.background = ggplot2::element_rect(
                fill = '#c8cac8', colour = NA
              ),
              plot.background = ggplot2::element_rect(
                fill = '#c8cac8', colour = NA
              ),
              strip.background = ggplot2::element_rect(
                fill = '#c8cac8', colour = NA
              ),
              panel.grid = ggplot2::element_line(colour = '#647a8d'),
              panel.grid.minor.x = ggplot2::element_blank(),
              panel.grid.major.x = ggplot2::element_blank(),
              panel.grid.minor.y = ggplot2::element_blank(),
              panel.grid.major.y = ggplot2::element_line(
                size = ggplot2::rel(0.5), colour = '#647a8d'
              )
            )
        }
      )

    res <- cowplot::plot_grid(
      plotlist = plot_list,
      nrow = 2, ncol = 4
    )
    return(res)
  })

  output$info_plot <- shiny::renderPlot({
    plot_generation()
  })

  output$plot_title <- shiny::renderText({
    click_value <- map_reactives$lidar_map_shape_click$id
    glue::glue(
      translate_app(
        glue::glue("{data_reactives$poly_type_sel}_info_plot_title"),
        lang(), app_translations
      )
    )
  })
}
