#' @title mod_tabTranslateOutput and mod_tabTranslate
#'
#' @description A shiny module to translate tab titles
#'
#' @param id shiny id
#'
#' @export
mod_tabTranslateOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::uiOutput(ns("tab_title_translated"))
}

#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#' @param app_translations dictionary
#' @param tab_title title to translate (character)
#'
#' @rdname mod_tabTranslateOutput
#'
#' @export
mod_tabTranslate <- function(
  input, output, session,
  lang, app_translations, tab_title
) {
  output$tab_title_translated <- shiny::renderUI({
    translated_title <- translate_app(tab_title, lang(), app_translations)
    shiny::tagList(translated_title)
  })
}
