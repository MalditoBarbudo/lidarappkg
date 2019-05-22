# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' catalonia_poly
#'
#' return the data pre calculated for catalonia
#'
catalonia_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_catalunya')
}

#' provinces_poly
#'
#' return the data pre calculated for provinces
#'
provinces_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_provincias')
}

#' counties_poly
#'
#' return the data pre calculated for counties
#'
counties_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_comarcas')
}

#' municipalities_poly
#'
#' return the data pre calculated for municipalities
#'
municipalities_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_municipios')
}

#' veguerias_poly
#'
#' return the data pre calculated for veguerias
#'
veguerias_poly <- function(lidar_db) {
  sf::st_read(lidar_db, 'lidar_veguerias')
}


#' file_poly
#'
#' return the data calculated on-the-fly for the file loaded
#'
file_poly <- function(lidar_db, file, poly_id) {

  # check if there is file
  if (is.null(file)) {return()}

  sf::st_read(file) %>%
    lidar_clip(lidar_db = lidar_db, poly_id = poly_id)
}
