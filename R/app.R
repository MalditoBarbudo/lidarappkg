#' function to launch the lidar app
#'
#' @importFrom magrittr %>%
#'
#' @export
lidar_app <- function(
  user = 'guest', password = 'guest',
  host = NULL, port = NULL, dbname = 'lidargis'
) {

  ### DB access ################################################################
  lidar_db <- pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    user = user,
    password = password,
    dbname = dbname,
    host = host,
    port = port
  )

  ### Variables names inter ####################################################


  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'allometrApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(

    # shinyjs
    shinyjs::useShinyjs(),
    # shinyWidgets::chooseSliderSkin(skin = "Shiny", color = '#0DB3D4'),
    # shinyWidgets::useSweetAlert(),

    # css
    shiny::tags$head(
      # custom css
      shiny::includeCSS(
        system.file('resources', 'lidarapp.css', package = 'lidarappkg')
      )
    ),

    navbarPageWithInputs(
      # opts
      title = 'LiDAR App',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # navbarPage contents
      shiny::tabPanel(
        title = 'Explore',
        ########################################################### debug ####
        # shiny::absolutePanel(
        #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
        #   draggable = TRUE, width = 640, height = 'auto',
        #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
        #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
        #   top = 60, left = 'auto', right = 50, bottom = 'auto',
        #
        #   shiny::textOutput('debug1'),
        #   shiny::textOutput('debug2'),
        #   shiny::textOutput('debug3')
        # ),
        ####################################################### end debug ####

        # we need an UI beacuse we need to translate based on the lang input from the
        # navbar
        shiny::uiOutput('explore_ui')

      ) # end of tabPanel "Explore"
    ) # end of navbarPage
  )

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   data_reactives$diameter_classes
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_click
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_shape_click
    # })

    ## lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    ## explore UI (to use lang) ####
    output$explore_ui <- shiny::renderUI({

      # lang
      lang_declared <- lang()

      # proper UI ####
      shiny::fluidPage(
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # title
            shiny::h4('Controls'),

            # var input
            shiny::selectInput(
              'lidar_var_sel', 'Select the variable to visualize',
              choices = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'),
              selected = 'AB'
            ),

            # poly input
            shiny::selectInput(
              'poly_type_sel', 'Select the thing',
              choices = c(
                'Catalonia', 'Provinces', 'Counties', 'Municipalities', 'Veguerias',
                'Drawed polygon', 'File upload'
              ),
              selected = 'Provinces'
            ),

            # res output
            shiny::h4('Results'),
            shiny::tableOutput('poly_res_table'),

            # download buttons
            shiny::h4('Download'),
            shinyWidgets::actionBttn(
              'download_trigger_btn', 'Download', icon = shiny::icon('download'),
              color = 'success', size = 'sm'
            )
          ),
          mainPanel = shiny::mainPanel(
            width = 8,
            # map module
            # shiny::div(
            #   class = 'mapouter',
              leaflet::leafletOutput('raster_map', height = 600)
            # )
          )
        ) # end of sidebar layout
      ) # end of fluidPage
    }) # end of exploreUI

    # proper server ####

    # data res reactive
    data_res <- shiny::reactive({

      data_res <- switch(input$poly_type_sel,
        'Catalonia' = catalonia_poly(lidar_db),
        'Provinces' = provinces_poly(lidar_db),
        'Counties' = counties_poly(lidar_db),
        'Municipalities' = municipalities_poly(lidar_db),
        'Veguerias' = veguerias_poly(lidar_db),
        'Drawed polygon' = drawed_poly(lidar_db),
        'File upload' = file_poly(lidar_db)
      )
      return(data_res)
    })


    output$poly_res_table <- shiny::renderTable({

      lidar_var <- tolower(input$lidar_var_sel)
      var_column <- glue::glue("mean_{lidar_var}")

      data_res() %>%
        dplyr::as_tibble() %>%
        dplyr::select(poly_id, !! rlang::sym(var_column))
    })

    output$raster_map <- leaflet::renderLeaflet({

      # shiny::validate(
      #   shiny::need(data_res(), 'No data'),
      #   shiny::need(input$poly_type_sel, 'No polygon type selected'),
      #   shiny::need(input$lidar_val_sel, 'No lidar variable selected')
      # )

      lidar_band <- switch(
        input$lidar_var_sel,
        'AB' = 1,
        'BAT' = 6,
        'BF' = 4,
        'CAT' = 7,
        'DBH' = 2,
        'HM' = 3,
        'REC' = 5,
        'VAE' = 8
      )

      temp_postgresql_conn <- pool::poolCheckout(lidar_db)
      lidar_raster <- rpostgis::pgGetRast(temp_postgresql_conn, 'lidar_stack', bands = lidar_band)
      pool::poolReturn(temp_postgresql_conn)
      # rm(temp_postgresql_conn)

      palette <- leaflet::colorNumeric(
        viridis::magma(100),
        # raster::values(basal_area_raster),
        raster::values(lidar_raster),
        na.color = 'transparent'
      )

      poly_type <- input$poly_type_sel
      var_column <- glue::glue('mean_{tolower(input$lidar_var_sel)}')
      user_poly <- data_res() %>%
        sf::st_transform('+proj=longlat +datum=WGS84') %>%
        dplyr::select(poly_id, !! rlang::sym(var_column))

      leaflet::leaflet() %>%
        leaflet::setView(1, 41.70, zoom = 8) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief, group = 'Relief') %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Imaginery') %>%
        leaflet::addLayersControl(
          baseGroups = c('Relief', 'Imaginery'),
          overlayGroups = c('lidar', 'poly'),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) %>%
        leaflet::clearGroup('raster') %>%
        leaflet::clearGroup('poly') %>%
        leaflet::addRasterImage(
          lidar_raster, project = FALSE, colors = palette, opacity = 1, group = 'lidar'
        ) %>%
        leaflet::addPolygons(
          data = user_poly, group = 'poly',
          label = ~poly_id,
          weight = 1, smoothFactor = 1,
          opacity = 1.0, fill = TRUE,
          color = '#6C7A89FF', fillColor = palette(user_poly[[var_column]]),
          fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            color = "#CF000F", weight = 2,
            bringToFront = FALSE
          )
        ) %>%
        leaflet::addLegend(
          pal = palette, values = raster::values(lidar_raster),
          title = input$lidar_var_sel, position = 'bottomright'
        )
    })

    # output$raster_map <- leaflet::renderLeaflet({
    #
    #   # browser()
    #
    #   lidar_band <- switch(
    #     input$lidar_var_sel,
    #     'AB' = 1,
    #     'BAT' = 6,
    #     'BF' = 4,
    #     'CAT' = 7,
    #     'DBH' = 2,
    #     'HM' = 3,
    #     'REC' = 5,
    #     'VAE' = 8
    #   )
    #
    #   temp_postgresql_conn <- pool::poolCheckout(lidar_db)
    #   lidar_raster <- rpostgis::pgGetRast(temp_postgresql_conn, 'lidar_stack', bands = lidar_band)
    #   pool::poolReturn(temp_postgresql_conn)
    #   # rm(temp_postgresql_conn)
    #
    #   palette <- leaflet::colorNumeric(
    #     viridis::magma(100),
    #     # raster::values(basal_area_raster),
    #     raster::values(lidar_raster),
    #     na.color = 'transparent'
    #   )
    #
    #   leaflet::leaflet() %>%
    #     leaflet::setView(1, 41.70, zoom = 8) %>%
    #     leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief, group = 'Relief') %>%
    #     leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Imaginery') %>%
    #     leaflet::addRasterImage(lidar_raster, project = FALSE, colors = palette, opacity = 1, group = 'lidar') %>%
    #     leaflet::addLayersControl(
    #       baseGroups = c('Relief', 'Imaginery'),
    #       overlayGroups = c('lidar', 'poly'),
    #       options = leaflet::layersControlOptions(collapsed = TRUE)
    #     ) %>%
    #     leaflet::addLegend(
    #       pal = palette, values = raster::values(lidar_raster), title = 'Raster legend'
    #     )
    # })
    #
    #
    # observer to update the polygons
    # shiny::observe({
    #
    #
    #   shiny::validate(
    #     shiny::need(input$poly_type_sel, 'No polygon type selected')
    #   )
    #
    #   # browser()
    #   poly_type <- input$poly_type_sel
    #   var_column <- glue::glue('mean_{tolower(input$lidar_var_sel)}')
    #   user_poly <- data_res() %>%
    #     sf::st_transform('+proj=longlat +datum=WGS84') %>%
    #     dplyr::select(poly_id, !! rlang::sym(var_column))
    #   palette_polys <- leaflet::colorNumeric(
    #     viridis::magma(100),
    #     raster::values(lidar_raster), na.color = 'transparent'
    #   )
    #
    #   leaflet::leafletProxy('raster_map') %>%
    #     leaflet::clearGroup(group = 'poly') %>%
    #     leaflet::clearGroup(group = 'raster') %>%
    #     leaflet::addPolygons(
    #       data = user_poly, group = 'poly',
    #       label = ~poly_id,
    #       weight = 1, smoothFactor = 1,
    #       opacity = 1.0, fill = TRUE,
    #       color = '#6C7A89FF', fillColor = palette_polys(user_poly[[var_column]]),
    #       fillOpacity = 0.3,
    #       highlightOptions = leaflet::highlightOptions(
    #         color = "#CF000F", weight = 2,
    #         bringToFront = FALSE
    #       )
    #     ) %>%
    #     leaflet::addLegend(
    #       position = 'bottomright', pal = palette_polys, values = user_poly[[var_column]],
    #       title = var_column, group = 'poly'
    #     )
    # })


  } # end of server function

  # Run the application
  lidarapp <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(lidar_db)
      })
    }
  )

  # shiny::runApp(nfi_app)
  return(lidarapp)

}
