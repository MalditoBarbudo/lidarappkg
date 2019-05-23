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

            # hidden file selector div
            shinyjs::hidden(
              shiny::div(
                id = 'tururu',
                shiny::fluidRow(
                  shiny::column(
                    12,
                    shiny::fileInput(
                      'user_file_sel', 'Upload a file',
                      accept = c('zip', 'gpkg', 'csv'),
                      buttonLabel = 'Browse...',
                      placeholder = 'No file selected'
                    )#,
                    # shiny::selectInput(
                    #   'poly_id_var', 'Select the polygon id variable',
                    #   names(data_res())
                    # )
                  )
                )
              )
            ),

            # res output
            shiny::h4('Results'),
            shiny::tableOutput('poly_res_table'),

            # download buttons
            shiny::h4('Download'),
            shiny::actionButton(
              'download_trigger_btn', 'Download', icon = shiny::icon('download')
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

    # data res reactive ####
    data_res <- shiny::reactive({

      data_res <- switch(input$poly_type_sel,
        'Catalonia' = catalonia_poly(lidar_db),
        'Provinces' = provinces_poly(lidar_db),
        'Counties' = counties_poly(lidar_db),
        'Municipalities' = municipalities_poly(lidar_db),
        'Veguerias' = veguerias_poly(lidar_db),
        'Drawed polygon' = drawed_poly(lidar_db),
        'File upload' = file_poly(lidar_db, input$user_file_sel, input$poly_id_var)
      )
      return(data_res)
    })

    # table output ####
    output$poly_res_table <- shiny::renderTable({

      shiny::validate(
        shiny::need(data_res(), 'No data yet')
      )

      lidar_var <- tolower(input$lidar_var_sel)
      var_column <- glue::glue("mean_{lidar_var}")

      data_res() %>%
        dplyr::as_tibble() %>%
        dplyr::select(poly_id, !! rlang::sym(var_column))
    })

    ## map output ####
    output$raster_map <- leaflet::renderLeaflet({

      shiny::validate(
        shiny::need(data_res(), 'No data')
        # shiny::need(input$poly_type_sel, 'No polygon type selected'),
        # shiny::need(input$lidar_val_sel, 'No lidar variable selected')
      )

      # band to get from db stack
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

      # raster intermediates
      temp_postgresql_conn <- pool::poolCheckout(lidar_db)
      lidar_raster <- rpostgis::pgGetRast(temp_postgresql_conn, 'lidar_stack', bands = lidar_band)
      pool::poolReturn(temp_postgresql_conn)
      # rm(temp_postgresql_conn)

      palette <- leaflet::colorNumeric(
        viridis::plasma(100),
        # raster::values(basal_area_raster),
        raster::values(lidar_raster),
        na.color = 'transparent'
      )

      # poly intermediates
      poly_type <- input$poly_type_sel
      var_column <- glue::glue('mean_{tolower(input$lidar_var_sel)}')
      user_poly <- data_res() %>%
        sf::st_transform('+proj=longlat +datum=WGS84') %>%
        dplyr::select(poly_id, !! rlang::sym(var_column))

      # proper map
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

    ## download ####
    # modal for saving the data
    shiny::observeEvent(
      eventExpr = input$download_trigger_btn,
      handlerExpr = {

        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(

              shiny::fluidRow(
                shiny::column(
                  12,
                  # format options
                  shiny::radioButtons(
                    'data_format', 'Data format',
                    # text_translate('data_format', lang(), texts_thes),
                    choices = c('shp', 'wkt', 'gpkg'),
                    selected = 'gpkg'
                  ),
                  # length options
                  shiny::radioButtons(
                    'data_length', 'All data?',
                    # text_translate('data_length', lang(), texts_thes),
                    choices = c('visible', 'all_columns'),
                    selected = 'visible', width = '100%'
                  )
                )
              )
            ),
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton('Dismiss'),
              shiny::downloadButton(
                'download_data_with_options',
                label = 'Download',
                class = 'btn-success'
              )
            )
          )
        )
      }
    )

    # download handlers
    output$download_data_with_options <- shiny::downloadHandler(
      filename = function() {
        if (input$data_format == 'shp') {
          'lidar_data.zip'
        } else {
          if (input$data_format == 'wkt') {
            'lidar_data.csv'
          } else {
            'lidar_data.gpkg'
          }
        }
      },
      content = function(file) {

        # data length
        result_data <- data_res() %>%
          sf::st_transform('+proj=longlat +datum=WGS84')

        if (input$data_length == 'visible') {
          var_column <- glue::glue('mean_{tolower(input$lidar_var_sel)}')
          result_data <- result_data %>%
            dplyr::select(poly_id, !! rlang::sym(var_column))
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
            }
          }
        }
      }
    )

    ## file upload observer ####
    shiny::observeEvent(
      eventExpr = input$poly_type_sel,
      handlerExpr = {
        poly_type <- input$poly_type_sel
        if (poly_type == 'File upload') {
          shinyjs::show('tururu')
        } else {
          shinyjs::hide('tururu')
        }
      }
    )

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
