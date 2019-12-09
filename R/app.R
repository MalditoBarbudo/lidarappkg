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

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'lidarappkg')
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
      ),
      # corporative image css
      shiny::includeCSS(
        system.file('resources', 'corp_image.css', package = 'lidarappkg')
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
    #   input$raster_map_shape_click
    # })
    # output$debug2 <- shiny::renderPrint({
    #   input$raster_map_click
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
            shiny::h4(translate_app('sidebar_h4_title', lang_declared)),

            # var input
            shiny::selectInput(
              'lidar_var_sel', translate_app('lidar_val_sel_label', lang_declared),
              choices = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE') %>%
                magrittr::set_names(translate_app(., lang_declared)),
              selected = 'AB'
            ),

            # poly input
            shiny::selectInput(
              'poly_type_sel', translate_app('poly_type_sel_label', lang_declared),
              choices = c(
                'Catalonia', 'Provinces', 'Counties', 'Municipalities', 'Veguerias',
                'Drawed polygon', 'File upload'
              ) %>% magrittr::set_names(translate_app(., lang_declared)),
              selected = 'Provinces'
            ),

            # hidden file selector div
            shinyjs::hidden(
              shiny::div(
                id = 'file_sel_div',
                shiny::fluidRow(
                  shiny::column(
                    12,
                    shiny::fileInput(
                      'user_file_sel',
                      translate_app('user_file_sel_label', lang_declared),
                      accept = c('zip', 'gpkg'),
                      buttonLabel = translate_app('user_file_sel_button_label', lang_declared),
                      placeholder = translate_app('user_file_sel_placeholder', lang_declared)
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
            shiny::h4(translate_app('sidebar_h4_results', lang_declared)),
            # shiny::tableOutput('poly_res_table'),
            DT::DTOutput('poly_res_table'),

            shinyjs::hidden(
              shiny::uiOutput('click_info')
            ),

            # download buttons
            shiny::h4(translate_app('sidebar_h4_download', lang_declared)),
            shiny::actionButton(
              'download_trigger_btn', translate_app('sidebar_h4_download', lang_declared),
              icon = shiny::icon('download')
            )
          ),
          mainPanel = shiny::mainPanel(
            width = 8,
            leaflet::leafletOutput('raster_map', height = 600) %>%
              shinyWidgets::addSpinner(spin = 'cube', color = '#26a65b'),
            shiny::p(
              translate_app('main_panel_raster_siz_1', lang_declared),
              translate_app('main_panel_raster_siz_2', lang_declared)
            )
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
        'Drawed polygon' = drawed_poly(lidar_db, input$raster_map_draw_all_features, lang()),
        'File upload' = file_poly(lidar_db, input$user_file_sel, input$poly_id_var, lang())
      )
      return(data_res)
    })

    # table output ####
    # output$poly_res_table <- shiny::renderTable({
    output$poly_res_table <- DT::renderDT({

      shiny::validate(
        shiny::need(data_res(), translate_app('data_res_need', lang()))
      )

      lidar_var <- tolower(input$lidar_var_sel)
      var_column <- glue::glue("mean_{lidar_var}")
      lang_declared <- lang()
      # table_page <- selected_row()$page
      selected <- selected_row()$row
      if (input$poly_type_sel %in% c('Counties', 'Municipalities')) {
        displayStart <- selected - 1
      } else {
        displayStart <- 0
      }


      # browser()

      data_res() %>%
        dplyr::as_tibble() %>%
        dplyr::select(
          dplyr::one_of(c('poly_id', 'comarca', 'provincia', var_column))
        ) %>%
        dplyr::mutate_if(is.numeric, ~round(., 3)) %>%
        magrittr::set_names(
          translate_app(names(.), lang_declared)
        ) %>%
        DT::datatable(
          class = 'compact hover nowrap row-border order-column',
          selection = list(
            mode = 'single', selected = selected, target = 'row'
          ),
          # extensions = 'Scroller',
          options = list(
            dom = 'trp',
            displayStart = displayStart,
            pageLength = 10
            # lengthMenu = c(10, 25, 50),
            # deferRender = FALSE,
            # scrollY = '250px'
          )
        )
    })

    ## map output ####
    output$raster_map <- leaflet::renderLeaflet({

      shiny::validate(
        shiny::need(data_res(), translate_app('data_res_need', lang()))
        # shiny::need(input$poly_type_sel, 'No polygon type selected'),
        # shiny::need(input$lidar_val_sel, 'No lidar variable selected')
      )

      lang_declared <- lang()

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
      lidar_raster <- rpostgis::pgGetRast(
        temp_postgresql_conn,
        c('public', 'lidar_stack'), bands = lidar_band
      )
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
        leaflet::setView(1.744, 41.726, zoom = 8) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldShadedRelief,
          group = 'Relief' %>% translate_app(lang_declared),
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = 'Imaginery' %>% translate_app(lang_declared),
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addMapPane('polys', zIndex = 410) %>%
        leaflet::addMapPane('rasters', zIndex = 420) %>%
        leaflet::addLayersControl(
          baseGroups = c('Relief', 'Imaginery') %>% translate_app(lang_declared),
          overlayGroups = c('lidar', 'poly') %>%
            translate_app(lang_declared) %>%
            purrr::map_chr(~ glue::glue(.x)),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
        ) %>%
        leaflet::hideGroup('lidar' %>% translate_app(lang_declared)) %>%
        leaflet::removeImage('raster') %>%
        leaflet::clearGroup('poly' %>%
                              translate_app(lang_declared) %>%
                              purrr::map_chr(~ glue::glue(.x))) %>%
        leaflet::addRasterImage(
          lidar_raster, project = FALSE, colors = palette, opacity = 1,
          group = 'lidar' %>% translate_app(lang_declared), layerId = 'raster'
        ) %>%
        leaflet::addPolygons(
          data = user_poly,
          group = 'poly' %>%
            translate_app(lang_declared) %>%
            purrr::map_chr(~ glue::glue(.x)),
          label = ~poly_id,
          layerId = ~poly_id,
          weight = 1, smoothFactor = 1,
          opacity = 1.0, fill = TRUE,
          color = '#6C7A89FF', fillColor = palette(user_poly[[var_column]]),
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
          pal = palette, values = raster::values(lidar_raster),
          title = input$lidar_var_sel %>% translate_app(lang_declared), position = 'bottomright',
          opacity = 1
        ) %>%
        # leaflet.extras plugins
        leaflet.extras::addDrawToolbar(
          targetGroup = 'poly' %>%
            translate_app(lang_declared) %>%
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
    })

    ## map and table reactives and observers ####
    # reactive to select the row of clicked polygon
    selected_row <- shiny::reactive({

      clicked <- input$raster_map_shape_click

      # browser()
      if (!is.null(clicked)) {
        row_index <- data_res() %>%
          dplyr::mutate(no = dplyr::row_number()) %>%
          dplyr::filter(poly_id == clicked$id) %>%
          dplyr::pull(no)

        if (length(row_index) < 1) {
          row_index <- 0
        }
      } else {
        row_index <- 0
      }
      return(list(row = row_index))
    })

    click_raster_values <- shiny::reactive({
      shiny::validate(
        shiny::need(input$raster_map_click, translate_app('map_click_need', lang()))
      )
      # map click
      map_click <- input$raster_map_click
      # var input
      lidar_band <- switch(
        input$lidar_var_sel,
        'AB' = 1, 'BAT' = 6, 'BF' = 4, 'CAT' = 7,
        'DBH' = 2, 'HM' = 3, 'REC' = 5, 'VAE' = 8
      )

      # browser()
      # get the raster value
      raster_query <- glue::glue(
        "SELECT ST_Value(
           rast,
           ST_Transform(ST_SetSRID(ST_Makepoint({map_click$lng},{map_click$lat}),4326),3043)
         ) As foo
         FROM {tolower(input$lidar_var_sel)}
         WHERE ST_Intersects(
           rast,
           ST_Transform(ST_SetSRID(ST_Makepoint({map_click$lng},{map_click$lat}),4326),3043)
         );"
      )
      # query for 400x400
      raster_agg_query <- glue::glue(
        "SELECT ST_Value(
           rast,
           {lidar_band},
           ST_Transform(ST_SetSRID(ST_Makepoint({map_click$lng},{map_click$lat}),4326),900913)
         ) As foo
         FROM lidar_stack
         WHERE ST_Intersects(
           rast,
           ST_Transform(ST_SetSRID(ST_Makepoint({map_click$lng},{map_click$lat}),4326),900913)
         );"
      )

      raster20x20_val <- pool::dbGetQuery(lidar_db, raster_query) %>% dplyr::pull(foo)
      raster400x400_val <- pool::dbGetQuery(lidar_db, raster_agg_query) %>% dplyr::pull(foo)

      return(list(raw = raster20x20_val, agg = raster400x400_val))
    })

    # observer to watch the click in map to show rasters
    # values of the clicked coordinates
    shiny::observeEvent(
      eventExpr = input$raster_map_click,
      handlerExpr = {
        # shinyjs::show('click_info')
      }
    )

    output$click_info <- renderUI({
      map_click <- input$raster_map_click
      shiny::tagList(
        shiny::wellPanel(
          shiny::h4(
            glue::glue(
              translate_app('sidebar_h4_coords', lang())
            )
          ),
          shiny::p(glue::glue(
            translate_app('sidebar_p_rawraster', lang())
          )),
          shiny::p(glue::glue(
            translate_app('sidebar_p_aggraster', lang())
          ))
        )
      )
    })

    ## download ####
    # modal for saving the data
    shiny::observeEvent(
      eventExpr = input$download_trigger_btn,
      handlerExpr = {

        lang_declared = lang()

        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(

              shiny::fluidRow(
                shiny::column(
                  12,
                  # format options
                  shiny::selectInput(
                    'data_format', translate_app('data_format_label', lang_declared),
                    choices = list(
                      'GIS' = c('shp', 'wkt', 'gpkg') %>%
                        magrittr::set_names(translate_app(., lang_declared)),
                      'TABLE' = c('csv', 'xlsx') %>%
                        magrittr::set_names(translate_app(., lang_declared))
                    ) %>% magrittr::set_names(translate_app(names(.), lang_declared)),
                    selected = 'gpkg'
                  ),
                  # shiny::radioButtons(
                  #   'data_format', 'Data format',
                  #   # text_translate('data_format', lang(), texts_thes),
                  #   choices = list('GIS' = c('shp', 'wkt', 'gpkg'),
                  #                  'TABLE' = c('csv', 'xlsx')),
                  #   selected = 'gpkg'
                  # ),
                  # length options
                  shiny::radioButtons(
                    'data_length', translate_app('data_length_label', lang_declared),
                    # text_translate('data_length', lang(), texts_thes),
                    choices = c('visible', 'all_columns') %>%
                      magrittr::set_names(translate_app(., lang_declared)),
                    selected = 'visible', width = '100%'
                  )
                )
              )
            ),
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton(translate_app('modal_dismiss_label', lang_declared)),
              shiny::downloadButton(
                'download_data_with_options',
                label = translate_app('sidebar_h4_download', lang_declared),
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

    ## file upload observer ####
    shiny::observeEvent(
      eventExpr = input$poly_type_sel,
      handlerExpr = {
        poly_type <- input$poly_type_sel
        if (poly_type == 'File upload') {
          shinyjs::show('file_sel_div')
        } else {
          shinyjs::hide('file_sel_div')
        }
      }
    )

    ## drawed poly observer ####

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
