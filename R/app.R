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
  lidar_db <- RPostgreSQL::dbConnect(
    'PostgreSQL', host = host, dbname = dbname, user = user,
    password = password, port = port
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
            shiny::tableOutput('poly_res_table'),

            # download buttons
            shinyWidgets::actionBttn(
              'download_trigger_btn', 'Download', icon = shiny::icon('download'),
              color = 'success', size = 'sm'
            )
          ),
          mainPanel = shiny::mainPanel(
            width = 8,
            # map module
            shiny::div(
              class = 'mapouter',
              leaflet::leafletOutput('raster_map', height = '100%')
            )
          )
        ) # end of sidebar layout
      ) # end of fluidPage
    }) # end of exploreUI

    # proper server ####

    # poly_reactive
    poly_to_sf <- shiny::reactive({

      poly_sel <- switch (input$poly_type_sel,
        'Catalonia' = catalonia_poly(),
        'Provinces' = provinces_poly(),
        'Counties' = counties_poly(),
        'Municipalities' = municipalities_poly(),
        'Veguerias' = veguerias_poly(),
        'Drawed polygon' = drawed_poly(),
        'File upload' = file_poly()
      )

    })


    output$poly_res_table <- shiny::renderTable({

      lidar_var <- input$lidar_var_sel



    })


  } # end of server function

  # Run the application
  lidarapp <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      RPostgreSQL::dbDisconnect(lidar_db)
    }
  )

  # shiny::runApp(nfi_app)
  return(lidarapp)

}
