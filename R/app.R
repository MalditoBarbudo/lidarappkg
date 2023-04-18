#' function to launch the lidar app
#'
#' @export
lidar_app <- function() {

  ### DB access ################################################################
  lidardb <- lfcdata::lidar()

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'lidarappkg')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue(
      "<img class='flag-image' src='images/cat.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/spa.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/eng.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    )
  )

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    "var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});"
  )

  ## UI ####
  ui <- shiny::tagList(
    # js script,
    shiny::tags$script(keep_alive_script),

    # shinyjs
    shinyjs::useShinyjs(),
    # shinyWidgets::chooseSliderSkin(skin = "Shiny", color = '#0DB3D4'),
    # shinyWidgets::useSweetAlert(),

    # waiter
    waiter::use_waiter(),
    waiter::use_hostess(),

    # css
    shiny::tags$head(
      # corporative image css
      shiny::includeCSS(
        system.file('resources', 'corp_image.css', package = 'lidarappkg')
      ),
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
        title = mod_tabTranslateOutput('main'),
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
        # shiny::uiOutput('explore_ui')
        # we use modules now, as in ifn app.
        shiny::sidebarLayout(
          position = 'left',
          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # data selection module:
            mod_dataInput('mod_dataInput'),
            shiny::br(),
            mod_saveOutput('mod_saveOutput')
          ),
          mainPanel = shiny::mainPanel(
            width = 8,
            shiny::tabsetPanel(
              id = 'main_panel_tabset', type = 'pills',
              shiny::tabPanel(
                title = mod_tabTranslateOutput('map'),
                # 'map',
                value = 'map_panel',
                mod_mapOutput('mod_mapOutput')
              ),
              shiny::tabPanel(
                title = mod_tabTranslateOutput('table'),
                # 'table',
                value = 'table_panel',
                mod_tableOutput('mod_tableOutput')
              )
            )
          )
        )

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

    ## modules calling ####
    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput', lang, app_translations
    )
    # main_data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput', lang, app_translations,
      data_reactives, map_reactives,
      lidardb
    )
    # save output
    shiny::callModule(
      mod_save, 'mod_saveOutput', lang, app_translations,
      main_data_reactives
    )
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput', lang, app_translations,
      main_data_reactives, data_reactives
    )
    # table
    table_reactives <- shiny::callModule(
      mod_table, 'mod_tableOutput', lang, app_translations,
      main_data_reactives
    )
    ## tab title translations
    shiny::callModule(
      mod_tabTranslate, 'main', lang, app_translations, 'main_translation'
    )
    shiny::callModule(
      mod_tabTranslate, 'map', lang, app_translations, 'map_translation'
    )
    shiny::callModule(
      mod_tabTranslate, 'table', lang, app_translations, 'table_translation'
    )

    ## observers ####
    # modal observers, triggered by map shape click or table row click
    shiny::observeEvent(
      eventExpr = map_reactives$lidar_map_shape_click,
      handlerExpr = {

        # id
        id_click <- map_reactives$lidar_map_shape_click$id
        # module call
        shiny::callModule(
          mod_info, 'mod_infoUI', lang, app_translations,
          main_data_reactives, data_reactives, id_click
        )
        # modal
        shiny::showModal(
          shiny::modalDialog(
            mod_infoUI('mod_infoUI'),
            footer = shiny::modalButton(
              translate_app('dismiss', lang(), app_translations)
            ),
            size = 'l', easyClose = TRUE
          )
        )
      }
    )
    shiny::observeEvent(
      eventExpr = table_reactives$lidar_table_rows_selected,
      handlerExpr = {

        # id
        id_index <- table_reactives$lidar_table_rows_selected
        id_click <- main_data_reactives$data_visible$poly_id[id_index]
        # module call
        shiny::callModule(
          mod_info, 'mod_infoUI', lang, app_translations,
          main_data_reactives, data_reactives, id_click
        )
        # modal
        shiny::showModal(
          shiny::modalDialog(
            mod_infoUI('mod_infoUI'),
            footer = shiny::modalButton(
              translate_app('dismiss', lang(), app_translations)
            ),
            size = 'l', easyClose = TRUE
          )
        )
      }
    )
  } # end of server function

  # Run the application
  lidarapp <- shiny::shinyApp(
    ui = ui, server = server
  )
  return(lidarapp)
}
