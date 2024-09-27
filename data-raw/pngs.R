## libraries
library(terra)
library(dplyr)
library(base64enc)

## raster by raster loop
raster_stack <- lfcdata::lidar()$get_lowres_raster(
  toupper(c('ab', 'bat', 'bf', 'cat', 'dbh', 'den', 'hm', 'lai', 'rec', 'vae')),
  spatial = "raster"
)

png_table <-
  c('ab', 'bat', 'bf', 'cat', 'dbh', 'den', 'hm', 'lai', 'rec', 'vae') |>
  toupper() |>
  purrr::map(
    .f = \(variable) {
      palette_selected <- "Rocket"
      pngs_folder <- "data-raw/pngs/"
      png_file <- paste0(pngs_folder, variable, "_rgb.png")

      rounded_rast <- round(
        terra::project(raster_stack[[variable]], "epsg:4326") * 1000, 0
      )

      rounded_values <- terra::values(rounded_rast)
      
      color_table <- dplyr::as_tibble(rounded_values) |>
        dplyr::distinct() |>
        dplyr::rename(value = {{ variable }}) |>
        dplyr::mutate(
          col = scales::col_numeric(
            hcl.colors(10, palette_selected),
            c(min(value, na.rm = TRUE), max(value, na.rm = TRUE)),
            na.color = "#FFFFFF00", reverse = FALSE, alpha = TRUE
          )(value)
        ) |>
        as.data.frame()
      
      terra::coltab(rounded_rast) <- color_table
      
      rounded_colorized <- terra::colorize(
        rounded_rast, "rgb",
        filename = png_file,
        NAflag = 255, overwrite = TRUE
      )
      
      rounded_ext <- terra::ext(rounded_colorized)
      png_base64_string <- paste0(
        "data:image/png;base64,",
        base64enc::base64encode(png_file)
      )
      
      dplyr::tibble(
        var = variable,
        palette_selected = palette_selected,
        base64_string = png_base64_string,
        left_ext = as.numeric(rounded_ext[1]),
        down_ext = as.numeric(rounded_ext[3]),
        right_ext = as.numeric(rounded_ext[2]),
        up_ext = as.numeric(rounded_ext[4]),
        min_value = min(color_table[["value"]], na.rm = TRUE) / 1000,
        max_value = max(color_table[["value"]], na.rm = TRUE) / 1000
      )
    }
  ) |>
  purrr::list_rbind()

## Write to db
# db conn
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  host = 'laboratoriforestal.creaf.cat', dbname = 'lidargis',
  user = 'ifn',
  password = Sys.getenv('ifn_db')
)

dplyr::copy_to(conn, png_table, "pngs", overwrite = TRUE, temporary = FALSE)

RPostgres::dbDisconnect(conn)

# benchmark
# lidardb <- lfcdata::lidar()

# tictoc::tic()
# foo <- lidardb$get_lowres_raster("AB", "raster")
# tictoc::toc()

# tictoc::tic()
# bar <- lidardb$.__enclos_env__$super$get_data("pngs")
# tictoc::toc()

# mapdeck::set_token(Sys.getenv("MAPBOX_TOKEN"))
# mapdeck::mapdeck(
#   location = c(1.5, 41.25), zoom = 10, style = mapdeck::mapdeck_style("satellite"),
#   pitch = 22
# ) |>
#   mapdeck::add_bitmap(
#     image = bar$base64_string[1],
#     bounds = c(bar$left_ext[1], bar$down_ext[1], bar$right_ext[1], bar$up_ext[1]) |>
#       as.numeric(),
#     update_view = TRUE, focus_layer = TRUE,
#     transparent_colour = "#00000000"
#   )
