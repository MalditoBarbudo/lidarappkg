## libraries ####
library(raster)
library(rayshader)
library(lidarappkg)
library(tidyverse)

## AB rayshader ####
ab_raster <- raster::raster('data-raw/AB_aggregated20.grd')
ab_raster %>%
  raster::extract(raster::extent(.), buffer = 1000) %>%
  matrix(nrow = ncol(ab_raster), ncol = nrow(ab_raster)) -> abmat

abmat %>%
  sphere_shade(texture = 'imhof1') %>%
  plot_map()

abmat %>%
  sphere_shade(texture = 'imhof1') %>%
  add_water(detect_water(abmat), color = 'desert') %>%
  plot_map()

abraymat <- ray_shade(abmat)

abmat %>%
  sphere_shade(texture = 'imhof1') %>%
  add_water(detect_water(abmat), color = 'desert') %>%
  add_shadow(abraymat) %>%
  plot_map()

abambmat <- ambient_shade(abmat)

abmat %>%
  sphere_shade(texture = 'imhof1') %>%
  add_water(detect_water(abmat), color = 'desert') %>%
  add_shadow(abraymat) %>%
  add_shadow(abambmat) %>%
  plot_map()

abmat %>%
  sphere_shade(texture = 'imhof1') %>%
  add_water(detect_water(abmat), color = 'desert') %>%
  add_shadow(ray_shade(abmat), 0.5) %>%
  add_shadow(abambmat, 0.5) %>%
  plot_3d(abmat)
render_depth(focus = 0.7)
render_snapshot()
