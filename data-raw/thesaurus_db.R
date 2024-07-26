library(tidyverse)

conn <- RPostgres::dbConnect(
  'PostgreSQL', host = 'laboratoriforestal.creaf.cat', dbname = 'lidargis', user = 'ifn',
  password = Sys.getenv('ifn_db')
)

thes_df <- tibble::tribble(
  ~var_id, ~translation_cat, ~translation_eng, ~translation_spa, ~var_units, ~var_description_eng,

  # lidar_val_sel choices
  'AB', "Àrea Basal", "Basal Area", "Área Basal", "m²/ha", "Calculation based on LiDAR flights and National Forest Inventory data",
  'BAT',  "Biomassa Aèria Total", "Total Aerial Biomass", "Biomasa Aérea Total", "t/ha", "Calculation based on LiDAR flights and National Forest Inventory data",
  'BF', "Biomassa de fulles", "Leaf Biomass", "Biomasa de Hojas", "t/ha", "Calculation based on LiDAR flights and National Forest Inventory data",
  'CAT', "Carboni Aéri Total", "Total Aerial Carbon", "Carbono Aéreo Total", "t/ha", "Calculation based on LiDAR flights and National Forest Inventory data",
  'DBH', "Diàmetre Normal", "Diameter at Breast Height", " Diámetro Normal", "cm", "Calculation based on LiDAR flights and National Forest Inventory data",
  'HM', "Altura Mitjana", "Mean Height", "Altura Media", "m", "Calculation based on LiDAR flights and National Forest Inventory data",
  'REC', "Recobriment", "Cover", "Recubrimiento", "%", "Calculation based on LiDAR flights and National Forest Inventory data",
  'VAE', "Volum amb Escorça", "Over Bark Volume", "Volúmen con Corteza", "m³/ha", "Calculation based on LiDAR flights and National Forest Inventory data",
  'DEN', "Densitat", "Density", "Densidad", "trees/ha", "Calculation based on LiDAR flights and National Forest Inventory data",
  'LAI', "Índex d'àrea foliar", "Leaf area index", "Índice de área foliar", "m²/m²", "Calculation based on LiDAR flights and National Forest Inventory data"
)

dplyr::copy_to(
  conn, df = thes_df, name = 'variables_thesaurus', overwrite = TRUE, temporary = FALSE,
  indexes = list(
    'var_id'
  )
)

# disconnect the db ####
RPostgres::dbDisconnect(conn)

