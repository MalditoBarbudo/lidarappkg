library(tidyverse)

tibble::tribble(
  ~var_id, ~translation_cat, ~translation_eng, ~translation_spa, ~var_units, ~var_description_eng,

  # lidar_val_sel choices
  'AB', "Àrea Basal", "Basal Area", "Área Basal", "[m²/ha]", "",
  'BAT',  "Biomassa Aèria Total", "Total Aerial Biomass", "Biomasa Aérea Total", "[t/ha]", "",
  'BF', "Biomassa de fulles", "Leaf Biomass", "Biomasa de Hojas", "[t/ha]", "",
  'CAT', "Carboni Aéri Total", "Total Aerial Carbon", "Carbono Aéreo Total", "[t/ha]", "",
  'DBH', "Diàmetre Normal", "Diameter at Breast Height", " Diámetro Normal", "[cm]", "",
  'HM', "Altura Mitjana", "Mean Height", "Altura Media", "[m]", "",
  'REC', "Recobriment", "Coating", "Recubrimiento", "[%]", "",
  'VAE', "Volum amb Escorça", "Over Bark Volume", "Volúmen con Corteza", "[m³/ha]", ""
)
