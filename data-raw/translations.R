## Script for creating the translations

tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,

  "sidebar_h4_title", "Controls", 'Controls', "Controles",
  "lidar_val_sel_label", "Seleccioneu la variable que voleu visualitzar", "Select the variable to visualize", "Selecciona la variable a visualizar",
  "poly_type_sel_label", "Seleccioneu els polígons a agregar", "Select the polygons to aggregate", "Selecciona los polígonos a agregar",
  "user_file_sel_label", "Pengeu un fitxer", "Upload a file", "Cargar un archivo",
  "user_file_sel_button_label", "Navegueu", "Browse", "Explora",
  "user_file_sel_placeholder", "No s’ha seleccionat cap fitxer", "No file selected", "No se seleccionado ningún archivo",
  "sidebar_h4_results", "Resultats", "Results", "Resultados",
  "sidebar_h4_download", "Descarrega", "Download", "Descarga",
  "main_panel_raster_siz_1", "Ràster per a la visualització té una mida de cel·la de 100x100 metres.", "Raster for visualization has a cell size of 100x100 meters.", "El ráster para la visualización tiene un tamaño de celda de 100x100 metros.",
  "main_panel_raster_siz_2", "Ràster per als càlculs té una mida de cel·la de 20x20 metres.", "Raster for calculations has a cell size of 20x20 meters.", "El ráster para los cálculos tiene un tamaño de celda de 20x20 metros.",
  "data_format_label", "Format de dades", "Data format", "Formato de los datos",
  "data_length_label", "¿Totes les variables?", "All the variables?", "¿Todas las variables?",
  "modal_dismiss_label", "Cancel·lar", "Dismiss", "Cancelar",
  # lidar_val_sel choices
  'AB', "Àrea Basal (AB)", "Basal Area (AB)", "Área Basal (AB)",
  'BAT',  "Biomassa Aèria Total (BAT)", "Total Aerial Biomass (BAT)", "Biomasa Aérea Total (BAT)",
  'BF', "Biomassa de fulles (BH)", "Leaf Biomass (BH)", "Biomasa de Hojas (BH)",
  'CAT', "Carboni Aéri Total (CAT)", "Total Aerial Carbon (CAT)", "Carbono Aéreo Total (CAT)",
  'DBH', "Diàmetre Normal (DBH)", "Diameter at Breast Height (DBH)", " Diámetro Normal (DBH)",
  'HM', "Altura Mitjana (HM)", "Mean Height (HM)", "Altura Media (HM)",
  'REC', "Recobriment (REC)", "Coating (REC)", "Recubrimiento (REC)",
  'VAE', "Volum amb Escorça (VOB)", "Over Bark Volume(VOB)", "Volúmen con Corteza (VOB)",
  # poly type sel choices
  'Catalonia', "Catalunya", "Catalonia", "Cataluña",
  'Provinces', "Provincies", "Provinces", "Provincias",
  'Counties', "Comarques", "Counties", "Comarcas",
  'Municipalities', "Municipis", "Municipalities", "Municipios",
  'Veguerias', "Vegueries", "Veguerias", "Veguerias",
  'Drawed polygon', "Polígon dibuxat", "Drawn polygon", "Polígono dibujado",
  'File upload', "Fitxer", "File upload", "Archivo",
  # data format options
  'GIS', 'SIG', 'GIS', 'SIG',
  'TABLE', 'Table', 'Table', 'Tabla',
  'shp', 'Shapefile', 'Shapefile', 'Shapefile',
  'wkt', 'Well Known Text', 'Well Known Text', 'Well Known Text',
  'gpkg', 'GeoPackage', 'GeoPackage', 'GeoPackage',
  'csv', 'CSV', 'CSV', 'CSV',
  'xlsx', 'Excel', 'Excel', 'Excel',
  # data length choices
  'all_columns', 'Totes les variables', 'All the variables', 'Todas las variables',
  'visible', 'Només la variable selecionada', 'Only the selected variable', 'Sólo la variable seleccionada',
  # map translations
  'Relief', 'Relleu', 'Relief', 'Relieve',
  'Imaginery', 'Satèl·lit', 'Imaginery', 'Satélite',
  'poly', 'Polígons', 'Polygons', 'Polígonos',
  'lidar', 'LiDAR', 'LiDAR', 'LiDAR',
  # table names
  'poly_id', 'Polígon', 'Polygon', 'Polígono',
  'mean_ab', "Àrea Basal Mitjana (AB)", "Mean Basal Area (AB)", "Área Basal Media (AB)",
  'mean_bat',  "Biomassa Aèria Total Mitjana (BAT)", "Mean Total Aerial Biomass (BAT)", "Biomasa Aérea Total Media (BAT)",
  'mean_bf', "Biomassa de fulles Mitjana (BH)", "Mean Leaf Biomass (BH)", "Biomasa de Hojas Media (BH)",
  'mean_cat', "Carboni Aéri Total Mitjan (CAT)", "Mean Total Aerial Carbon (CAT)", "Carbono Aéreo Total Medio (CAT)",
  'mean_dbh', "Diàmetre Normal Mitjan (DBH)", "Mean Diameter at Breast Height (DBH)", " Diámetro Normal Medio (DBH)",
  'mean_hm', "Altura Mitjana Mitjana (HM)", "Mean Height (HM)", "Altura Media (HM)",
  'mean_rec', "Recobriment Mitjan (REC)", "Mean Coating (REC)", "Recubrimiento Medio (REC)",
  'mean_vae', "Volum amb Escorça Mitjan (VOB)", "Mean Over Bark Volume(VOB)", "Volúmen con Corteza Medio (VOB)",
  # validate
  'data_res_need', 'Encara no hi ha dades', 'No data yet', 'Sin datos todavía',
  'map_click_need', 'No hi ha cap clic del mapa', 'No map click', 'No ha habido click en el mapa',
  'file_need', 'No s’ha seleccionat cap fitxer', 'No file selected', 'No se ha seleccionado ningún archivo',
  'custom_poly_need', 'No s’ha dibuixat cap polígon. Torneu enrere i dibuixeu un polígon abans de seleccionar "Polígon dibuxat" al menú', 'No custom polygon drawed. Please go back and draw a polygon before selecting "Drawn polygon" from the menu', 'No hay ningún polígono dibujado. Vuelva atrás y dibuje un polígono antes de seleccionar "Polígono dibujado" en el menú',
  # coordinates info
  'sidebar_h4_coords', '{input$lidar_var_sel} a les coordenades: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}', '{input$lidar_var_sel} at coordinates: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}', '{input$lidar_var_sel} en las coordenadas: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}',
  'sidebar_p_rawraster', 'Valor per al ràster de 20x20m: {round(click_raster_values()$raw, 3)}', '20x20m raster value: {round(click_raster_values()$raw, 3)}', 'Valor para el ráster de 20x20m: {round(click_raster_values()$raw, 3)}',
  'sidebar_p_aggraster', 'Valor per al ràster de 100x100: {round(click_raster_values()$agg, 3)}', '100x100 raster value: {round(click_raster_values()$agg, 3)}', 'Valor para el ráster de 100x100: {round(click_raster_values()$agg, 3)}',

  ## TODO continue translations thesaurus
) %>%
  {.} -> app_translations

usethis::use_data(app_translations, internal = TRUE, overwrite = TRUE)
