## Script for creating the translations

tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  # tabs
  "map_translation", "Mapa", "Map", "Mapa",
  "table_translation", "Taula", "Table", "Tabla",
  "main_translation", "Explora", "Explore", "Explora",
  "sidebar_h4_title", "Controls", 'Controls', "Controles",
  "lidar_var_sel_label", "Seleccioneu la variable que voleu visualitzar", "Select the variable to visualize", "Selecciona la variable a visualizar",
  "poly_type_sel_label", "Seleccioneu els polígons a agregar", "Select the polygons to aggregate", "Selecciona los polígonos a agregar",
  "user_file_sel_label", "Pengeu un fitxer", "Upload a file", "Cargar un archivo",
  "user_file_sel_button_label", "Navegueu", "Browse", "Explora",
  "user_file_sel_placeholder", "No s’ha seleccionat cap fitxer", "No file selected", "No se seleccionado ningún archivo",
  "file_text", 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels polígons continguts.', 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained polygons.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de los polígonos contenidos.',
  "sidebar_h4_results", "Resultats", "Results", "Resultados",
  "sidebar_h4_download", "Descarrega", "Download", "Descarga",
  "main_panel_raster_siz_1", "Ràster per a la visualització té una mida de cel·la de 400x400 metres.", "Raster for visualization has a cell size of 400x400 meters.", "El ráster para la visualización tiene un tamaño de celda de 400x400 metros.",
  "main_panel_raster_siz_2", "Ràster per als càlculs té una mida de cel·la de 20x20 metres.", "Raster for calculations has a cell size of 20x20 meters.", "El ráster para los cálculos tiene un tamaño de celda de 20x20 metros.",
  "data_format_label", "Format de dades", "Data format", "Formato de los datos",
  "data_length_label", "¿Totes les variables?", "All the variables?", "¿Todas las variables?",
  "modal_dismiss_label", "Cancel·lar", "Dismiss", "Cancelar",
  # lidar_var_sel choices
  'AB', "Àrea Basal [m²/ha]", "Basal Area [m²/ha]", "Área Basal [m²/ha]",
  'BAT',  "Biomassa Aèria Total [t/ha]", "Total Aerial Biomass [t/ha]", "Biomasa Aérea Total [t/ha]",
  'BF', "Biomassa de fulles [t/ha]", "Leaf Biomass [t/ha]", "Biomasa de Hojas [t/ha]",
  'CAT', "Carboni Aéri Total [t/ha]", "Total Aerial Carbon [t/ha]", "Carbono Aéreo Total [t/ha]",
  'DBH', "Diàmetre Normal [cm]", "Diameter at Breast Height [cm]", " Diámetro Normal [cm]",
  'HM', "Altura Mitjana [m]", "Mean Height [m]", "Altura Media [m]",
  'REC', "Recobriment [%]", "Coating [%]", "Recubrimiento [%]",
  'VAE', "Volum amb Escorça [m³/ha]", "Over Bark Volume [m³/ha]", "Volúmen con Corteza [m³/ha]",
  'LAI', "Índex d'àrea foliar [m²/m²]", "Leaf area index [m²/m²]", "Índice de área foliar [m²/m²]",
  'DEN', "Densitat [trees/ha]", "Density [trees/ha]", "Densidad [trees/ha]",
  # poly type sel choices
  "aut_community", "Catalunya", "Catalonia", "Cataluña",
  "province", "Provincies", "Provinces", "Provincias",
  "vegueria", "Vegueries", "Veguerias", "Veguerias",
  "region", "Comarques", "Counties", "Comarcas",
  "municipality", "Municipis", "Municipalities", "Municipios",
  "natural_interest_area", "Àrees d'interès natural", "Natural interest areas", "Áreas de interés natural",
  "special_protection_natural_area", "Àrees naturals de protecció especial", "Special protection natural areas", "Áreas naturales de protección especial",
  "natura_network_2000", "Xarxa Natura 2000", "Natura 2000 network", "Red Natura 2000",
  "file", "Arxiu de polìgons", "Polygon file", "Archivo de polígonos",
  "drawn_poly", "Polígon dibuixat", "Drawn polygon", "Polígono dibujado",
  # poly_types_info_titles
  "province_info_plot_title", "{click_value} comparat amb altres províncies", "{click_value} compared to other provinces", "{click_value} comparado con otras provincias",
  "vegueria_info_plot_title", "{click_value} comparat amb altres vegueries", "{click_value} compared to other veguerias", "{click_value} comparado con otras veguerias",
  "region_info_plot_title", "{click_value} comparat amb altres comarques", "{click_value} compared to other counties", "{click_value} comparado con otras comarcas",
  "municipality_info_plot_title", "{click_value} comparat amb altres municipis", "{click_value} compared to other municipalities", "{click_value} comparado con otros municipios",
  "natural_interest_area_info_plot_title", "{click_value} comparat amb altres arees", "{click_value} compared to other areas", "{click_value} comparado con otras áreas",
  "special_protection_natural_area_info_plot_title", "{click_value} comparat amb altres arees", "{click_value} compared to other areas", "{click_value} comparado con otras áreas",
  "natura_network_2000_info_plot_title", "{click_value} comparat amb altres arees", "{click_value} compared to other areas", "{click_value} comparado con otras áreas",
  "file_info_plot_title", "{click_value} comparat amb altres polígons", "{click_value} compared to other polygons", "{click_value} comparado con otros polígonos",
  "drawn_poly_info_plot_title",  "{click_value} comparat amb altres polígons", "{click_value} compared to other polygons", "{click_value} comparado con otros polígonos",
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
  'Relief', 'Relleu (base)', 'Relief (base)', 'Relieve (base)',
  'Imaginery', 'Satèl·lit (base)', 'Imaginery (base)', 'Satélite (base)',
  'poly', '{data_reactives$poly_type_sel |> translate_app(lang(), app_translations)} (capa)', '{data_reactives$poly_type_sel |> translate_app(lang(), app_translations)} (layer)', '{data_reactives$poly_type_sel |> translate_app(lang(), app_translations)} (capa)',
  'lidar', 'LiDAR (capa)', 'LiDAR (layer)', 'LiDAR (capa)',
  # show poly
  'show_polys', 'Veure polígons', 'Show polygons', 'Ver polígonos',
  # show 3d
  'poly_3d', 'Vista en 3D', '3D view', 'Vista en 3D',
  'using_3d', "Per modificar l'angle de visió mantenir apretat Ctrl mentre s'arrossega amb el ratolí", 'To modify the viewing angle, hold down Ctrl while dragging with the mouse', 'Para modificar el ángulo de visión mantener apretado Ctrl mientras se arrastra con el ratón',
  # table names
  'poly_id', 'Polígon', 'Polygon', 'Polígono',
  'poly_km2', 'Àrea del polígon [km2]', 'Polygon area [km2]', 'Área del polígono [km2]',
  'AB_average', "Àrea Basal Mitjana [m²/ha]", "Mean Basal Area [m²/ha]", "Área Basal Media [m²/ha]",
  'BAT_average',  "Biomassa Aèria Total Mitjana [t/ha]", "Mean Total Aerial Biomass [t/ha]", "Biomasa Aérea Total Media [t/ha]",
  'BF_average', "Biomassa de fulles Mitjana [t/ha]", "Mean Leaf Biomass [t/ha]", "Biomasa de Hojas Media [t/ha]",
  'CAT_average', "Carboni Aéri Total Mitjan [t/ha]", "Mean Total Aerial Carbon [t/ha]", "Carbono Aéreo Total Medio [t/ha]",
  'DBH_average', "Diàmetre Normal Mitjan [cm]", "Mean Diameter at Breast Height [cm]", " Diámetro Normal Medio [cm]",
  'HM_average', "Altura Mitjana Mitjana [m]", "Mean Height [m]", "Altura Media [m]",
  'REC_average', "Recobriment Mitjan [%]", "Mean Coating [%]", "Recubrimiento Medio [%]",
  'VAE_average', "Volum amb Escorça Mitjan [m³/ha]", "Mean Over Bark Volume[m³/ha]", "Volúmen con Corteza Medio [m³/ha]",
  'LAI_average', "Índex d'Àrea Foliar Mitjan [m²/m²]", "Mean Leaf Area Index [m²/m²]", "Índice de Área Foliar Medio [m²/m²]",
  'DEN_average', "Densitat Mitjana [trees/ha]", "Mean Density [trees/ha]", "Densidad Media [trees/ha]",
  'AB_pixels', "Pixels", "Pixels", "Pixels",
  'BAT_pixels',  "Pixels", "Pixels", "Pixels",
  'BF_pixels', "Pixels", "Pixels", "Pixels",
  'CAT_pixels', "Pixels", "Pixels", "Pixels",
  'DBH_pixels', "Pixels", "Pixels", "Pixels",
  'HM_pixels', "Pixels", "Pixels", "Pixels",
  'REC_pixels', "Pixels", "Pixels", "Pixels",
  'VAE_pixels', "Pixels", "Pixels", "Pixels",
  'LAI_pixels', "Pixels", "Pixels", "Pixels",
  'DEN_pixels', "Pixels", "Pixels", "Pixels",
  'AB_min', "Mìnim", "Minimum", "Mínimo",
  'BAT_min',  "Mìnim", "Minimum", "Mínimo",
  'BF_min', "Mìnim", "Minimum", "Mínimo",
  'CAT_min', "Mìnim", "Minimum", "Mínimo",
  'DBH_min', "Mìnim", "Minimum", "Mínimo",
  'HM_min', "Mìnim", "Minimum", "Mínimo",
  'REC_min', "Mìnim", "Minimum", "Mínimo",
  'VAE_min', "Mìnim", "Minimum", "Mínimo",
  'LAI_min', "Mìnim", "Minimum", "Mínimo",
  'DEN_min', "Mìnim", "Minimum", "Mínimo",
  'AB_max', "Màxim", "Maximum", "Máximo",
  'BAT_max',  "Màxim", "Maximum", "Máximo",
  'BF_max', "Màxim", "Maximum", "Máximo",
  'CAT_max', "Màxim", "Maximum", "Máximo",
  'DBH_max', "Màxim", "Maximum", "Máximo",
  'HM_max', "Màxim", "Maximum", "Máximo",
  'REC_max', "Màxim", "Maximum", "Máximo",
  'VAE_max', "Màxim", "Maximum", "Máximo",
  'LAI_max', "Màxim", "Maximum", "Máximo",
  'DEN_max', "Màxim", "Maximum", "Máximo",
  'AB_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'BAT_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'BF_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'CAT_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'DBH_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'HM_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'REC_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'VAE_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'LAI_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'DEN_sd', "Desviació estàndard", "Standard deviation", "Desviación estándar",
  'AB_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'BAT_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'BF_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'CAT_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'DBH_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'HM_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'REC_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'VAE_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'LAI_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'DEN_km2', "Àrea coberta per ràster [km2]", "Area covered by raster [km2]", "Área cubierta por ráster [km2]",
  'AB_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'BAT_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'BF_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'CAT_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'DBH_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'HM_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'REC_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'VAE_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'LAI_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  'DEN_km2_perc', "Àrea coberta per ràster [%]", "Area covered by raster [%]", "Área cubierta por ráster [%]",
  # validate
  'data_res_need', 'Encara no hi ha dades', 'No data yet', 'Sin datos todavía',
  'map_click_need', 'No hi ha cap clic del mapa', 'No map click', 'No ha habido click en el mapa',
  'file_need', 'No s’ha seleccionat cap fitxer', 'No file selected', 'No se ha seleccionado ningún archivo',
  'custom_poly_need', 'No s’ha dibuixat cap polígon. Torneu enrere i dibuixeu un polígon abans de seleccionar "Polígon dibuxat" al menú', 'No custom polygon drawed. Please go back and draw a polygon before selecting "Drawn polygon" from the menu', 'No hay ningún polígono dibujado. Vuelva atrás y dibuje un polígono antes de seleccionar "Polígono dibujado" en el menú',
  'polygon_area_need', "L'àrea del polígon (o la suma de les àrees de polígons) està per sobre del valor màxim ({round(user_area/1000000, 1)} > 500 km2)", 'Polygon area (or polygons sum of areas) are above the maximum value ({round(user_area/1000000, 1)} > 500 km2)', 'El área del polígono (o la suma de las áreas de los polígonos) está por encima del valor máximo ({round(user_area/1000000, 1)} > 500 km2)',
  'feature_number_need', "El nombre d'objectes (polígons) és superior al valor màxim ({user_features} > 10 polígons)", 'Number of features (polygons) is above the maximum value ({user_features} > 10 polygons)', 'El número de objetos (polígonos) está por encima del valor máximo ({user_features} > 10 polígonos)',
  # coordinates info
  'sidebar_h4_coords', '{input$lidar_var_sel |> translate_app(lang_declared)} a les coordenades: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}', '{input$lidar_var_sel |> translate_app(lang_declared)} at coordinates: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}', '{input$lidar_var_sel |> translate_app(lang_declared)} en las coordenadas: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}',
  'sidebar_p_rawraster', 'Valor per al ràster de 20x20m: {round(click_raster_values()$raw, 3)}', '20x20m raster value: {round(click_raster_values()$raw, 3)}', 'Valor para el ráster de 20x20m: {round(click_raster_values()$raw, 3)}',
  'sidebar_p_aggraster', 'Valor per al ràster de 400x400m: {round(click_raster_values()$agg, 3)}', '400x400m raster value: {round(click_raster_values()$agg, 3)}', 'Valor para el ráster de 400x400m: {round(click_raster_values()$agg, 3)}',
  # progress messages
  "poly_data_progress_mes", "Obtenint dades dels polígons...", "Retrieving polygon data...", "Obteniendo datos de los polígonos...",
  "poly_visible_progress_mes", "Preparant dades...", "Preparing data", "Preparando datos...",
  "raster_progress_mes", "S'està baixant ràster de baixa resolució...", "Downloading low res raster...", "Descargando ráster de baja resolución...",
  "dismiss", "Descartar", "Dismiss", "Cancelar",
  "not_enough_info_plot_warning", "No hi ha dades suficients per construir el gràfic de forma segura", "Not enough data to safely build the plot", "No hay datos suficientes para contruir la gráfica de manera segura",
  "map_progress_mes", "Dibuixant el mapa...", "Drawing the map...", "Dibujando el mapa...",
  # donwload buttons
  "download_trigger_btn", "Descarrega les dades dels polígons", "Download polygon data", "Descargar datos de los polígonos",
  "download_raster_trigger_btn", "Descarrega ràster de baixa resoluciò", "Download low res raster", "Descarga ráster de baja resolución",
  # sweet alert
  'sweet_alert_nopoly_title', "No s'ha dibuixat cap polígon", "No polygon has been drawn", "No se ha dibujado ningún polígono",
  'sweet_alert_nopoly_text', "Dibuixa un polígon amb l'eina de l'map i prova de nou", "Draw a polygon with the map tool and try again", "Dibuja un polígono con la herramienta del map y prueba de nuevo",
  # data info
  "h3_data_version_info", "Versió de les dades:", "Data version:", "Versión de los datos:",
  'p_data_version_info', "Les variables LiDAR han estat calculades amb les dades dels vols LiDAR més recents (2016) i les dades de la 4ª versió de l'Inventari Forestal Nacional (~2015)", "LiDAR variables had been calculated with the latest LiDAR flights for Catalonia (2016) and the 4th version of the National Forest Inventory data (~2015)", "Las variables LiDAR han sido calculadas con los datos de los vuelos LiDAR más recientes (2016) y los datos de la 4ª version del Inventario Forestal Nacional (~2015)",
  # poly_id_var_check
  "poly_id_missing_title", "No s'ha trobat cap variable anomenada 'poly_id' al fitxer", "Not 'poly_id' variable found in file", "No se ha encontrado ninguna variable llamada 'poly_id' en el archivo",
  "poly_id_missing_message", "S'ha fet servir la primera variable del fitxer com a poly_id", "First variable found in file used as poly_id", "Se ha usado la primera variable del archivo como poly_id"
  ## TODO continue translations thesaurus
) -> app_translations

usethis::use_data(app_translations, internal = TRUE, overwrite = TRUE)
