library(sf)
library(dplyr)
library(leaflet)

catalogo <- read.csv("capas_disponibles.csv")

# Ver el listado exacto de capas del censo
catalogo[grep("censo", catalogo$capa, ignore.case = TRUE), "capa"]

# ============================================================
# 2. DESCARGAR CAPA BASE Y FILTRAR "EL ESPINILLO"
# ============================================================


# Extender tiempo de espera para descargas de R


# Aumentar tiempo límite a 300 segundos (5 min) para capas censales grandes
options(timeout = 300)

obtener_capa <- function(capa_nombre, gpkg = "gualeguaychu_salud.gpkg") {
  layer_name <- gsub(":", "_", capa_nombre, fixed = TRUE)
  
  # 1. Si ya existe en el GeoPackage local, la lee directo del disco
  if (file.exists(gpkg) && layer_name %in% st_layers(gpkg)$name) {
    cat("Cargando desde GeoPackage local:", layer_name, "\n")
    return(st_read(gpkg, layer = layer_name, quiet = TRUE))
  }
  
  cat("Descargando a demanda:", capa_nombre, "...\n")
  
  capa_encoded <- utils::URLencode(capa_nombre, reserved = TRUE)
  
  # Petición HTTP WFS 1.1.0 (más rápida para capas censales complejas)
  url_geojson <- paste0(
    "http://geo.gualeguaychu.gov.ar/geoserver/ows",
    "?service=WFS&version=1.1.0&request=GetFeature",
    "&typeName=", capa_encoded,
    "&outputFormat=application/json"
  )
  
  tmp_file <- tempfile(fileext = ".json")
  
  # Ocultar advertencias intermedias si hay reintentos
  status <- tryCatch({
    suppressWarnings(
      download.file(url_geojson, destfile = tmp_file, mode = "wb", quiet = TRUE)
    )
  }, error = function(e) e)
  
  # Contingencia en HTTPS si falla HTTP
  if (inherits(status, "error") || !file.exists(tmp_file) || file.info(tmp_file)$size == 0) {
    url_geojson_alt <- paste0(
      "https://geo.gualeguaychu.gov.ar/geoserver/ows",
      "?service=WFS&version=2.0.0&request=GetFeature",
      "&typeNames=", capa_encoded,
      "&outputFormat=application/json"
    )
    status <- tryCatch({
      suppressWarnings(
        download.file(url_geojson_alt, destfile = tmp_file, mode = "wb", quiet = TRUE)
      )
    }, error = function(e) e)
  }
  
  if (inherits(status, "error") || !file.exists(tmp_file) || file.info(tmp_file)$size == 0) {
    cat("[!] ERROR: La capa '", capa_nombre, "' no respondió a tiempo. Reintenta más tarde.\n", sep = "")
    return(NULL)
  }
  
  # Leer GeoJSON descargado
  datos <- tryCatch(st_read(tmp_file, quiet = TRUE), error = function(e) NULL)
  unlink(tmp_file)
  
  if (is.null(datos)) return(NULL)
  
  # Sanitizar columna 'fid'
  if ("fid" %in% names(datos)) names(datos)[names(datos) == "fid"] <- "id_wfs"
  if ("FID" %in% names(datos)) names(datos)[names(datos) == "FID"] <- "id_wfs"
  
  # Guardar en el GeoPackage local
  st_write(datos, dsn = gpkg, layer = layer_name, append = FALSE, quiet = TRUE)
  cat(">>> OK: Capa '", layer_name, "' guardada con éxito en el GeoPackage.\n", sep = "")
  
  return(datos)
}


# Descargo las capas
censo_nbi <- obtener_capa("gis:Censo_2022_NBI")
hogares_raw <- obtener_capa("gis:Censo_2022_Cantidad_de_habitantes_por_hogar")


# seteo el codigo indec del barrio

radio_target <- "300562404"

#espinllos_nbi <-  censo_nbi %>%  filter(as.character(cod_indec) == radio_target))

espinillo_censo <- hogares_raw %>%
  filter(as.character(cod_indec) == radio_target) %>%
  mutate(across(starts_with("X"), ~ as.numeric(as.character(.)))) %>%
  rowwise() %>%
  mutate(
    viviendas_totales   = Viviendas.Particulares,
    total_hogares       = sum(c_across(starts_with("X")), na.rm = TRUE),
    hogares_numerosos   = sum(c_across(any_of(paste0("X", c(5:16, 22)))), na.rm = TRUE),
    poblacion_censo_est = sum(c_across(starts_with("X")) * as.numeric(gsub("X", "", names(c_across(starts_with("X"))))), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Cálculos territoriales en POSGAR 98
    superficie_m2         = as.numeric(st_area(geom)),
    superficie_ha         = round(superficie_m2 / 10000, 2),
    viviendas_por_ha      = round(viviendas_totales / superficie_ha, 1),
    poblacion_por_ha      = round(poblacion_censo_est / superficie_ha, 1),
    pct_hogares_numerosos = round((hogares_numerosos / total_hogares) * 100, 1),
    pct_cohabitacion      = round(((total_hogares - viviendas_totales) / viviendas_totales) * 100, 1)
  )

# ============================================================
# PASO 2: MATRIZ EXTENSIBLE DE DATOS DE CAMPO Y SALUD
# (Aquí agregas/modificas todas las variables que quieras)
# ============================================================

datos_salud_campo <- data.frame(
  radio_id         = "300562404",
  pct_nbi          = 45.2,
  pct_hacinamiento = 31.8,
  pct_sin_cloaca   = 88.0,
  pct_sin_agua     = 62.5,
  menores_5_anos   = 165,
  caps_referencia  = "CAPS Suburbio Sur",
  relevamiento_fecha = "2026-08", # Ejemplo de trazabilidad de campo
  stringsAsFactors = FALSE
)

# ============================================================
# PASO 3: UNIÓN (LEFT JOIN) Y GUARDADO EN GEOPACKAGE
# ============================================================

# Convertir código a texto para garantizar el join
espinillo_censo$cod_indec <- as.character(espinillo_censo$cod_indec)

# Enriquecer polígono con la matriz de salud
espinillo_enriquecido <- espinillo_censo %>%
  left_join(datos_salud_campo, by = c("cod_indec" = "radio_id"))

# Sanitizar columna fid y reproyectar a WGS84 para Leaflet/Web
if ("fid" %in% names(espinillo_enriquecido)) names(espinillo_enriquecido)[names(espinillo_enriquecido) == "fid"] <- "id_wfs"

espinillo_wgs84 <- st_transform(espinillo_enriquecido, crs = 4326)

# ============================================================
# PASO 4: MAPA LEAFLET INTEGRADO CON POPUP COMPLETO
# ============================================================

# Construcción dinámica del popup agrupando Censo + Campo
popup <- paste0(
  "<div style='font-family: Arial, sans-serif; font-size: 13px;'>",
  "<h4 style='margin:0; color:#990000;'>Barrio Popular: El Espinillo</h4>",
  "<b>Radio Censal:</b> ", espinillo_wgs84$cod_indec, "<br>",
  "<b>Centro de Salud Base:</b> ", espinillo_wgs84$caps_referencia, "<br><hr style='margin:5px 0;'>",
  
  "<b style='color:#2b5c8f;'>DETERMINANTES TERRITORIALES (Censo 2022)</b><br>",
  "• <b>Superficie:</b> ", espinillo_wgs84$superficie_ha, " ha<br>",
  "• <b>Población censal est.:</b> ", espinillo_wgs84$poblacion_censo_est, " hab. (", espinillo_wgs84$poblacion_por_ha, " hab/ha)<br>",
  "• <b>Viviendas / Hogares:</b> ", espinillo_wgs84$viviendas_totales, " viv. / ", espinillo_wgs84$total_hogares, " hog.<br>",
  "• <b>Hogares numerosos (5+ hab):</b> ", espinillo_wgs84$pct_hogares_numerosos, "%<br>",
  "• <b>Índice de Cohabitación:</b> ", espinillo_wgs84$pct_cohabitacion, "%<br><hr style='margin:5px 0;'>",
  
  "<b style='color:#d95f02;'>PERFIL SOCIOSANITARIO / CAMPO</b><br>",
  "• <b>Hogares NBI:</b> ", espinillo_wgs84$pct_nbi, "%<br>",
  "• <b>Hacinamiento Crítico:</b> ", espinillo_wgs84$pct_hacinamiento, "%<br>",
  "• <b>Sin Red de Cloacas:</b> ", espinillo_wgs84$pct_sin_cloaca, "%<br>",
  "• <b>Sin Agua de Red:</b> ", espinillo_wgs84$pct_sin_agua, "%<br>",
  "• <b>Niños < 5 años:</b> ", espinillo_wgs84$menores_5_anos, "<br>",
  "</div>"
)

library(leaflet)

leaflet(espinillo_wgs84) %>%
  addTiles(
    urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
    options = tileOptions(tms = TRUE, maxZoom = 18),
    attribution = "IGN Argentina"
  ) %>%
  addPolygons(
    fillColor = "#d95f02",
    fillOpacity = 0.5,
    color = "#990000",
    weight = 2,
    highlightOptions = highlightOptions(weight = 4, color = "#000", fillOpacity = 0.7),
    popup = popup
  ) %>%
  setView(lng = -58.508, lat = -33.010, zoom = 15)
