library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(leaflet.extras2)
library(osrm)



# 1. Definir la URL base de WMS
geo_gchu <- "https://geo.gualeguaychu.gov.ar/geoserver/wms"

# 2. Mapa Leaflet con Argenmap y capas WMS de Gualeguaychú
leaflet() %>%
  # Base Map: Argenmap Gris
  addTiles(
    urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
    options = tileOptions(tms = TRUE, maxZoom = 18),
    attribution = '<a target="_blank" href="https://www.ign.gob.ar">IGN Argentina</a> // OpenStreetMap',
    group = "Argenmap"
  ) %>%
  
  # Capa WMS 1: NBI Censo 2022
  addWMS(
    baseUrl = geo_gchu,
    layers = "gis:Censo_2022_NBI",
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE, 
      info_format = "text/html"
    ),
    group = "Censo NBI 2022"
  ) %>%
  
  # Capa WMS 2: Establecimientos de Salud
  addWMS(
    baseUrl = geo_gchu,
    layers = "gis:salud",
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE, 
      info_format = "text/html"
    ),
    group = "Salud"
  ) %>%
  
  # Capa WMS 3: Centros de Salud (CAPS)
  addWMS(
    baseUrl = geo_gchu,
    layers = "geonode:caps",
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE, 
      info_format = "text/html"
    ),
    group = "CAPS"
  ) %>%
  
  # Capa WMS 4: Barrios
  addWMS(
    baseUrl = geo_gchu,
    layers = "gis:barrios_populares",
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE, 
      info_format = "text/html"
    ),
    group = "Barrios"
  ) %>%
  
  # Control de capas interactivo
  addLayersControl(
    baseGroups = c("Argenmap"),
    overlayGroups = c("Censo NBI 2022", "Salud", "CAPS","Barrios"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Centrar en Gualeguaychú
  setView(lng = -58.515, lat = -33.008, zoom = 12)



