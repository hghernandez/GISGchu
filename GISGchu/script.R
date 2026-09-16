library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(leaflet.extras2)
library(osrm)



library(sf)

# Probar la ruta alternativa WFS
geo_wfs_alt <- "WFS:https://geo.gualeguaychu.gov.ar/geoserver/gis/wfs"

capas <- st_layers(geo_wfs_alt)
head(capas$name, 10)


#Mapeamos algunas capas

leaflet() %>%
  addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
           tileOptions(tms = TRUE,maxZoom = 14), attribution = '<a target="_blank" href="https://www.ign.gob.ar/argenmap/argenmap.jquery/docs/#datosvectoriales" style="color: black; text-decoration: underline; font-weight: normal;">Datos IGN Argentina // OpenStreetMap</a>',
           group = "Argenmap") %>% #Aquí agregamos Argenmap
  addWMS(
    geo_gchu,
    layers = "radios_censales",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,info_format = "text/html", tiled=FALSE),
    group = "Radios Censales"
  ) %>% #Agregamos layer de Radios Censales
  addWMS(
    geo_gchu,
    layers = "areas_programaticas",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,info_format = "text/html", tiled=FALSE),
    group = "Areas Programáticas"
  )  %>% #Agregamos layer de Areas Programáticas
  addWMS(
    geo_gchu,
    layers = "est_salud",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,info_format = "text/html", tiled=FALSE),
    group = "Est de salud"
  ) %>% #Agregamos layer de Establecimientos de Salud
  addLayersControl(
    baseGroups = "Argenmap",
    overlayGroups = c("Areas Programáticas","Est de salud","Radios Censales"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -58.52597, lat = -33.00606,zoom = 11)



