library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(spdep)
library(leaflet)
library(units)
library(leaflet.extras2)


#Descargo las capas que necesito

#1 - leo el catalogo

capas_disponibles <- read.csv("capas_disponibles.csv")

#	gis:Censo_2022_NBI_Escolaridad
# gis:Censo_2022_Educacion_por_grupo_quinquenal
# gis:Censo_2022_Indices_de_empleo
# gis:Censo_2022_NBI_Hacinamiento
# gis:Censo_2022_Hogares_con_saneamiento_cloacal
# gis:Censo_2022_Hogares_con_tenencia_de_agua
# gis:Censo_2022_Cobertura_de_salud
# gis:Censo_2022_Poblacion_en_grupos_quinquenales
# gis:Censo_2022_Tipos_de_vivienda
# gis:Censo_2022_Cantidad_de_habitantes_por_hogar
# gis:radios_censales

#Descargo las capas seleccionadas

options(timeout = 300)

obtener_capa <- function(capa_nombre, ruta = "datos_gis", nombre_archivo = NULL) {
  
  # 1. Crear directorio de destino si no existe
  if (!dir.exists(ruta)) {
    dir.create(ruta, recursive = TRUE)
  }
  
  # 2. Definir nombre del archivo .gpkg individual
  if (is.null(nombre_archivo)) {
    nombre_clean <- gsub("^[^:]+:", "", capa_nombre) # Quita "gis:"
    archivo_gpkg <- paste0(nombre_clean, ".gpkg")
  } else {
    archivo_gpkg <- if (!grepl("\\.gpkg$", nombre_archivo, ignore.case = TRUE)) {
      paste0(nombre_archivo, ".gpkg")
    } else {
      nombre_archivo
    }
  }
  
  archivo_path <- file.path(ruta, archivo_gpkg)
  
  # 3. Control de caché: Si el .gpkg individual ya existe, lo lee
  if (file.exists(archivo_path)) {
    cat("Cargando archivo local existente:", archivo_path, "\n")
    return(st_read(archivo_path, quiet = TRUE))
  }
  
  cat("Descargando a demanda:", capa_nombre, "...\n")
  capa_encoded <- utils::URLencode(capa_nombre, reserved = TRUE)
  
  # Petición HTTP WFS 1.1.0
  url_geojson <- paste0(
    "http://geo.gualeguaychu.gov.ar/geoserver/ows",
    "?service=WFS&version=1.1.0&request=GetFeature",
    "&typeName=", capa_encoded,
    "&outputFormat=application/json"
  )
  
  tmp_file <- tempfile(fileext = ".json")
  
  status <- tryCatch({
    suppressWarnings(
      download.file(url_geojson, destfile = tmp_file, mode = "wb", quiet = TRUE)
    )
  }, error = function(e) e)
  
  # Contingencia HTTPS WFS 2.0.0
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
    cat("[!] ERROR: La capa '", capa_nombre, "' no respondió a tiempo.\n", sep = "")
    return(NULL)
  }
  
  # Leer GeoJSON descargado
  datos <- tryCatch(st_read(tmp_file, quiet = TRUE), error = function(e) NULL)
  unlink(tmp_file)
  
  if (is.null(datos)) return(NULL)
  
  # Sanitizar columna 'fid'
  if ("fid" %in% names(datos)) names(datos)[names(datos) == "fid"] <- "id_wfs"
  if ("FID" %in% names(datos)) names(datos)[names(datos) == "FID"] <- "id_wfs"
  
  # 4. Guardar archivo .gpkg exclusivo
  st_write(datos, dsn = archivo_path, quiet = TRUE)
  cat(">>> OK: Archivo guardado individualmente en '", archivo_path, "'.\n", sep = "")
  
  return(datos)
}



# Descargo las capas
escolaridad <- obtener_capa("gis:Censo_2022_NBI_Escolaridad")
educacion_quinquenal <- obtener_capa("gis:Censo_2022_Educacion_por_grupo_quinquenal")
empleo <- obtener_capa("gis:Censo_2022_Indices_de_empleo")
hacinamiento <- obtener_capa("gis:Censo_2022_NBI_Hacinamiento")
cloacas  <- obtener_capa("gis:Censo_2022_Hogares_con_saneamiento_cloacal")
agua  <- obtener_capa("gis:Censo_2022_Hogares_con_tenencia_de_agua")
cober_salud  <- obtener_capa("gis:Censo_2022_Cobertura_de_salud")
poblacion_edad  <- obtener_capa("gis:Censo_2022_Poblacion_en_grupos_quinquenales")
tipo_vivienda  <- obtener_capa("gis:Censo_2022_Tipos_de_vivienda")
hab_por_hogar  <- obtener_capa("gis:Censo_2022_Cantidad_de_habitantes_por_hogar")
radios_censales  <- obtener_capa("gis:radios_censales")
vivienda_inconv <- obtener_capa("gis:Censo_2022_NBI_Vivienda_tipo_inconveniente")
ejido <- obtener_capa("gis:ejido")
planta_urbana <- obtener_capa("gis:planta_urbana")


#Cargo las capas

escolaridad <- st_read("datos_gis/Censo_2022_NBI_Escolaridad.gpkg")
empleo <- st_read("datos_gis/Censo_2022_Indices_de_empleo.gpkg")
cober_salud <- st_read("datos_gis/Censo_2022_Cobertura_de_salud.gpkg")
hacinamiento <- st_read("datos_gis/Censo_2022_NBI_Hacinamiento.gpkg")
planta_urbana <- st_read("datos_gis/planta_urbana.gpkg")
poblacion <- st_read("datos_gis/Censo_2022_Poblacion_en_grupos_quinquenales.gpkg")

#Descripcion de los totales pre-recorte

poblacion %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    
    # Limpieza de NAs en las columnas de grupos de edad (empiezan con X)
    across(starts_with("X"), ~ replace_na(as.numeric(.), 0)),
    
    # Suma total de población calculada desde las franjas quinquenales
    pob_total = rowSums(pick(starts_with("X"))),
  
  ) %>%
  summarise(pob_tot = sum(pob_total)) %>%
  bind_cols(poblacion %>%
              st_filter(planta_urbana, .predicate = st_intersects)%>%
              st_drop_geometry() %>%
              mutate(
                cod_indec = as.character(cod_indec),
                
                # Limpieza de NAs en las columnas de grupos de edad (empiezan con X)
                across(starts_with("X"), ~ replace_na(as.numeric(.), 0)),
                
                # Suma total de población calculada desde las franjas quinquenales
                pob_total = rowSums(pick(starts_with("X"))),
                
              ) %>%
              summarise(pob_urb = sum(pob_total))) %>%
  mutate(porc = pob_urb*100/pob_tot)



escolaridad %>%
  st_drop_geometry() %>%
  summarise(tot_hog = sum(Si)+sum(No)) %>%
  bind_cols(escolaridad %>%
              st_filter(planta_urbana, .predicate = st_intersects) %>%
              st_drop_geometry() %>%
              summarise(tot_hog_urb = sum(Si)+sum(No))) %>%
  mutate(porc = tot_hog_urb*100/tot_hog)

poblacion %>%
  mutate(
    sup_m2 = as.numeric(st_area(.)))%>%
  st_drop_geometry()%>%
  summarise(tot_area= sum(sup_m2)/1000000) %>%
  bind_cols(
    poblacion %>%
      st_filter(planta_urbana, .predicate = st_intersects) %>%
        mutate(
        sup_m2 = as.numeric(st_area(.)))%>%
      st_drop_geometry()%>%
      summarise(tot_area_urb= sum(sup_m2)/1000000)) %>%
  mutate(porc = tot_area_urb*100/tot_area)


# Mapa 1 para dimensionar el area urbana vs el ejido

# Armo un mapa de los radios urbanos

mapa_1 <- ggplot() +
  # 1. Ejido municipal completo con relleno gris claro neutro
  geom_sf(
    data = st_union(escolaridad),
    fill = "#f0f0f0",
    color = "#555555",
    linewidth = 0.5,
    aes(shape = "Ejido municipal"),
    show.legend = F
  ) +
  
  # 2. Planta urbana con color unificado y borde fino blanco
  geom_sf(
    data = planta_urbana,
    fill = "#2b5c8f",
    color = "white",
    linewidth = 0.05
  ) +

  # 5. Títulos y Fuentes (Norma Editorial)
  labs(
    fill= NULL,
    title = "Figura 1. Delimitación de la planta urbana dentro del ejido municipal de Gualeguaychú",
    subtitle = "Cobertura espacial de los 137 radios censales analizados (Censo 2022)",
    caption = "Fuente: Elaboración propia a partir de microdatos del Censo 2022 (INDEC) y la Infraestructura de Datos Espaciales (IDE) del\nMunicipio de Gualeguaychú."
    ) +
  
  # 6. Tema académico limpio
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 10)),
    axis.text = element_text(size = 7, color = "gray40"), # Muestra las coordenadas geográficas en los ejes
    axis.title = element_blank(),
    panel.grid = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ggsave(
#   filename = "Resultados/mapa_radios_censales_gchu_1_vs_ejido.png",
#   plot = mapa_1,
#   width = 8,        # Ancho en pulgadas
#   height = 6,       # Alto en pulgadas
#   units = "in",
#   dpi = 300,        # Resolución alta
#   bg = "white"      # Asegura fondo blanco en el PNG
# )


# 2. Creo función unificada de recorte urbano (Intersección + Área)
recortar_urbano <- function(capa, limite) {
  # Reproyectar el límite al CRS de la capa para evitar desacoples
  limite_crs <- st_transform(limite, st_crs(capa))
  
  # Filtrar espacialmente por intersección con la planta urbana
  capa %>%
    st_filter(limite_crs, .predicate = st_intersects)
}

# 3. Aplicar el filtro limpio a todas las capas iniciales
escolaridad  <- recortar_urbano(escolaridad, planta_urbana)
empleo       <- recortar_urbano(empleo, planta_urbana)
cober_salud  <- recortar_urbano(cober_salud, planta_urbana)
hacinamiento <- recortar_urbano(hacinamiento, planta_urbana)


geo_gchu <- "https://geo.gualeguaychu.gov.ar/geoserver/wms"



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
  setView(lng = -58.515, lat = -33.008, zoom = 12)
  
#Armo el dataframe

escolaridad_hogares_pct <- escolaridad %>%
  group_by(id_wfs, cod_indec) %>%
  summarise(
    pct_nbi_escolaridad = round((`Si` * 100) / (`Si` + `No`), 2),
    .groups = "drop"
  )

# Procesamiento de la dimensión Economía
dimension_economia <- empleo %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    # Convertir a numérico limpiando posibles "-" o espacios
    across(c(contains("Desocupad"), contains("PEA"), contains("Ocupad")), ~ {
      val <- as.character(.)
      val <- gsub("-", "0", val)
      val <- as.numeric(trimws(val))
      tidyr::replace_na(val, 0)
    })
  )


# Extracción limpia de la dimensión Economía
dimension_economia <- empleo %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    # Asegurar formato numérico por si existen caracteres extraños
    pct_desocupacion = as.numeric(gsub(",", ".", Tasa.de.desocupación))
  ) %>%
  select(cod_indec, pct_desocupacion)


dimension_hacinamiento <- hacinamiento %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    pct_hacinamiento = round((Si * 100) / (Si + No), 2)
  ) %>%
  select(cod_indec, pct_hacinamiento)


# Cálculo de la Dimensión Salud

dimension_salud <- cober_salud %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    # Asegurar conversión a numérico de las categorías
    os   = as.numeric(Obra.social.o.prepaga..incluye.PAMI.),
    prog = as.numeric(Programas.o.planes.estatales.de.salud),
    sin  = as.numeric(No.tiene.obra.social..prepaga.ni.plan.estatal),
    
    # Total de población evaluada en el radio
    pob_evaluada = os + prog + sin,
    
    # Porcentaje de población con cobertura pública exclusiva
    pct_sin_cobertura = round((sin * 100) / pob_evaluada, 2)
  ) %>%
 select(cod_indec, pct_sin_cobertura)

# Cargamos la capa de servicios sanitarios
cloacas <- st_read("datos_gis/Censo_2022_Hogares_con_saneamiento_cloacal.gpkg")

# 1. Cloacas (Infraestructura Sanitaria)
dim_cloacas <- cloacas %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    
    # Reemplazo de NAs por 0 en todas las opciones de saneamiento
    red   = replace_na(`A.red.pública..cloaca.`, 0),
    cam   = replace_na(`A.cámara.séptica.y.pozo.ciego`, 0),
    pozo  = replace_na(`Sólo.pozo.ciego`, 0),
    hoyo  = replace_na(`A.hoyo..excavación.en.la.tierra..etc.`, 0),
    
    # Total de hogares evaluados en el radio
    total_hogares = red + cam + pozo + hoyo,
    
    # Porcentaje de hogares sin red pública (cámara, pozo o latrina)
    pct_sin_cloaca = round(((cam + pozo + hoyo) * 100) / total_hogares, 2)
  ) %>%
  select(cod_indec, pct_sin_cloaca)

# 2. Agua

agua <- st_read("datos_gis/Censo_2022_Hogares_con_tenencia_de_agua.gpkg")
View(agua)

dim_agua <- agua %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    
    # Imputación de NAs a 0 en las opciones de provisión
    dentro     = replace_na(`Por.cañería.dentro.de.la.vivienda`, 0),
    fuera_viv  = replace_na(`Fuera.de.la.vivienda..pero.dentro.del.terreno`, 0),
    fuera_terr = replace_na(`Fuera.del.terreno`, 0),
    
    # Total de viviendas/hogares evaluados
    total_agua = dentro + fuera_viv + fuera_terr,
    
    # Porcentaje con acceso deficitario de agua
    pct_deficit_agua = round(((fuera_viv + fuera_terr) * 100) / total_agua, 2)
  ) %>%
  select(cod_indec, pct_deficit_agua)

# 3.Edad

poblacion <- st_read("datos_gis/Censo_2022_Poblacion_en_grupos_quinquenales.gpkg")
View(poblacion)

dim_edad <- poblacion %>%
  st_drop_geometry() %>%
  mutate(
    cod_indec = as.character(cod_indec),
    
    # Limpieza de NAs en las columnas de grupos de edad (empiezan con X)
    across(starts_with("X"), ~ replace_na(as.numeric(.), 0)),
    
    # Suma total de población calculada desde las franjas quinquenales
    pob_total = rowSums(pick(starts_with("X"))),
    
    # Porcentaje de primera infancia (0 a 4 años)
    pct_infancia = round((X00.A.04 * 100) / pob_total, 2)
  ) %>%
  select(cod_indec, pct_infancia)






# ==============================================================================
# 1. UNIFICACIÓN TABULAR (LEFT JOIN)
# ==============================================================================
escolaridad_hogares_pct <- escolaridad_hogares_pct %>%
  mutate(cod_indec = as.character(cod_indec))

ivs_tabla <- escolaridad_hogares_pct %>%
  left_join(dimension_economia, by = "cod_indec") %>%
  left_join(dimension_hacinamiento, by = "cod_indec") %>%
  left_join(dimension_salud, by = "cod_indec") %>%
  left_join(dim_cloacas, by = "cod_indec") %>%
  left_join(dim_agua, by = "cod_indec") %>%
  left_join(dim_edad, by = "cod_indec")

# Control de consistencia
cat("Total de radios consolidados:", nrow(ivs_tabla), "\n")
cat("Chequeo de NAs por columna:\n")
print(colSums(is.na(ivs_tabla)))

# ==============================================================================
# 2. ESTANDARIZACIÓN POR PUNTAJE Z (METODOLOGÍA BUZAI)
# ==============================================================================
# Se transforma cada variable original X_i a su valor estandarizado Z = (X - mean) / sd
# Como scale() retorna una matriz de 1 columna, extraemos el vector con [,1]

ivs_tabla <- ivs_tabla %>%
  mutate(
    z_educacion   = scale(pct_nbi_escolaridad)[, 1],
    z_empleo      = scale(pct_desocupacion)[, 1],
    z_hacinamiento = scale(pct_hacinamiento)[, 1],
    z_salud       = scale(pct_sin_cobertura)[, 1],
    z_cloacas     = scale(pct_sin_cloaca)[, 1],
    z_agua        = scale(pct_deficit_agua)[, 1],
    z_infancia    = scale(pct_infancia)[, 1]
  )

# ==============================================================================
# 3. CONSTRUCCIÓN DEL ÍNDICE SINTÉTICO (IVS)
# ==============================================================================
# Promediamos el eje ambiental (agua + cloaca) para no sobreponderar infraestructura
# y combinamos con las otras 5 dimensiones estructurales.

ivs_tabla <- ivs_tabla %>%
  mutate(
    z_infraestructura = rowMeans(pick(z_cloacas, z_agua), na.rm = TRUE),
    IVS = rowMeans(
      pick(z_educacion, z_empleo, z_hacinamiento, z_salud, z_infraestructura, z_infancia), 
      na.rm = TRUE
    )
  )


names(ivs_tabla)
# Agrupamos los puntajes z de las dimensiones

obtener_rangos_puntaje_z <- function(x) {
  cut(
    x,
    breaks = c(-Inf, -1.0, -0.5, 0.5, 1.0, Inf),
    labels = c(
      "1. Muy Bajo (< -1.0)",
      "2. Bajo (-1.0 a -0.5)",
      "3. Medio (-0.5 a 0.5)",
      "4. Alto (0.5 a 1.0)",
      "5. Muy Alto (> 1.0)"
    ),
    include.lowest = TRUE
  )
}

ivs_tabla <- ivs_tabla %>%
  mutate(
    across(
      # 1. Columnas estandarizadas a clasificar
      c(z_educacion, z_empleo, z_hacinamiento, z_salud, z_infancia, z_infraestructura, IVS), 
      
      # 2. Aplicación de categorías teóricas de Puntaje Z (Buzai)
      obtener_rangos_puntaje_z, 
      
      # 3. Sufijo indicativo de categoría/nivel
      .names = "{.col}_cat" 
    )
  )

ivs_tabla$IVS

# Armamos cartografía del Indice de vulnerabilidad social

library(ggplot2)
library(sf)

mapa_2 <- ggplot() +
  # 1. Capa de radios urbanos según categorías de IVS
  geom_sf(
    data = ivs_tabla,
    aes(fill = IVS_cat),
    color = "white",       # Límite blanco fino para separar radios
    linewidth = 0.08
  ) +
  
  # 2. Paleta divergente ajustada para Puntajes Z (Invertida)
  scale_fill_brewer(
    palette = "RdYlBu",
    direction = -1,        # Invierte la paleta: Azul = Muy Bajo | Rojo = Muy Alto
    na.value = "transparent",
    drop = FALSE
  ) +
  
  # 3. Títulos y Leyenda adaptados al tema
  labs(
    fill = NULL,           # Elimina el título genérico de la leyenda
    title = "Figura 2. Distribución espacial del Índice de Vulnerabilidad Social (IVS)",
    subtitle = "Categorización por intervalos fijos de Puntaje Z en la planta urbana de Gualeguaychú (N = 137)",
    caption = "Fuente: Elaboración propia a partir de microdatos del Censo 2022 (INDEC) y la Infraestructura de Datos Espaciales (IDE) del\nMunicipio de Gualeguaychú."
  ) +
  
  # 4. Tema académico
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5),
    axis.text = element_text(size = 7, color = "gray40"),
    axis.title = element_blank(),
    panel.grid = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2)
  ) +
  
  # 5. Guía de la leyenda distribuida horizontalmente en 1 fila
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


ggsave(
  filename = "Resultados/mapa_ivs_pje_z.png",
  plot = mapa_2,
  width = 8,        # Ancho en pulgadas
  height = 6,       # Alto en pulgadas
  units = "in",
  dpi = 300,        # Resolución alta
  bg = "white"      # Asegura fondo blanco en el PNG
)

ivs_tabla %>%
  group_by(IVS_cat) %>%
  summarise(n= n_distinct(cod_indec))


# 1. Transformar CRS a WGS84 (EPSG:4326) para compatibilidad con Leaflet
ivs_leaflet <- ivs_tabla %>% 
  st_transform(crs = 4326)

# 2. Definir la paleta categórica de 5 niveles con los colores de RdYlBu invertidos
pal_ivs <- colorFactor(
  palette = c("#2B83BA", "#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C"),
  domain = ivs_leaflet$IVS_cat,
  ordered = TRUE
)

# 3. Armar el mapa interactivo
leaflet(ivs_leaflet) %>%
  # 1. Base Map: Argenmap Gris (IGN) - Ideal para visualizar datos
  addTiles(
    urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
    options = tileOptions(tms = TRUE, maxZoom = 18),
    attribution = '<a target="_blank" href="https://www.ign.gob.ar">IGN Argentina</a>',
    group = "Argenmap Gris"
  ) %>%
  
  # 2. Base Map: OpenStreetMap estándar - Ideal para leer nombres de calles
  addTiles(
    group = "Calles (OSM)"
  ) %>%
  
  # Capa de Polígonos de IVS (tu código intacto)
  addPolygons(
    fillColor = ~pal_ivs(IVS_cat),
    fillOpacity = 0.7,
    weight = 0.8,
    color = "white",
    dashArray = "1",
    highlightOptions = highlightOptions(
      weight = 2, color = "#000000", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE
    ),
    label = ~paste0("Radio Censal: ", cod_indec),
    popup = ~paste0(
      "<strong>Radio Censal:</strong> ", cod_indec, "<br>",
      "<strong>Categoría IVS:</strong> ", IVS_cat, "<br>",
      "<strong>Puntaje Z IVS:</strong> ", round(IVS, 2)
    ),
    group = "IVS por Radio Censal"
  ) %>%
  
  # 3. Control para alternar entre el mapa gris y el de calles
  addLayersControl(
    baseGroups = c("Argenmap Gris", "Calles (OSM)"),
    overlayGroups = c("IVS por Radio Censal"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(pal = pal_ivs, values = ~IVS_cat, title = "Nivel de IVS (Puntajes Z)", position = "bottomright", opacity = 0.8) %>%
  setView(lng = -58.515, lat = -33.008, zoom = 12)

##%######################################################%##
#                                                          #
####          Mapeo de las dimensiones del IVS          ####
#                                                          #
##%######################################################%##

ivs_dimensiones <- ivs_tabla %>%
  # Seleccionamos las columnas de identificación y las dimensiones categóricas
  select(cod_indec, ends_with("_cat"), -matches("^(ivs|IVS)_cat")) %>%
  
  # Llevamos las dimensiones a formato largo
  pivot_longer(
    cols = ends_with("_cat"),
    names_to = "dimension",
    values_to = "categoria"
  ) %>%
  
  # Emprolijamos los nombres de las dimensiones para los títulos del facet
  mutate(
    dimension = stringr::str_remove(dimension, "^z_"),       # Quita el prefijo 'z_' si lo tiene
    dimension = stringr::str_remove(dimension, "_cat$"),      # Quita el sufijo '_cat'
    dimension = stringr::str_to_title(dimension)              # Pone la primera letra en mayúscula
  )


mapa_3 <- ggplot(ivs_dimensiones) +
  geom_sf(aes(fill = categoria), color = "white", linewidth = 0.05) +
  scale_fill_brewer(
    palette = "RdYlBu",
    direction = -1,
    drop = FALSE
  ) +
  facet_wrap(~ dimension, ncol = 2) + # Organiza los mapas en columnas
  labs(
    fill = NULL,
    title = "Figura 3. Descomposición del IVS por dimensiones socioespaciales",
    subtitle = "Categorización por Puntajes Z según dimensión analizada en la planta urbana",
    caption = "Fuente: Elaboración propia a partir de datos del Censo 2022 (INDEC)."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9), # Títulos de cada mapa del facet
    axis.text = element_blank(),                       # Quita coordenadas para limpiar los mapas pequeños
    panel.grid = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))


ggsave(
  filename = "Resultados/mapa_dimensiones_pje_z.png",
  plot = mapa_3,
  width = 8,        # Ancho en pulgadas
  height = 6,       # Alto en pulgadas
  units = "in",
  dpi = 300,        # Resolución alta
  bg = "white"      # Asegura fondo blanco en el PNG
)

##%######################################################%##
#                                                          #
####  Indice de concentracion espacial global y local   ####
#                                                          #
##%######################################################%##



# 1. Cálculo de superficie y proporción espacial (s_i)
mapa_ice <- ivs_tabla %>%
  mutate(
    sup_m2 = as.numeric(st_area(.)),
    s_i = (sup_m2 / sum(sup_m2, na.rm = TRUE)) * 100,
    pct_infra_sintesis = (pct_sin_cloaca + pct_deficit_agua) / 2
  )

# 2. Función para procesar ICEG, ICEA y la curva por dimensión
procesar_ice_dimension <- function(df_sf, col_conteo, nombre_dim) {
  df_sf %>%
    st_drop_geometry() %>%
    filter(!is.na(.data[[col_conteo]]), !is.na(s_i)) %>%
    mutate(
      a_i = (.data[[col_conteo]] / sum(.data[[col_conteo]], na.rm = TRUE)) * 100,
      icea = a_i / s_i,
      diff_pos = ifelse(a_i > s_i, a_i - s_i, 0)
    ) %>%
    arrange(desc(icea)) %>%
    mutate(
      s_acum = cumsum(s_i),
      a_acum = cumsum(a_i),
      dimension = nombre_dim,
      iceg_val = round(sum(diff_pos), 2)
    )
}

# 3. Mapeo de dimensiones (Ajustá los valores en rojo a los nombres de tus columnas absolutas o porcentuales base, NO a los puntajes Z)
variables_conteo <- list(
  "Educación"       = "pct_nbi_escolaridad", 
  "Empleo"          = "pct_desocupacion",
  "Hacinamiento"    = "pct_hacinamiento",
  "Infancia"        = "pct_infancia",
  "Infraestructura" = "pct_infra_sintesis",
  "Salud"           = "pct_sin_cobertura"
 
)


# 4. Consolidación de datos de Lorenz
tabla_lorenz_global <- imap_dfr(variables_conteo, ~ {
  procesar_ice_dimension(mapa_ice, .x, .y)
})

# Etiquetas formateadas con el valor ICEG para cada panel
etiquetas_iceg <- tabla_lorenz_global %>%
  group_by(dimension) %>%
  summarise(label = paste0("ICEG: ", unique(iceg_val)))

# 5. Gráfico multi-panel estandarizado
figura_4 <- ggplot(tabla_lorenz_global, aes(x = s_acum, y = a_acum)) +
  # Área de concentración (sombreado)
  geom_ribbon(aes(ymin = s_acum, ymax = a_acum), fill = "#d73027", alpha = 0.2) + # Tono rojizo tenue acorde a la paleta RdYlBu
  # Curva de Lorenz espacial
  geom_line(color = "#d73027", linewidth = 0.8) +
  # Línea de equidistribución
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # Etiqueta del ICEG reubicada
  geom_text(data = etiquetas_iceg, aes(x = 35, y = 85, label = label), 
            fontface = "bold", size = 3.5, color = "gray20") +
  
  facet_wrap(~ dimension, ncol = 3) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Figura 4. Curvas de concentración espacial (Lorenz) por dimensiones",
    subtitle = "Distribución territorial acumulada de la vulnerabilidad socioespacial (N = 137)",
    x = "Superficie Acumulada (%)",
    y = "Población/Dimensión Acumulada (%)",
    caption = "Fuente: Elaboración propia a partir de datos del Censo 2022 (INDEC)."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15)),
    strip.text = element_text(face = "bold", size = 9.5), # Títulos de cada panel limpios, sin fondo
    axis.title = element_text(size = 9, color = "gray20"),
    axis.text = element_text(size = 8, color = "gray50"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5) # Borde sutil para contener cada gráfico
  )

ggsave(
  filename = "Resultados/figura4_curvas_lorenz.png", # Nombre corregido
  plot = figura_4,                             # Asegurate de llamar al objeto ggplot de Lorenz
  width = 10,       # Un poco más ancho para acomodar las 3 columnas
  height = 6,       # Alto estándar
  units = "in",
  dpi = 300,        # Calidad óptima para revista científica
  bg = "white"      # Fundamental para que theme_minimal() no quede transparente
)


library(dplyr)
library(flextable)

# Cálculo de la concentración acumulada al 20% y 50% de superficie para Gualeguaychú
cuadro2_gchu <- tabla_lorenz_global %>%
  group_by(dimension) %>%
  summarise(
    `20% Superficie` = paste0(round(approx(s_acum, a_acum, xout = 20)$y, 2), "%"),
    `50% Superficie` = paste0(round(approx(s_acum, a_acum, xout = 50)$y, 2), "%")
  ) %>%
  rename(`Dimensión` = dimension) # Pone la mayúscula y el tilde para la tabla final

print(cuadro2_gchu)


# 2. Creación y formateo de la tabla académica
tabla_academica <- flextable(cuadro2_gchu) %>%
  theme_booktabs() %>% # Aplica el estilo académico clásico (solo líneas horizontales)
  set_caption(caption = "Tabla 1. Concentración espacial acumulada por dimensión al 20% y 50% de la superficie urbana. Gualeguaychú, Censo 2022.") %>%
  align(j = 2:3, align = "center", part = "all") %>% # Centra los porcentajes
  bold(part = "header") %>% # Pone los títulos de columna en negrita
  font(fontname = "Arial", part = "all") %>% # Usa Arial para que coincida con Google Docs
  fontsize(size = 11, part = "all") %>%
  autofit() # Ajusta automáticamente el ancho de las columnas al texto


#Correlacion espacial



# 1. Matriz de Vecindad (Criterio Reina / Queen)
vecinos <- poly2nb(ivs_tabla, queen = TRUE)
weights <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# 2. Test de Moran Global
moran_global <- moran.test(ivs_tabla$IVS, weights, zero.policy = TRUE)
print(moran_global)

# 3. Cálculo de LISA Local
local_m <- localmoran(ivs_tabla$IVS, weights, zero.policy = TRUE)

# 4. Construcción del Cuadrante LISA y Recategorización
ivs_lisa <- ivs_tabla %>%
  mutate(
    z_ivs = as.numeric(scale(IVS)),
    lag_z_ivs = as.numeric(lag.listw(weights, z_ivs, zero.policy = TRUE)),
    p_val = local_m[, 5], # Columna Pr(z != E(Ii))
    cluster = case_when(
      p_val >= 0.05 ~ "No significativo",
      z_ivs > 0 & lag_z_ivs > 0 ~ "Alto-Alto",
      z_ivs < 0 & lag_z_ivs < 0 ~ "Bajo-Bajo",
      z_ivs > 0 & lag_z_ivs < 0 ~ "Alto-Bajo",
      z_ivs < 0 & lag_z_ivs > 0 ~ "Bajo-Alto"
    ),
    cluster = factor(
      cluster, 
      levels = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo")
    )
  )

# Resumen de frecuencias por conglomerado
table(ivs_lisa$cluster)

ivs_lisa_4326 <- st_transform(ivs_lisa, 4326)

centroides_proyectados <- st_point_on_surface(ivs_lisa)
centroides_sf <- st_transform(centroides_proyectados, 4326)

lineas_vecindad <- nb2lines(vecinos, coords = st_coordinates(centroides_sf), as_sf = TRUE)
lineas_vecindad <- st_set_crs(lineas_vecindad, 4326)

# 5. Paletas cromáticas
pal_lisa <- leaflet::colorFactor(
  palette = c(
    "Alto-Alto"        = "#d7191c",
    "Bajo-Bajo"        = "#2b83ba",
    "Alto-Bajo"        = "#fdae61",
    "Bajo-Alto"        = "#abdda4",
    "No significativo" = "#f0f0f0"
  ),
  domain = ivs_lisa_4326$cluster
)

pal_quintil <- leaflet::colorFactor(palette = "YlOrRd", domain = NULL)

# 6. Identificación de capas de quintiles para el mapa
columnas_q <- names(ivs_lisa_4326)[grep("_quintil$", names(ivs_lisa_4326))]

# 7. Ensamblado del Mapa Interactivo
mapa_completo <- leaflet() %>%
  addTiles(
    urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
    options = tileOptions(tms = TRUE, maxZoom = 18),
    group = "Argenmap Base"
  ) %>%
  
  # Capa Polígonos LISA
  addPolygons(
    data = ivs_lisa_4326,
    fillColor = ~pal_lisa(cluster),
    fillOpacity = 0.6,
    color = "#444444",
    weight = 1,
    popup = ~paste0("<b>Radio: </b>", cod_indec, "<br><b>Clúster LISA: </b>", cluster),
    group = "Clusters LISA"
  ) %>% addPolylines(
    data = lineas_vecindad,
    color = "#333333",
    weight = 1.2,
    opacity = 0.5,
    dashArray = "3,3",
    group = "Red de Vecindad (W)"
  ) %>%
  addCircleMarkers(
    data = centroides_sf,
    radius = 4,
    color = "#000000",
    fillColor = "#ffffff",
    fillOpacity = 1,
    weight = 1.5,
    popup = ~paste0("<b>Centroide Radio: </b>", cod_indec),
    group = "Nodos (Centroides)"
  ) %>%
  addLayersControl(
    baseGroups = c("Argenmap Base"),
    overlayGroups = c("Clusters LISA", "Red de Vecindad (W)", "Nodos (Centroides)"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  setView(lng = -58.515, lat = -33.008, zoom = 12)

mapa_completo


names(ivs_tabla)

# Armo los mapas para las dimensiones

dimensiones <- c("z_educacion_cat","z_empleo_cat","z_hacinamiento_cat","z_salud_cat",
                 "z_infancia_cat" ,"z_infraestructura_cat","IVS_cat")

names <- c("Educacion","Empleo","Hacinamiento","Salud","Infancia","Infraestructura","IVS")

mapas <- list()


for (d in 1:length(dimensiones)){
print(d)
  
mapas[[d]] <- ggplot(ivs_tabla) +
  geom_sf(aes(fill = .data[[dimensiones[d]]]), color = "white", linewidth = 0.08) +
  scale_fill_brewer(
    palette = "YlOrRd", # Paleta ColorBrewer: Amarillo (Q1) -> Naranja -> Rojo (Q5)
    direction = 1,
    name = names[d],
    drop = FALSE
  ) +
  #labs(title = titulo) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )
}

mapas[[7]]
