library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(spdep)
library(leaflet)
#library(tmap)


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


#Cargo las capas

escolaridad <- st_read("datos_gis/Censo_2022_NBI_Escolaridad.gpkg")
empleo <- st_read("datos_gis/Censo_2022_Indices_de_empleo.gpkg")
cober_salud <- st_read("datos_gis/Censo_2022_Cobertura_de_salud.gpkg")
hacinamiento <- st_read("datos_gis/Censo_2022_NBI_Hacinamiento.gpkg")
ejido <- st_read("datos_gis/ejido.gpkg")


# 2. Creo función unificada de recorte urbano (Intersección + Área)
recortar_urbano <- function(capa, limite) {
  capa %>%
    st_filter(limite, .predicate = st_intersects) %>%
    mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
    filter(area_km2 < 3) # Descarta los polígonos rurales extensos
}

# 3. Aplicar el filtro limpio a todas las capas iniciales
escolaridad  <- recortar_urbano(escolaridad, ejido)
empleo       <- recortar_urbano(empleo, ejido)
cober_salud  <- recortar_urbano(cober_salud, ejido)
hacinamiento <- recortar_urbano(hacinamiento, ejido)

plot(st_geometry(ejido), border = "red", lwd = 2)
plot(st_geometry(escolaridad), add = TRUE, col = "lightblue")

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
# Calculamos los quintiles de los puntajes z de las dimensiones

obtener_rangos_cuantil <- function(x) {
  # 1. Calcular percentiles de quintil
  cortes <- quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
  
  # 2. Deduplicar cortes PRIMERO
  cortes_unicos <- unique(cortes)
  
  # Control de seguridad si la variable casi no tiene variabilidad
  if (length(cortes_unicos) < 2) {
    return(as.factor(rep("Sin variación", length(x))))
  }
  
  # 3. Redondear cortes únicos
  cortes_redondos <- round(cortes_unicos, 2)
  
  # 4. Construir etiquetas ajustadas dinámicamente al número real de intervalos
  etiquetas <- paste0(cortes_redondos[-length(cortes_redondos)], " a ", cortes_redondos[-1])
  
  # 5. Aplicar cut() de forma segura
  cut(
    x, 
    breaks = cortes_unicos, 
    include.lowest = TRUE,
    labels = etiquetas
  )
}


ivs_tabla <- ivs_tabla %>%
  mutate(
    across(
      # 1. Seleccionás las columnas de tus dimensiones
      c(z_educacion,z_empleo,z_hacinamiento,z_salud,z_infancia,z_infraestructura,IVS), 
      
      # 2. Aplicás la función de quintiles (el punto '.' representa cada columna)
      obtener_rangos_cuantil, 
      
      # 3. Le das un sufijo a las columnas nuevas para no sobrescribir las originales
      .names = "{.col}_quintil" 
    )
  )



# ==============================================================================
# 4. UNIÓN CON LA CAPA VECTORIAL BASE (RADIOS CENSALES)
# ==============================================================================
radios_gchu_sf <- st_read("datos_gis/radios_censales.gpkg") %>%
mutate(
    cod_indec = sprintf("30056%02d%02d", as.numeric(fraccion), as.numeric(radio))
  ) %>%
  st_filter(ejido, .predicate = st_intersects)

ivs_gchu_sf <- radios_gchu_sf %>%
  inner_join(st_drop_geometry(ivs_tabla), by = "cod_indec")

plot(st_geometry(ejido), border = "red", lwd = 2)
plot(st_geometry(ivs_gchu_sf), add = TRUE, col = "lightblue")

ivs_gchu_urbano_sf <- radios_gchu_sf %>%
  st_filter(ejido, .predicate = st_intersects) %>%
  # Calcular área en km2
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  # Conservar solo los radios urbanos (menores a 3 km2)
  filter(area_km2 < 3) %>% 
  inner_join(st_drop_geometry(ivs_tabla), by = "cod_indec")

plot(st_geometry(ivs_gchu_urbano_sf), col = "lightblue", border = "black")
radios_omitidos <- setdiff(ivs_tabla$cod_indec, ivs_gchu_urbano_sf$cod_indec)

cat("Cantidad de radios de la tabla que no están en el mapa urbano:", length(radios_omitidos), "\n")

# Filtrar las geometrías de los 91 radios omitidos
radios_omitidos_sf <- radios_gchu_sf %>%
  filter(cod_indec %in% radios_omitidos)

# 1. Ver qué fracciones censales quedaron fuera
# (Las fracciones urbanas de Gualeguaychú suelen ser las primeras; las más altas son rurales/departamento)
table(radios_omitidos_sf$fraccion)

# 2. Mapear rápidamente los radios omitidos en rojo junto a la mancha urbana en azul
plot(st_geometry(radios_omitidos_sf), col = "salmon", border = "red")
plot(st_geometry(ivs_gchu_urbano_sf), col = "lightblue", add = TRUE)
# ==============================================================================
# 5. INSPECCIÓN RESUMEN DEL ÍNDICE
# ==============================================================================
summary(ivs_gchu_sf$IVS)


##%######################################################%##
#                                                          #
####  Indice de concentracion espacial global y local   ####
#                                                          #
##%######################################################%##

library(dplyr)
library(sf)
library(ggplot2)
library(purrr)

# 1. Cálculo de superficie por radio (s_i)
mapa_ice <- ivs_gchu_sf %>%
  mutate(
    sup_m2 = as.numeric(st_area(.)),
    s_i = (sup_m2 / sum(sup_m2, na.rm = TRUE)) * 100
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
    # Ordenar por densidad/ICEA descendente para construir la curva superior
    arrange(desc(icea)) %>%
    mutate(
      s_acum = cumsum(s_i),
      a_acum = cumsum(a_i),
      dimension = nombre_dim,
      iceg_val = round(sum(diff_pos), 2)
    )
}

# 3. Mapeo de dimensiones con sus variables de conteo absoluto
variables_conteo <- list(
  "Educación"      = "pct_nbi_escolaridad", # O la variable con el conteo de casos
  "Desocupación"   = "pct_desocupacion",
  "Hacinamiento"   = "pct_hacinamiento",
  "Salud Pública"  = "pct_sin_cobertura",
  "Sin Cloacas"    = "pct_sin_cloaca",
  "Déficit Agua"   = "pct_deficit_agua"
)

# 4. Consolidación de datos de Lorenz
tabla_lorenz_global <- imap_dfr(variables_conteo, ~ {
  procesar_ice_dimension(mapa_ice, .x, .y)
})

# Etiquetas formateadas con el valor ICEG para cada panel
etiquetas_iceg <- tabla_lorenz_global %>%
  group_by(dimension) %>%
  summarise(label = paste0("ICEG: ", unique(iceg_val)))

# 5. Gráfico multi-panel de Curvas de Lorenz (Estilo Buzai)
ggplot(tabla_lorenz_global, aes(x = s_acum, y = a_acum)) +
  geom_ribbon(aes(ymin = s_acum, ymax = a_acum), fill = "#3182bd", alpha = 0.5) +
  geom_line(color = "#08519c", linewidth = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey30") +
  geom_text(data = etiquetas_iceg, aes(x = 25, y = 85, label = label), 
            fontface = "bold", size = 3.8, color = "black") +
  facet_wrap(~ dimension, ncol = 3) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  labs(
    title = "Figura. Curvas de concentración de Lorenz por Dimensión",
    subtitle = "Gualeguaychú - Censo 2022 | Adaptado de Metodología Buzai",
    x = "Superficie (%)",
    y = "Dimensión (% Acumulado)",
    caption = "Facultad de Bromatología - UNER"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


library(dplyr)

# Cálculo de la concentración acumulada al 20% y 50% de superficie para Gualeguaychú
cuadro2_gchu <- tabla_lorenz_global %>%
  group_by(dimension) %>%
  summarise(
    `20% Superficie` = paste0(round(approx(s_acum, a_acum, xout = 20)$y, 2), "%"),
    `50% Superficie` = paste0(round(approx(s_acum, a_acum, xout = 50)$y, 2), "%")
  )

print(cuadro2_gchu)


#Correlacion espacial



# 1. Matriz de Vecindad (Criterio Reina / Queen)
vecinos <- poly2nb(ivs_gchu_sf, queen = TRUE)
weights <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# 2. Test de Moran Global
moran_global <- moran.test(ivs_gchu_sf$IVS, weights, zero.policy = TRUE)
print(moran_global)

# 3. Cálculo de LISA Local
local_m <- localmoran(ivs_gchu_sf$IVS, weights, zero.policy = TRUE)

# 4. Construcción del Cuadrante LISA y Recategorización
ivs_lisa <- ivs_gchu_sf %>%
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
  )

# Agregar cada dimensión por quintiles mediante bucle
for (col in columnas_q) {
  nombre_capa <- paste0("Quintiles: ", gsub("_quintil", "", col) %>% gsub("z_", "", .) %>% toupper())
  
  mapa_completo <- mapa_completo %>%
    addPolygons(
      data = ivs_lisa_4326,
      fillColor = ~pal_quintil(get(col)),
      fillOpacity = 0.7,
      color = "#444444",
      weight = 1,
      popup = ~paste0(
        "<b>Radio: </b>", cod_indec, "<br>",
        "<b>", nombre_capa, ": </b>", get(col)
      ),
      group = nombre_capa
    )
}

# Agregar red espacial, centroides y control de capas
nombres_capas_q <- paste0("Quintiles: ", gsub("_quintil", "", columnas_q) %>% gsub("z_", "", .) %>% toupper())

mapa_completo <- mapa_completo %>%
  addPolylines(
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
    overlayGroups = c("Clusters LISA", nombres_capas_q, "Red de Vecindad (W)", "Nodos (Centroides)"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  setView(lng = -58.515, lat = -33.008, zoom = 12)

mapa_completo <- mapa_completo %>%
  # Leyenda 1: Clústeres LISA
  addLegend(
    pal = pal_lisa,
    values = ivs_lisa_4326$cluster,
    title = "Clústeres LISA",
    position = "bottomleft",
    opacity = 0.8
  ) %>%
  # Leyenda 2: Escala General de Quintiles
  addLegend(
    colors = RColorBrewer::brewer.pal(5, "YlOrRd"),
    labels = c("Q1 (Menor)", "Q2", "Q3", "Q4", "Q5 (Mayor)"),
    title = "Nivel de Quintil",
    position = "bottomright",
    opacity = 0.8
  )

mapa_completo

install.packages("tmap")
library(tmap)

# Configurar tmap en modo estático (plot)
tmap_mode("plot")

# Crear el mapa multipanel estático estilo Buzai
mapa_grid <- tm_shape(ivs_lisa) +
  tm_polygons(
    col = c("z_educacion_quintil", "z_empleo_quintil", 
            "z_hacinamiento_quintil", "z_salud_quintil", 
            "z_infancia_quintil", "z_infraestructura_quintil"),
    palette = "YlOrRd",
    title = "Quintiles",
    border.col = "grey40",
    lwd = 0.5
  ) +
  tm_facets(ncol = 2) + # 2 columnas x 3 filas (igual a la imagen de Buzai)
  tm_layout(
    legend.position = c("right", "bottom"),
    frame = FALSE
  )