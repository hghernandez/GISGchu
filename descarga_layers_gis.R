library(httr2)
library(xml2)

library(sf)


# Usar HTTP si HTTPS no responde a la conexión inicial
url_xml <- "http://geo.gualeguaychu.gov.ar/geoserver/ows?service=WFS&version=2.0.0&request=GetCapabilities"

tryCatch({
  resp <- request(url_xml) %>%
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") %>%
    req_options(connecttimeout = 120) %>%  # Extiende el tiempo de conexión inicial
    req_timeout(120) %>%                   # Extiende el tiempo de respuesta
    req_perform()
  
  xml <- read_xml(resp_body_string(resp))
  capas_nombres <- xml_text(xml_find_all(xml, ".//wfs:FeatureType/wfs:Name"))
  
  catalogo <- data.frame(capa = capas_nombres, stringsAsFactors = FALSE)
  write.csv(catalogo, "capas_disponibles.csv", row.names = FALSE)
  
  cat("¡Éxito! Catálogo generado con", nrow(catalogo), "capas en 'capas_disponibles.csv'.\n")
  
}, error = function(e) {
  cat("\nNo se pudo establecer conexión con GeoServer.\n")
  cat("Detalle:", conditionMessage(e), "\n")
  cat("\nTip: Si el servidor municipal está fuera de línea temporalmente, reintenta en unos minutos.\n")
})


