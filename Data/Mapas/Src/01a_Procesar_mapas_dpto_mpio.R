#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 30 agosto, 2021
# Procesar mapas departamentales y municipales para creacion de graficas
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, raster)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Mapas/Input"
datos <- "Data/Mapas/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Abrir shapefiles ----
#-------------------------------------------------------#

#--------------------------#
# A. Departamentos ----
#--------------------------#

# Mapas a nivel departamental
map_dptos <- st_read("Data/Mapas/Input/MGN_DPTO_POLITICO.shp") %>% 
  dplyr::select(DPTO_CCDGO, DPTO_CNMBR) %>% 
  rename(nivel_value = DPTO_CCDGO, nivel_label = DPTO_CNMBR) %>% 
  mutate(nivel_value = as.numeric(nivel_value), id_nivel = "dpto", nivel_label = str_to_title(nivel_label)) %>%
  dplyr::select(id_nivel, nivel_label, nivel_value)

# Transformamos a CRS Magna Sirgas (proyectado) para simplificar correctamente
projcrs <- "+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
map_dptos <- sf::st_transform(map_dptos, crs = CRS('+init=EPSG:3116'))
map_dptos <- st_set_crs(map_dptos, projcrs)

# Simplificamos la geometria de los poligonos: evita bordes muy detallados que consumen memoria
map_dptos <- st_simplify(map_dptos, preserveTopology = FALSE, dTolerance = 1000)

# Transformamos a CRS 4326
lonlat <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
map_dptos <- st_transform(map_dptos, crs = CRS('+init=EPSG:4326'))
map_dptos <- st_set_crs(map_dptos, lonlat)

# Extraemos coordenadas (para etiquetas de departamento)
map_dptos <- cbind(map_dptos, st_coordinates(st_centroid(map_dptos)))
head(map_dptos)

# Guardamos mapa
st_write(map_dptos, glue("{datos}/mapa_departamentos_colombia.shp"))

#--------------------------#
# B. Municipios ----
#--------------------------#

# Mapas a nivel municipal
map_mpios <- st_read("Data/Mapas/Input/MGN_MPIO_POLITICO.shp") %>% 
  dplyr::select(DPTO_CCDGO, DPTO_CNMBR, MPIO_CCDGO, MPIO_CNMBR) %>% 
  rename(cod_dpto = DPTO_CCDGO, nom_dpto = DPTO_CNMBR, cod_mpio = MPIO_CCDGO, nom_mpio = MPIO_CNMBR) %>% 
  mutate(cod_mpio = as.numeric(glue("{cod_dpto}{cod_mpio}")), cod_dpto = as.numeric(cod_dpto),
         nom_dpto = str_to_title(nom_dpto), nom_mpio = str_to_title(nom_mpio), id_nivel = "mpio") %>% 
  rename(nivel_value = cod_mpio, nivel_label = nom_mpio) %>%
  dplyr::select(id_nivel, nivel_label, nivel_value)

# Transformamos a CRS Magna Sirgas (proyectado) para simplificar correctamente
map_mpios <- sf::st_transform(map_mpios, crs = CRS('+init=EPSG:3116'))
map_mpios <- st_set_crs(map_mpios, projcrs)

# Simplificamos la geometria de los poligonos
map_mpios <- st_simplify(map_mpios, preserveTopology = FALSE, dTolerance = 1000)

# Transformamos a CRS 4326 (Original: 4686 Magna Sirgas 2020)
map_mpios <- st_transform(map_mpios, crs = CRS('+init=EPSG:4326'))
map_mpios <- st_set_crs(map_mpios, lonlat)

# Extraemos coordenadas (para etiquetas de municipio)
map_mpios <- cbind(map_mpios, st_coordinates(st_centroid(map_mpios)))

# Guardamos mapa
st_write(map_mpios, glue("{datos}/mapa_municipios_colombia.shp"))
