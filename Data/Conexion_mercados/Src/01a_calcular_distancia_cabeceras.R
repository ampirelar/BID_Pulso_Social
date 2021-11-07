#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 4 octubre, 2021
# Construir distancia a mercados
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl, janitor, sf, geosphere, gmapsdistance)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Conexion_mercados/Input"
datos <- "Data/Conexion_mercados/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Identificar origen y destino ----
#-------------------------------------------------------#

# Abrimos panel CEDE para identificar municipios de origen y destino
panel_cede <- read_excel("Data/Panel_cede/Input/PANEL_CARACTERISTICAS_GENERALES(2019).xlsx") %>%
  clean_names() %>%
  select(c(coddepto, codmpio, municipio, mercado_cercano)) %>%
  distinct() %>%
  rename(cod_dpto = coddepto, cod_mpio = codmpio, nom_mpio = municipio)

# Coordenadas cabeceras municipales
coords <- st_read(glue("{datos_ori}/Centroides_cabeceras_municipales/pladiv_label.shp")) %>%
  clean_names() %>% select(cod_dane, nom_muni, simbolo)

# Asignamos sistema de coordenadas del MGN del DANE
mgn <- st_read(glue("Data/Mapas/input/MGN_MPIO_POLITICO.shp")) %>%
  clean_names() %>% select(dpto_cnmbr, mpio_cdpmp)

coords <- st_transform(coords, crs = st_crs(mgn))

# Unimos coordenadas cabeceras con sus codigos de municipio
coords_mpio <- st_join(coords, mgn) %>% 
  rename(cod_dpto = cod_dane, nom_mpio = nom_muni, nom_dpto = dpto_cnmbr, cod_mpio = mpio_cdpmp) %>%
  select(cod_dpto, cod_mpio, nom_dpto, nom_mpio, simbolo) %>%
  mutate(cod_mpio = as.numeric(cod_mpio)) %>%
  drop_na(cod_mpio) %>%
  filter(simbolo == "CABECE" | simbolo == "CAP") 

coords_mpio <- cbind(coords_mpio$cod_mpio, st_coordinates(coords_mpio)) %>% 
  as.data.frame() %>% 
  rename(cod_mpio = V1, longitud = X, latitud = Y) %>%
  distinct(cod_mpio, .keep_all = T)

# coords_mpio[duplicated(coords_mpio$cod_mpio),]

# Unir origen y destino con coordenadas
data_coords <- panel_cede %>%
  left_join(coords_mpio, by = "cod_mpio") %>%
  rename(lon_ori = longitud, lat_ori = latitud, cod_mpio_ori = cod_mpio, cod_mpio = mercado_cercano) %>%
  left_join(coords_mpio, by = "cod_mpio") %>%
  rename(lon_des = longitud, lat_des = latitud, cod_mpio_des = cod_mpio, 
         cod_dpto_ori = cod_dpto, nom_mpio_ori = nom_mpio) %>%
  drop_na(lon_ori, lat_ori)

colSums(is.na(data_coords))
  
rm(mgn, coords, coords_mpio)

#-------------------------------------------------------#
# 2. Calculo distancias ----
#-------------------------------------------------------#

# El key de la API de google maps distance se obtiene de googlecloud, pasos: 
# 1. registrarse, inscribir datos de tarjeta credito (se inicia con $300 de prueba gratuita)
# 2. ir a API y servicios, habilitar API
# 3. buscar distance matrix API, habilitar
# 4. ir a credenciales, crear credenciales, clave de API

# Coordenadas deben concatenarse en un string
data_google <- data_coords %>%
  mutate(coords_ori = paste(as.character(lat_ori), as.character(lon_ori), sep=",")) %>% 
  mutate(coords_des = paste(as.character(lat_des), as.character(lon_des), sep=",")) 

# Calcular distancias en automovil, domingo a las 11pm
key <- "AIzaSyDgA1BS9kykm5iUbV98us-d0NXiV4G3NCM"
set.api.key(key)
base_dist <- gmapsdistance(data_google$coords_ori, data_google$coords_des, 
                           combinations = "pairwise", mode = "driving",
                           dep_date = "2021-10-10", dep_time = "23:00:00")

# Organizar base con distancias en km y tiempo en horas
data_dist <- data_google %>%
  mutate(distance = base_dist[["Distance"]][["Distance"]], time = base_dist[["Time"]][["Time"]]) %>%
  select(cod_mpio_ori, cod_mpio_des, distance, time) %>%
  mutate(distance = distance/1000, time = (time/60)/60) 

# Base distancias
dist <- data_dist %>%
  mutate(id_data = 22, nivel_value = cod_mpio_ori, variable = "dist_mer", 
         value_label = "Distancia al mercado más cercano", id_nivel = "mpio", 
         id_time = 1, time = 2021, value = distance) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Base tiempo
time <- data_dist %>%
  mutate(id_data = 22, nivel_value = cod_mpio_ori, variable = "tiempo_mer", 
         value_label = "Tiempo al mercado más cercano", id_nivel = "mpio", 
         id_time = 1, value = time, time = 2021) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(dist, glue("{datos}/base_dist_mer_mpio_2021.csv"))
write_csv(time, glue("{datos}/base_tiempo_mer_mpio_2021.csv"))


