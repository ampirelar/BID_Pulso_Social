#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 27 sept, 2021
# Procesar indice de vulnerabilidad climatica 2071-2100
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl, janitor, sf, rgeos)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Indice_vulnerabilidad_climatica/Input"
datos <- "Data/Indice_vulnerabilidad_climatica/Output"
options(scipen = 999)

# Porcentaje area mpio que cae en cada categoria de vulnerabilidad

#-------------------------------------------------------#
# 0. Abrir datos ----
#-------------------------------------------------------#

# Indice de vulnerabilidad climatica: calculamos area poligonos en km2
ind <- st_read(glue("{datos_ori}/VlnrAmbn_2071_2100.shp")) %>% 
  clean_names() %>% dplyr::select(-c(objectid, ruleid))

# Corregimos algunos errores en definicion poligonos
ind <- st_make_valid(ind)
ind <- ind %>% select(-starts_with("shape"))
head(ind)

# Guardamos shp corregido
# saveRDS(ind, glue("{datos}/indice_vulnerabilidad_climatica.rds"))
ind <- readRDS(glue("{datos}/indice_vulnerabilidad_climatica.rds"))

# Shp de municipios
mpio <- st_read("Data/Mapas/Output/mapa_municipios_colombia.shp") %>% 
  dplyr::select(nivl_vl) %>% rename(nivel_value = nivl_vl) %>%
  mutate(area_m2 = st_area(geometry), area_km2 = area_m2/1000000) %>%
  select(-area_m2)

mpio$area_km2 <- units::set_units(mpio$area_km2, NULL)

# Shp de departamentos
dpto <- st_read("Data/Mapas/Output/mapa_departamentos_colombia.shp") %>% 
  dplyr::select(nivl_vl) %>% rename(nivel_value = nivl_vl) %>%
  mutate(area_m2 = st_area(geometry), area_km2 = area_m2/1000000) %>%
  select(-area_m2)

dpto$area_km2 <- units::set_units(dpto$area_km2, NULL)

# Pasamos mpios y dptos a magna sirgas (CRS  del indice)
mpio <- st_transform(mpio, crs = st_crs(ind))
dpto <- st_transform(dpto, crs = st_crs(ind))

#-------------------------------------------------------#
# 1. Extraer informacion indice ----
# Calculamos el % area del mpio/dpto que cae dentro de categorias de vulnerabilidad climatica
# E.g. el 85% del area del mpio tiene una vulnerabilidad "Muy alta"
#-------------------------------------------------------#

# Interceptamos poligonos de vulnerabilidad climatica con poligonos administrativos

# Departamental
dpto_ind <- st_intersection(dpto, ind)

# Organizamos dataframe: colapsamos areas segun vulnerabilidad y calculamos % sobre area poligono
data_dpto <- dpto_ind %>% mutate(area_clim = st_area(geometry), area_clim = area_clim/1000000)
data_dpto$area_clim <- units::set_units(data_dpto$area_clim, NULL)
st_geometry(data_dpto) <- NULL

data_dpto <- data_dpto %>%
  group_by(nivel_value, vulnerable, area_km2) %>%
  summarise(area_clim = sum(area_clim)) %>%
  ungroup() %>%
  group_by(nivel_value) %>%
  mutate(area_total = sum(area_clim)) %>%
  ungroup() %>%
  mutate(part_cat = 100*(area_clim/area_total))

table(data_dpto$vulnerable)

# Municipal
mpio_ind <- st_intersection(mpio, ind)

# Organizamos dataframe: colapsamos areas segun vulnerabilidad y calculamos % sobre area poligono
data_mpio <- mpio_ind %>% mutate(area_clim = st_area(geometry), area_clim = area_clim/1000000)
data_mpio$area_clim <- units::set_units(data_mpio$area_clim, NULL)
st_geometry(data_mpio) <- NULL

data_mpio <- data_mpio %>%
  group_by(nivel_value, vulnerable, area_km2) %>%
  summarise(area_clim = sum(area_clim)) %>%
  ungroup() %>%
  group_by(nivel_value) %>%
  mutate(area_total = sum(area_clim)) %>%
  ungroup() %>%
  mutate(part_cat = 100*(area_clim/area_total))

table(data_mpio$vulnerable)

#-------------------------------------------------------#
# 3. Organizar bases ----
#-------------------------------------------------------#

# Departamental
data_dpto <- data_dpto %>%
  mutate(id_data = 20, nivel_value = glue("{nivel_value}_{vulnerable}"), variable = "area_vul_clim", 
         value_label = "Área vulnerable al cambio climático", id_nivel = "dpto", 
         id_time = 4, time = "2071-2100", value = part_cat) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(data_dpto, glue("{datos}/base_vulnerabilidad_dpto_2071-2100.csv"))

# Municipal
data_mpio <- data_mpio %>%
  mutate(id_data = 20, nivel_value = glue("{nivel_value}_{vulnerable}"), variable = "area_vul_clim", 
         value_label = "Área vulnerable al cambio climático", id_nivel = "mpio", 
         id_time = 4, time = "2071-2100", value = part_cat) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(data_dpto, glue("{datos}/base_vulnerabilidad_mpio_2071-2100.csv"))



