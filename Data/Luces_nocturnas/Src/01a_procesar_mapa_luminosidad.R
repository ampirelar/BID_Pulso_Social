#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 18 octubre, 2021
# Procesar datos satelitales de luces nocturnas
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, raster, pulsosocialcolombia, colorspace)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Luces_nocturnas/Input"
datos <- "Data/Luces_nocturnas/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Abrir y recortar raster ----
#-------------------------------------------------------#

# Abrir shp de colombia para recortar raster de luces
col <- st_read("Data/Mapas/Input/MGN_DPTO_POLITICO.shp")

# Recortar rasters 2012-2021
# 2021 corresponde al mes de abril (dato mas reciente al 05/10/2021) y no esta disponible sin background
years <- c(2012:2021)

lapply(years, function(x){
  
  print(x)
  # Abrir raster
  if(x == 2012){
    luces <- raster(glue("{datos_ori}/VNL_v2_npp_201204-201303_global_vcmcfg_c202102150000.average_masked.tif"))
  } 
  if(x == 2013){
    luces <- raster(glue("{datos_ori}/VNL_v2_npp_{x}_global_vcmcfg_c202102150000.average_masked.tif"))
  } 
  if(x >= 2014 & x <= 2020){
    luces <- raster(glue("{datos_ori}/VNL_v2_npp_{x}_global_vcmslcfg_c202102150000.average_masked.tif"))
  }
  if(x == 2021){
    luces <- raster(glue("{datos_ori}/SVDNB_npp_20210401-20210430_global_vcmcfg_v10_c202105062200.avg_rade9h.tif"))
  }
  
  # Recortar
  luces_col <- raster::crop(luces, extent(col))
  luces_col <- raster::mask(luces_col, col)

  # Guardar raster
  writeRaster(luces_col, glue("{datos}/rasters_col/mapa_luces_nocturnas_col_{x}.tif"))
})

rm(col, years)

#-------------------------------------------------------#
# 2. Luminosidad por municipio ----
#-------------------------------------------------------#

# Abrir shp de municipios
mpios <- st_read("Data/Mapas/Input/MGN_MPIO_POLITICO.shp") %>%
  janitor::clean_names() %>% dplyr::select(mpio_cnmbr, mpio_cdpmp)

# Lista municipios
lista_mpios <- unique(mpios$mpio_cdpmp)

# Identifica pixeles de luminosidad por municipio y calcula promedio anual
lapply(2012:2020, function(y){
  
  print(y)
  luces_col <- raster(glue("{datos}/rasters_col/mapa_luces_nocturnas_col_{y}.tif"))
  
  luces_px <- lapply(lista_mpios, function(x){
    data_px <- raster::extract(luces_col, mpios[mpios$mpio_cdpmp == x,], fun = mean, na.rm = T) 
    data_px <- data.frame(cod_mpio = x, year = y, mean_light = data_px)
    return(data_px)
  }) %>% bind_rows()
  
  # Guardar
  saveRDS(luces_px, glue("{datos}/luces_nocturnas_anual/base_luces_nocturnas_mpio_{y}.rds"))
  
})

# Enlistar, leer y unir bases de datos de luminosidad municipal 2012-2020
lista_luces <- list.files(path = glue("{datos}/luces_nocturnas_anual"), pattern = "base_luces_nocturnas_mpio", full.names = T)
df_luces <- lapply(lista_luces, readRDS) %>% bind_rows()
table(df_luces$year)  

#--------------------------#
# A. Logaritmo luminosidad ----
#--------------------------#

# Organizar datos
data_luces <- df_luces %>%
  # Sumamos 0.01 a municipios con luminosidad 0
  mutate(mean_light = ifelse(mean_light == 0, mean_light + 0.001, mean_light)) %>%
  mutate(id_data = 22, variable = "log_luces_nocturnas", 
         value_label = "Logaritmo de la intensidad lumínica promedio", id_nivel = "mpio", 
         id_time = 1, nivel_value = as.numeric(cod_mpio), value = log(mean_light), time = year) 

# Hay 389 obs con medicion 0
# data_luces$value[is.infinite(data_luces$value)] <- NA
colSums(is.na(data_luces))
sum(is.infinite(data_luces$value))

data_luces <- data_luces %>%
  dplyr::select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  arrange(nivel_value, time)

# Exportar datos
write_csv(data_luces, glue("{datos}/base_log_luces_nocturnas_mpio_2012-2020.csv"))
rm(data_luces)

#--------------------------#
# B. Luminosidad per capita ----
#--------------------------#

# Abrimos proyecciones de poblacion municipales
pob_muni <- read_csv("Data/Proyecciones_poblacion/Output/base_proyecciones_poblacion_mpio_1985-2035.csv") %>%
  filter(time >=2012 & time <= 2020) %>% 
  dplyr::select(nivel_value, time, value) %>% 
  rename(poblacion = value)

data_luces <- df_luces %>%
  # Sumamos 0.01 a municipios con luminosidad 0
  mutate(mean_light = ifelse(mean_light == 0, mean_light + 0.001, mean_light)) %>%
  mutate(id_data = 22, variable = "luces_nocturnas_pc", 
         value_label = "Intensidad lumínica promedio per cápita", id_nivel = "mpio", 
         id_time = 1, nivel_value = as.numeric(cod_mpio), time = year) %>%
  dplyr::select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, mean_light) %>%
  left_join(pob_muni, by = c("nivel_value", "time")) %>%
  # Luces per capita
  mutate(value = mean_light/poblacion) %>%
  arrange(nivel_value, time) %>%
  dplyr::select(-c(mean_light, poblacion))

# Exportar datos
write_csv(data_luces, glue("{datos}/base_luces_nocturnas_pc_mpio_2012-2020.csv"))
rm(data_luces, df_luces, pob_muni, lista_luces)

#-------------------------------------------------------#
# 3. Luminosidad por departamento ----
#-------------------------------------------------------#

# Abrir shp de departamentos
dptos <- st_read("Data/Mapas/Input/MGN_DPTO_POLITICO.shp") %>%
  janitor::clean_names() %>% dplyr::select(dpto_cnmbr, dpto_ccdgo)

# Lista departamentos
lista_dptos <- unique(dptos$dpto_ccdgo)

# Identifica pixeles de luminosidad por departamento y calcula promedio anual
lapply(2012:2020, function(y){
  
  print(y)
  luces_col <- raster(glue("{datos}/rasters_col/mapa_luces_nocturnas_col_{y}.tif"))
  
  luces_px <- lapply(lista_dptos, function(x){
    data_px <- raster::extract(luces_col, dptos[dptos$dpto_ccdgo == x,], fun = mean, na.rm = T) 
    data_px <- data.frame(cod_dpto = x, year = y, mean_light = data_px)
    return(data_px)
  }) %>% bind_rows()
  
  # Guardar
  saveRDS(luces_px, glue("{datos}/luces_nocturnas_anual/base_luces_nocturnas_dpto_{y}.rds"))
  
})

# Enlistar, leer y unir bases de datos de luminosidad departamental 2012-2020
lista_luces <- list.files(path = glue("{datos}/luces_nocturnas_anual"), pattern = "base_luces_nocturnas_dpto", full.names = T)
df_luces <- lapply(lista_luces, readRDS) %>% bind_rows()
table(df_luces$year)  

#--------------------------#
# A. Logaritmo luminosidad ----
#--------------------------#

# Organizar datos
data_luces <- df_luces %>%
  # Sumamos 0.01 a municipios con luminosidad 0
  mutate(mean_light = ifelse(mean_light == 0, mean_light + 0.001, mean_light)) %>%
  mutate(id_data = 22, variable = "log_luces_nocturnas", 
         value_label = "Logaritmo de la intensidad lumínica promedio", id_nivel = "dpto", 
         id_time = 1, nivel_value = as.numeric(cod_dpto), value = log(mean_light), time = year) 

# Hay 0 obs con medicion 0
# data_luces$value[is.infinite(data_luces$value)] <- NA
colSums(is.na(data_luces))

data_luces <- data_luces %>%
  dplyr::select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  arrange(nivel_value, time)

# Exportar datos
write_csv(data_luces, glue("{datos}/base_log_luces_nocturnas_dpto_2012-2020.csv"))
rm(data_luces)

#--------------------------#
# B. Luminosidad per capita ----
#--------------------------#

# Abrimos proyecciones de poblacion municipales
pob_dpto <- read_csv("Data/Proyecciones_poblacion/Output/base_proyecciones_poblacion_mpio_1985-2035.csv") %>%
  filter(time >=2012 & time <= 2020) %>% 
  dplyr::select(nivel_value, time, value) %>% 
  rename(poblacion = value) %>%
  mutate(nivel_value = ifelse(nchar(nivel_value) == 4, glue("0{nivel_value}"), nivel_value),
         nivel_value = substr(nivel_value, 1, 2), nivel_value = as.numeric(nivel_value)) %>%
  group_by(nivel_value, time) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup()

table(pob_dpto$time)

data_luces <- df_luces %>%
  # Sumamos 0.01 a municipios con luminosidad 0
  mutate(mean_light = ifelse(mean_light == 0, mean_light + 0.001, mean_light)) %>%
  mutate(id_data = 22, variable = "luces_nocturnas_pc", 
         value_label = "Intensidad lumínica promedio per cápita", id_nivel = "mpio", 
         id_time = 1, nivel_value = as.numeric(cod_dpto), time = year) %>%
  dplyr::select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, mean_light) %>%
  left_join(pob_dpto, by = c("nivel_value", "time")) %>%
  # Luces per capita
  mutate(value = mean_light/poblacion) %>%
  arrange(nivel_value, time) %>%
  dplyr::select(-c(mean_light, poblacion))

# Exportar datos
write_csv(data_luces, glue("{datos}/base_luces_nocturnas_pc_dpto_2012-2020.csv"))
rm(data_luces, df_luces, pob_dpto, dptos, lista_luces)






# Algunos municipios marcan mediciones en 0: emiten muy poca luz
# 05475 5842 5873 15212 19318
# luces_col <- raster(glue("{datos}/rasters_col/mapa_luces_nocturnas_col_2013.tif"))
# test <- mpios[mpios$mpio_cdpmp == "05475",]
# plot(luces_col)
# plot(test$geometry, add= T, colour = "red")
# data_px <- raster::extract(luces_col, mpios[mpios$mpio_cdpmp == "05475",])

  
  
  
  
  
