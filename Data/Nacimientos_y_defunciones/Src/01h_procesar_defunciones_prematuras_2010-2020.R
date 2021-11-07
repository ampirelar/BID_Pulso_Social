#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 04 oct, 2021
# Procesamiento de datos de Defunciones y Nacimientos 2010-2018 + 2019
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl, janitor)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Nacimientos_y_defunciones/Input"
datos <- "Data/Nacimientos_y_defunciones/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Nacional ----
#-------------------------------------------------------#

tot <- read_csv(glue("Data/Proyecciones_poblacion/Input/poblacion_dpto_2005-2026.csv"))

tot <- tot %>% drop_na(cod_dpto) %>%
  group_by(year) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% rename(time = "year")

years <- c(2010:2018)

# Leer y organizar defunciones menores a 1 anio a nivel nacional
def_col <- lapply(years, function(x){
  
  if(x < 2014){
    lista <- list.files(path = glue("{datos_ori}/{x}/Mortalidad_no_fetal"), pattern = "cuadro3_.*")
  } 
  if(x >= 2014 & x < 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}/Mortalidad_no_fetal"), pattern = "CUADRO3-.*")
  }
  
  print(x)
  print(lista)
  
  data <- read_excel(glue("{datos_ori}/{x}/Mortalidad_no_fetal/{lista}"), sheet = "00") %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    select(na, hombres_2, mujeres_2, indeterminado_2) %>%
    mutate(hombres_2 = as.numeric(hombres_2), mujeres_2 = as.numeric(mujeres_2), indeterminado_2 = as.numeric(indeterminado_2)) %>%
    mutate(time = x, defunciones = hombres_2 + mujeres_2 + indeterminado_2)
  
  return(data)
  
}) %>% bind_rows()

def_col <- def_col %>% filter(na == "TOTAL NACIONAL")

def_col <- def_col %>% 
  mutate(id_data = 19, nivel_value = 1, 
         variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "nacional", id_time = 1, value = defunciones) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Pegamos anio 2019
nac_col19 <- read_csv(glue("{datos}/defunciones_prematuras_col_2019-2020.csv"))
nac_col <- bind_rows(def_col, nac_col19) %>% arrange(time)

# Exportar
write_csv(nac_col, glue("{datos}/base_defunciones_prematuras_col_2010-2020.csv"))
rm(nac_col, nac_col19, def_col)

#-------------------------------------------------------#
# 2. Departamental ----
#-------------------------------------------------------#

# Codigos de departamento y municipio
cod_dpto <- read_csv(glue("Data/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv")) %>%
  select(cod_dpto, cod_mpio) %>% rename(nivel_value = cod_mpio)

# Lista de codigos departamentos en el estilo de datos DANE
lista_dpto <- cod_dpto %>% 
  select(cod_dpto) %>% 
  distinct(cod_dpto) %>%
  mutate(nivel_value = ifelse(nchar(cod_dpto) == 1, glue("0{cod_dpto}"), cod_dpto)) %>%
  .[["nivel_value"]]

# Leer cada anio de defunciones y los departamentos
def_dpto <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "cuadro3_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "CUADRO3-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Mortalidad_no_fetal/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 2) %>% janitor::clean_names() %>%
      select(na, hombres_2, mujeres_2, indeterminado_2) %>%
      rename(dpto = na) %>%
      filter(dpto == "TOTAL") %>%
      mutate(hombres_2 = as.numeric(hombres_2), mujeres_2 = as.numeric(mujeres_2), indeterminado_2 = as.numeric(indeterminado_2)) %>%
      mutate(time = y, defunciones = hombres_2 + mujeres_2 + indeterminado_2, nivel_value = as.numeric(x))
    
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

def_dpto <- def_dpto %>% 
  mutate(id_data = 19, variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "dpto", id_time = 1, value = defunciones) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Pegamos anio 2019
nac_dpto19 <- read_csv(glue("{datos}/base_defunciones_prematuras_dpto_2019-2020.csv"))
nac_dpto <- bind_rows(def_dpto, nac_dpto19) %>% arrange(nivel_value)

# Exportar
write_csv(nac_dpto, glue("{datos}/base_defunciones_prematuras_dpto_2010-2020.csv"))
rm(nac_dpto, nac_dpto19, def_dpto)

#-------------------------------------------------------#
# 3. Municipal ----
#-------------------------------------------------------#

# Leer cada anio de defunciones

def_mpio <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "cuadro3_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "CUADRO3-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Mortalidad_no_fetal/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 2) %>% janitor::clean_names() %>%
      select(na, na_2, hombres_2, mujeres_2, indeterminado_2) %>%
      rename(valor = na_2, mpio = na) %>%
      filter(valor == "TOTAL") %>%
      mutate(nivel_value = as.numeric(substr(mpio, 1, 5))) %>%
      mutate(hombres_2 = as.numeric(hombres_2), mujeres_2 = as.numeric(mujeres_2), indeterminado_2 = as.numeric(indeterminado_2)) %>%
      mutate(time = y, defunciones = hombres_2 + mujeres_2 + indeterminado_2)
    
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

def_mpio <- def_mpio %>% drop_na(nivel_value)

def_mpio <- def_mpio %>% 
  mutate(id_data = 19, variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "mpio", id_time = 1, value = defunciones) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Pegamos anio 2019

nac_mpio19 <- read_csv(glue("{datos}/base_defunciones_prematuras_mpio_2019-2020.csv"))
def_mpio <- bind_rows(def_mpio, nac_mpio19) %>% arrange(nivel_value)

# Exportar
write_csv(def_mpio, glue("{datos}/base_defunciones_prematuras_mpio_2010-2020.csv"))




