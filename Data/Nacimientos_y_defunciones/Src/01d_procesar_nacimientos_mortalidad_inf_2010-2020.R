#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 26 sept, 2021
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

# Nacimientos a nivel nacional

nac_col <- lapply(years, function(x){
  
  if(x < 2014){
    lista <- list.files(path = glue("{datos_ori}/{x}/Nacimientos"), pattern = "Cuadro7_.*")
  } 
  if(x >= 2014 & x < 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}/Nacimientos"), pattern = "Cuadro7-.*")
  }
  
  print(x)
  print(lista)
  
  data <- read_excel(glue("{datos_ori}/{x}/Nacimientos/{lista}"), sheet = "00") %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    select(na_2) %>%
    rename(nacimientos = na_2) %>%
    mutate(time = x, nacimientos = as.numeric(nacimientos)) %>%
    left_join(def_col, by = "time") %>%
    mutate(id_data = 19, nivel_value = 1,
           variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil",
           id_nivel = "nacional", id_time = 1, value = (defunciones/nacimientos)*1000) %>%
    select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
  
  return(data)
  
}) %>% bind_rows()

# Pegamos anio 2019
nac_col19 <- read_csv(glue("{datos}/base_mortalidad_inf_col_2019-2020.csv"))
nac_col <- bind_rows(nac_col, nac_col19)

# Exportar
write_csv(nac_col, glue("{datos}/base_mortalidad_inf_col_2010-2020.csv"))
rm(nac_col, nac_col19, def_col)

#-------------------------------------------------------#
# 2. Departamental ----
#-------------------------------------------------------#

# Codigos de departamento y municipio
cod_dpto <- read_csv(glue("Descriptives/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv")) %>%
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

# Nacimientos por dpto

nac_dpto <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}/Nacimientos"), pattern = "Cuadro7_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Nacimientos"), pattern = "Cuadro7-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Nacimientos/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
      rename(dpto = na, nacimientos = na_2) %>%
      filter(dpto == "Total") %>%
      mutate(time = y, nacimientos = as.numeric(nacimientos),
             nivel_value = as.numeric(x)) %>%
      left_join(def_dpto, by = c("time", "nivel_value")) %>%
      mutate(id_data = 19, variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil",
             id_nivel = "dpto", id_time = 1, value = (defunciones/nacimientos)*1000) %>%
      select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

# Pegamos anio 2019
nac_dpto19 <- read_csv(glue("{datos}/base_mortalidad_inf_dpto_2019-2020.csv"))
nac_dpto <- bind_rows(nac_dpto, nac_dpto19) %>% arrange(nivel_value)

# Exportar
write_csv(nac_dpto, glue("{datos}/base_mortalidad_inf_dpto_2010-2020.csv"))
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

#Nacimientos mpio

nac_mpio <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}/Nacimientos"), pattern = "Cuadro7_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Nacimientos"), pattern = "Cuadro7-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Nacimientos/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
      select(na, na_2) %>%
      rename(mpio = na, nacimientos = na_2)  %>%
      filter(mpio != "Total" & mpio != "05999 Sin Información") %>%
      mutate(nivel_value = as.numeric(substr(mpio, 1, 5)), 
             time = y, nacimientos = as.numeric(nacimientos)) %>%
      left_join(def_mpio, by = c("time", "nivel_value")) %>%
      mutate(id_data = 19, variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil",
             id_nivel = "mpio", id_time = 1, value = (defunciones/nacimientos)*1000) %>%
      select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()
nac_mpio <- nac_mpio %>% drop_na(nivel_value)

# Pegamos anio 2019

nac_mpio19 <- read_csv(glue("{datos}/base_mortalidad_inf_mpio_2019-2020.csv"))
nac_mpio <- bind_rows(nac_mpio, nac_mpio19) %>% arrange(nivel_value)

# Exportar
write_csv(nac_mpio, glue("{datos}/base_mortalidad_inf_mpio_2010-2020.csv"))
