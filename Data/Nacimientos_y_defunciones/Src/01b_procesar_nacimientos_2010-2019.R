#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 22 sept, 2021
# Procesamiento de datos de Defunciones y Nacimientos 2010-2018 + 2019
# Nota: 2020 no tiene aun la informacion de nacimientos por grupo de edad de la madre
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
# 0. Poblacion ----
#-------------------------------------------------------#

# Codigos de departamento y municipio
cod_dpto <- read_csv(glue("Descriptives/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv")) %>%
  select(cod_dpto, cod_mpio) %>% rename(nivel_value = cod_mpio)

# Abrimos datos de poblacion por genero y edad
pob_edad <- read_csv("Data/Proyecciones_poblacion/Output/base_proyecciones_poblacion_mpio_gen_edad_2005-2026.csv") %>%
  mutate(genero = str_extract(nivel_value, "_.*"), genero = substr(genero, 1, 2), 
         genero = as.numeric(gsub("_", "", genero)), edad = as.numeric(gsub(".*_", "", nivel_value)),
         mpio = as.numeric(gsub("_.*", "", nivel_value)))

# Solo mujeres entre 15-19 anios
pob_joven <- pob_edad %>% 
  filter(genero == 2 & edad >= 15 & edad <= 19) %>% 
  select(mpio, time, value) %>% rename(nivel_value = mpio, poblacion = value) %>%
  group_by(nivel_value, time) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup()

# Poblacion joven por departamento
pob_joven_dpto <- pob_joven %>%
  left_join(cod_dpto, by = "nivel_value") %>%
  group_by(time, cod_dpto) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>%
  rename(nivel_value = cod_dpto)

# Poblacion joven nacional
pob_joven_col <- pob_joven %>% 
  group_by(time) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup()

table(pob_joven_col$time)

#-------------------------------------------------------#
# 1. Nacional ----
#-------------------------------------------------------#

years <- c(2010:2018)

# Leer y organizar nacimientos de jovenes 15-19 a nivel nacional
nac_col <- lapply(years, function(x){
  
  if(x < 2014){
    lista <- list.files(path = glue("{datos_ori}/{x}"), pattern = "Cuadro7_.*")
  } 
  if(x >= 2014 & x < 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}"), pattern = "Cuadro7-.*")
  }
  
  print(x)
  print(lista)
  
  data <- read_excel(glue("{datos_ori}/{x}/{lista}"), sheet = "00") %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    select(de_15_19_anos) %>%
    rename(nacimientos = de_15_19_anos) %>%
    mutate(time = x, nacimientos = as.numeric(nacimientos)) %>%
    left_join(pob_joven_col, by = "time") %>%
    mutate(id_data = 19, nivel_value = 1,
           variable = "fecundidad_joven", value_label = "Tasa de fecundidad de mujeres jóvenes",
           id_nivel = "nacional", id_time = 1, value = (nacimientos/poblacion)*1000) %>%
    select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
  
  return(data)
  
}) %>% bind_rows()

# Pegamos anio 2019
nac_col19 <- read_csv(glue("{datos}/base_fecundidad_joven_col_2019.csv"))
nac_col <- bind_rows(nac_col, nac_col19)

# Exportar
write_csv(nac_col, glue("{datos}/base_fecundidad_joven_col_2010-2019.csv"))
rm(nac_col, nac_col19, pob_joven_col)

#-------------------------------------------------------#
# 2. Departamental ----
#-------------------------------------------------------#

# Lista de codigos departamentos en el estilo de datos DANE
lista_dpto <- cod_dpto %>% 
  select(cod_dpto) %>% 
  distinct(cod_dpto) %>%
  mutate(nivel_value = ifelse(nchar(cod_dpto) == 1, glue("0{cod_dpto}"), cod_dpto)) %>%
  .[["nivel_value"]]

# Leer cada anio de nacimientos y los departamentos
nac_dpto <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}"), pattern = "Cuadro7_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}"), pattern = "Cuadro7-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
      select(na, de_15_19_anos) %>%
      rename(dpto = na, nacimientos = de_15_19_anos) %>%
      filter(dpto == "Total") %>%
      mutate(time = y, nacimientos = as.numeric(nacimientos),
             nivel_value = as.numeric(x)) %>%
      left_join(pob_joven_dpto, by = c("time", "nivel_value")) %>%
      mutate(id_data = 19, variable = "fecundidad_joven", value_label = "Tasa de fecundidad de mujeres jóvenes",
             id_nivel = "dpto", id_time = 1, value = (nacimientos/poblacion)*1000) %>%
      select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

# Pegamos anio 2019
nac_dpto19 <- read_csv(glue("{datos}/base_fecundidad_joven_dpto_2019.csv"))
nac_dpto <- bind_rows(nac_dpto, nac_dpto19) %>% arrange(nivel_value)

# Exportar
write_csv(nac_dpto, glue("{datos}/base_fecundidad_joven_dpto_2010-2019.csv"))
rm(nac_dpto, nac_dpto19, pob_joven_dpto)

#-------------------------------------------------------#
# 3. Municipal ----
#-------------------------------------------------------#

# Leer cada anio de nacimientos y los departamentos
nac_mpio <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}"), pattern = "Cuadro7_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}"), pattern = "Cuadro7-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
      # select(na, de_15_19_anos) %>%
      rename(mpio = na, nacimientos = de_15_19_anos) %>%
      filter(mpio != "Total" & mpio != "05999 Sin Información") %>%
      mutate(nivel_value = as.numeric(substr(mpio, 1, 5)), 
             time = y, nacimientos = as.numeric(nacimientos)) %>%
      left_join(pob_joven, by = c("time", "nivel_value")) %>%
      mutate(id_data = 19, variable = "fecundidad_joven", value_label = "Tasa de fecundidad de mujeres jóvenes",
             id_nivel = "mpio", id_time = 1, value = (nacimientos/poblacion)*1000) %>%
      select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

# Pegamos anio 2019
nac_mpio19 <- read_csv(glue("{datos}/base_fecundidad_joven_mpio_2019.csv"))
nac_mpio <- bind_rows(nac_mpio, nac_mpio19) %>% arrange(nivel_value)

# Exportar
write_csv(nac_mpio, glue("{datos}/base_fecundidad_joven_mpio_2010-2019.csv"))
rm(nac_mpio, nac_mpio19, pob_joven)
