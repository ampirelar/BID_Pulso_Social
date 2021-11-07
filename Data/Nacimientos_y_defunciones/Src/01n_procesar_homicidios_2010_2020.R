#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 12 oct, 2021
# Procesamiento de datos de homicidios 2010 -2020
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

tot <- tot %>% group_by(year) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% rename(time = "year")

years <- c(2010:2018)

# Leer y organizar defunciones a nivel nacional
def_col <- lapply(years, function(x){
  
  if(x < 2014){
    lista <- list.files(path = glue("{datos_ori}/{x}/Mortalidad_no_fetal"), pattern = "cuadro5_.*")
  } 
  if(x >= 2014 & x < 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}/Mortalidad_no_fetal"), pattern = "CUADRO5-.*")
  }
  
  print(x)
  print(lista)
  
  data <- read_excel(glue("{datos_ori}/{x}/Mortalidad_no_fetal/{lista}"), sheet = "00") %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    rename(value = na_2, gen_1 = hombres, gen_2 = mujeres) %>%
    mutate(cod_enf = as.character(substr(na, 1, 3)), time = x)%>% filter(cod_enf == "512")
  
  
  
  return(data)
  
}) %>% bind_rows()

#homicidios----

nac <- def_col %>% left_join(tot, by = "time") %>% mutate(value = (as.numeric(value)/poblacion)*100000,
                          id_data = 19, variable = "homicidios", value_label = "Agresiones (homicidios)",
                          id_nivel = "nacional", id_time = 1, nivel_value = glue("1")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_col19 <- read_csv(glue("{datos}/base_homicidios_col_2019-2020.csv"))
nac <- rbind (nac, def_col19)

write_csv(nac, glue("{datos}/base_homicidios_col_2010-2020.csv"))

rm(nac, def_col19)

# homicidios - género----

nac <- def_col %>% pivot_longer(cols = starts_with("gen_"), names_to = "gen", values_to = "homicidios") %>%
  left_join(tot, by = "time") %>% 
  mutate(value = (as.numeric(homicidios)/poblacion)*100000, id_data = 19, variable = "homicidios", gen = as.numeric(gsub("gen_", "", gen)),
         value_label = "Agresiones (homicidios)",
         id_nivel = "nacional_gen", id_time = 1, nivel_value = glue("1_{gen}")) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_col19 <- read_csv(glue("{datos}/base_homicidios_genero_col_2019-2020.csv"))
nac <- rbind (nac, def_col19)

write_csv(nac, glue("{datos}/base_homicidios_genero_col_2010-2020.csv"))

rm(nac, def_col19)

# homicidios - edad----

cols = c(12:34)
def_col[, cols] = apply(def_col[, cols], 2, function(x) as.numeric(as.character(x)))
def_col[is.na(def_col)] <- 0

def_col <- def_col %>% .[,-c(2:11)] 
def_col <- def_col %>% mutate(edad_5_a_14 = rowSums(.[2:4]), edad_15_a_44 = rowSums(.[5:7]),
                              edad_45_a_64 = rowSums(.[8:10]), edad_65_o_mas = rowSums(.[11:16]) + rowSums(.[19:21])) %>% 
  select(na, edad_5_a_14, edad_15_a_44, edad_45_a_64, edad_65_o_mas, time) %>%
  pivot_longer(cols = starts_with("edad_"), names_to = "edad", values_to = "homicidios") %>%
  mutate(edad = as.character(gsub("edad_", "", edad)))

def_col <- def_col %>% left_join(tot, by = "time") %>% 
  mutate(value = (as.numeric(homicidios)/poblacion)*100000, id_data = 19, variable = "homicidios",
                              value_label = "Agresiones (homicidios)",
                              id_nivel = "nacional_edad", id_time = 1, nivel_value = glue("1_{edad}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_col19 <- read_csv(glue("{datos}/homicidios_edad_col_2019-2020.csv"))
def_col <- rbind (def_col, def_col19)

write_csv(def_col, glue("{datos}/base_homicidios_edad_col_2010-2020.csv"))

rm(def_col, def_col19)

#-------------------------------------------------------#
# 2. Departamental ----
#-------------------------------------------------------#

# Codigos de departamento y municipio

path <- getwd()
path <- gsub("01_Datos", "02_Descriptivas", path)
options(scipen = 999)

cod_dpto <- read_csv(glue("{path}/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv")) %>%
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
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "cuadro5_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "CUADRO5-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Mortalidad_no_fetal/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 2) %>% janitor::clean_names() %>%
      rename(value = na_3) %>%
      rename(dpto = na, gen_1 = hombres, gen_2 = mujeres) %>% fill(dpto,.direction = "down") %>%
      mutate(time = y, cod_enf = as.character(substr(na_2, 1, 3)),
             value_label = "Agresiones (homicidios)", 
             id_time = 1, id_data = 19, id_d = x) %>%
      filter(cod_enf == "512")
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

#homicidios----

tot <- read_csv(glue("Data/Proyecciones_poblacion/Input/poblacion_dpto_2005-2026.csv"))

dpto <- def_dpto %>% filter(dpto == "TOTAL") %>% filter(na_2 != "TOTAL") %>%
  mutate(cod_dpto = as.numeric(id_d), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", value = (as.numeric(value)/poblacion)*100000,
         nivel_value = glue("{as.numeric(id_d)}"), id_nivel = "dpto", variable = "homicidios") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_dpto19 <- read_csv(glue("{datos}/homicidios_dpto_2019-2020.csv"))

nac_dpto <- rbind (dpto, def_dpto19)

write_csv(nac_dpto, glue("{datos}/base_homicidios_dpto_2010-2020.csv"))

rm(cod_dpto, def_dpto19, dpto, nac_dpto)

#homicidios - género----

dpto <- def_dpto %>% filter(dpto == "TOTAL") %>% filter(na_2 != "TOTAL") %>%
  pivot_longer(cols = starts_with("gen_"), names_to = "gen", values_to = "homicidios") %>%
  mutate(cod_dpto = as.numeric(id_d), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", id_data = 19, variable = "homicidios",
         gen = as.numeric(gsub("gen_", "", gen)), value = (as.numeric(homicidios)/poblacion)*100000,
         value_label = "Agresiones (homicidios)",
         id_nivel = "dpto_gen", id_time = 1, nivel_value = glue("{as.numeric(id_d)}_{gen}")) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_dpto19 <- read_csv(glue("{datos}/homicidios_genero_dpto_2019-2020.csv"))

dpto <- rbind (dpto, def_dpto19)

write_csv(dpto, glue("{datos}/base_homicidios_genero_dpto_2010-2020.csv"))

rm(def_dpto19, dpto)

# homicidios - edad----

cols = c(3:39)
def_dpto[, cols] = apply(def_dpto[, cols], 2, function(x) as.numeric(as.character(x)))
def_dpto[is.na(def_dpto)] <- 0

def_dpto <- def_dpto %>% mutate(edad_5_a_14 = rowSums(.[13:15]), edad_15_a_44 = rowSums(.[16:18]),
                                edad_45_a_64 = rowSums(.[19:21]), edad_65_o_mas = rowSums(.[22:27]) + rowSums(.[34:36])) %>% 
  select(dpto, edad_5_a_14, edad_15_a_44, edad_45_a_64, edad_65_o_mas, time, id_d) %>%
  pivot_longer(cols = starts_with("edad_"), names_to = "edad", values_to = "homicidios") %>%
  mutate(edad = as.character(gsub("edad_", "", edad)))

dpto <- def_dpto %>% filter(dpto == "TOTAL") %>% 
  mutate(cod_dpto = as.numeric(id_d), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", id_data = 19, variable = "homicidios",
         value = (as.numeric(homicidios)/poblacion)*100000,
         value_label = "Agresiones (homicidios)", nivel_label = glue("{nom_dpto}_{edad}"),
         id_nivel = "dpto_edad", id_time = 1, nivel_value = glue("{as.numeric(id_d)}_{edad}"),
         nivel_label = gsub(" ", "_", nivel_label)) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

def_dpto19 <- read_csv(glue("{datos}/homicidios_edad_dpto_2019-2020.csv"))

dpto <- rbind (dpto, def_dpto19)

write_csv(dpto, glue("{datos}/base_homicidios_edad_dpto_2010-2020.csv"))

rm(def_dpto19, def_dpto, dpto)

#-------------------------------------------------------#
# 3. Municipal ----
#-------------------------------------------------------#

cod_dpto <- read_csv("Data/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv") %>%
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
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "cuadro5_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "CUADRO5-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Mortalidad_no_fetal/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 2) %>% janitor::clean_names() %>%
      rename(value = na_3) %>%
      rename(dpto = na, gen_1 = hombres, gen_2 = mujeres) %>% fill(dpto,.direction = "down") %>%
      mutate(time = y, cod_enf = as.character(substr(na_2, 1, 3)),
             value_label = "Agresiones (homicidios)", 
             id_time = 1, id_data = 19, id_d = as.character(substr(dpto, 1, 5))) %>%
      filter(cod_enf == "512", dpto != "TOTAL")
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

#homicidios----

dpto <- def_dpto %>% mutate(nivel_value = glue("{id_d}"), id_nivel = "mpio", variable = "homicidios") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_dpto19 <- read_csv(glue("{datos}/base_homicidios_mpio_2019-2020.csv"))

nac_dpto <- rbind (dpto, def_dpto19)

write_csv(nac_dpto, glue("{datos}/base_homicidios_mpio_2010-2020.csv"))

rm(cod_dpto, def_dpto19, dpto, nac_dpto)

#homicidios - género----

dpto <- def_dpto %>% pivot_longer(cols = starts_with("gen_"), names_to = "gen", values_to = "homicidios") %>%
  mutate(id_data = 19, variable = "homicidios", gen = as.numeric(gsub("gen_", "", gen)),
         value_label = "Agresiones (homicidios)",
         id_nivel = "mpio_gen", id_time = 1, nivel_value = glue("{id_d}_{gen}"), value = homicidios) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_dpto19 <- read_csv(glue("{datos}/base_homicidios_genero_mpio_2019-2020.csv"))

dpto <- rbind (dpto, def_dpto19)

write_csv(dpto, glue("{datos}/base_homicidios_genero_mpio_2010-2020.csv"))

rm(def_dpto19, dpto)

# homicidios - edad----

cols = c(3:39)
def_dpto[, cols] = apply(def_dpto[, cols], 2, function(x) as.numeric(as.character(x)))
def_dpto[is.na(def_dpto)] <- 0

def_dpto <- def_dpto %>% mutate(edad_1 = rowSums(.[13:15]), edad_2 = rowSums(.[16:18]),
                                edad_3 = rowSums(.[19:21]), edad_4 = rowSums(.[22:27]) + rowSums(.[34:36])) %>% 
  select(dpto, edad_1, edad_2, edad_3, edad_4, time, id_d) %>%
  pivot_longer(cols = starts_with("edad_"), names_to = "edad", values_to = "homicidios") %>%
  mutate(edad = as.numeric(gsub("edad_", "", edad)))

def_dpto <- def_dpto %>% mutate(id_data = 19, variable = "homicidios",
                                value_label = "Agresiones (homicidios)",
                                id_nivel = "mpio_edad", id_time = 1, nivel_value = glue("{id_d}_{edad}"), value = homicidios) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_dpto19 <- read_csv(glue("{datos}/base_homicidios_edad_mpio_2019-2020.csv"))

def_dpto <- rbind (def_dpto, def_dpto19)

write_csv(def_dpto, glue("{datos}/base_homicidios_edad_mpio_2010-2020.csv"))

rm(def_dpto19)
