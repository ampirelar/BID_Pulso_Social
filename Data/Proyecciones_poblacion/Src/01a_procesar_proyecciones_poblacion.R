#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 14 sept, 2021
# Procesamiento de datos Proyecciones de poblacion
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Proyecciones_poblacion/Input"
datos <- "Data/Proyecciones_poblacion/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#

# fun_pob: Organiza informacion de proyecciones de poblacion
fun_pob <- function(data, edad = NULL, genero = NULL){
  
  # Limpiamos NA y organizamos nombres
  data <- data %>% drop_na(`...2`) %>% janitor::row_to_names(row_number = 1)
  names(data) <- tolower(names(data))
  
  if(!is.null(edad)){
    data <- data %>% dplyr::rename(cod_mpio = dpmp, year = año, area = `área geográfica`) 
  } 
  if(!is.null(genero)){
    data <- data %>% 
      dplyr::rename(cod_mpio = dpmp, hombres = `total hombres`, mujeres = `total mujeres`, year = año) %>% 
      dplyr::select(cod_mpio, year, hombres, mujeres) %>%
      pivot_longer(cols = c("hombres", "mujeres"), names_to = "genero", values_to = "poblacion")
  } 
  
  if(is.null(edad) & is.null(genero)){
    data <- data %>% dplyr::rename(cod_mpio = dpmp, poblacion = total, year = año) %>% 
      dplyr::select(cod_mpio, year, poblacion)
  }
  
  return(data)
}

# fun_edad: Organiza poblacion por edades, municipio y anio
fun_edad <- function(data, genero = NULL){
  
  # Reshape wide to long
  data <- data %>% dplyr::filter(area == "Total") %>%
    dplyr::select(cod_mpio, year, starts_with(c("hombres", "mujeres"))) %>%
    pivot_longer(cols = starts_with(c("hombres", "mujeres")), names_to = "edad", values_to = "poblacion")
  
  # Agrupamos poblacion por edades 
  if(!is.null(genero)){
    
    # Limpiamos edades y genero
    data <- data %>% mutate(genero = gsub("_.*", "", edad))
    data$edad <- gsub("hombres_", "", data$edad) 
    data$edad <- gsub("mujeres_", "", data$edad) 
    data$edad <- gsub("y más", "", data$edad)
    data$edad <- as.numeric(data$edad)
    
    data <- data %>%
      mutate(cod_mpio = as.numeric(cod_mpio), year = as.numeric(year), poblacion = as.numeric(poblacion)) 
    
  } else {
    
    # Limpiamos edades
    data$edad <- gsub("hombres_", "", data$edad) 
    data$edad <- gsub("mujeres_", "", data$edad) 
    data$edad <- gsub("y más", "", data$edad)
    data$edad <- as.numeric(data$edad)
    
    data <- data %>% 
      mutate_all(as.numeric) %>%
      group_by(cod_mpio, year, edad) %>%
      summarise(poblacion = sum(poblacion)) %>%
      ungroup()
  }

  return(data)
}

#-------------------------------------------------------#
# 1. Panel poblacion 1985-2035 ----
# Usamos proyecciones de poblacion del DANE con base al censo 2018
#-------------------------------------------------------#

#--------------------------#
# A. Total ----
#--------------------------#

# Poblacion 1985-2017
pob1 <- readxl::read_xlsx(glue("{datos_ori}/anexo-sexo-proyecciones-poblacion-Municipal_Sexo_1985-2017.xlsx")) 
pob1 <- fun_pob(pob1)

# Poblacion 2018-2035
pob2 <- readxl::read_xlsx(glue("{datos_ori}/anexo-proyecciones-poblacion-Municipal_Sexo_2018-2035.xlsx"))
pob2 <- fun_pob(pob2)

# Unimos proyecciones anuales y organizamos
panel_pob <- bind_rows(pob1, pob2) %>% 
  arrange(cod_mpio, year) %>% 
  mutate_all(as.numeric) %>%
  mutate(id_data = 17, variable = "poblacion", id_nivel = "mpio", nivel_value = cod_mpio, 
         id_time = 1, time = year, value_label = "Población", value = poblacion) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

table(panel_pob$time)

# Exportar base
write_csv(panel_pob, glue("{datos}/base_proyecciones_poblacion_mpio_1985-2035.csv"))
rm(panel_pob, pob1, pob2)

#--------------------------#
# B. Por edad ----
#--------------------------#

# Poblacion 2005-2017
pob1 <- readxl::read_xlsx(glue("{datos_ori}/anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")) 
pob1 <- fun_pob(pob1, edad = 1)
pob1 <- fun_edad(pob1)

# Poblacion 2018-2026
pob2 <- readxl::read_xlsx(glue("{datos_ori}/anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")) 
pob2 <- fun_pob(pob2, edad = 1)
pob2 <- fun_edad(pob2)

# Unimos proyecciones anuales y organizamos
panel_pob <- bind_rows(pob1, pob2) %>% 
  arrange(cod_mpio, year, edad) %>%
  mutate(id_data = 17, variable = "poblacion", id_nivel = "mpio_edad", 
         nivel_value = glue("{cod_mpio}_{edad}"), 
         id_time = 1, time = year, value_label = "Población", value = poblacion) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

table(panel_pob$time)
colSums(is.na(panel_pob))

# Exportar base
write_csv(panel_pob, glue("{datos}/base_proyecciones_poblacion_mpio_edad_2005-2026.csv"))
rm(panel_pob, pob1, pob2)

#--------------------------#
# C. Genero ----
#--------------------------#

# Poblacion 1985-2017
pob1 <- readxl::read_xlsx(glue("{datos_ori}/anexo-sexo-proyecciones-poblacion-Municipal_Sexo_1985-2017.xlsx")) 
pob1 <- fun_pob(pob1, genero = 1)

# Poblacion 2018-2035
pob2 <- readxl::read_xlsx(glue("{datos_ori}/anexo-proyecciones-poblacion-Municipal_Sexo_2018-2035.xlsx"))
pob2 <- fun_pob(pob2, genero = 1)

# Unimos proyecciones anuales y organizamos
panel_pob <- bind_rows(pob1, pob2) %>% 
  arrange(cod_mpio, year) %>% 
  mutate(genero = ifelse(genero == "hombres", "1", "2")) %>%
  mutate_all(as.numeric) %>%
  mutate(id_data = 17, variable = "poblacion", id_nivel = "mpio_gen", nivel_value = glue("{cod_mpio}_{genero}"), 
         id_time = 1, time = year, value_label = "Población", value = poblacion) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) 

table(panel_pob$time)

# Exportar base
write_csv(panel_pob, glue("{datos}/base_proyecciones_poblacion_mpio_gen_1985-2035.csv"))
rm(panel_pob, pob1, pob2)

#--------------------------#
# D. Edad genero ----
#--------------------------#

# Poblacion 2005-2017
pob1 <- readxl::read_xlsx(glue("{datos_ori}/anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")) 
pob1 <- fun_pob(pob1, edad = 1)
pob1 <- fun_edad(pob1, genero = 1)

# Poblacion 2018-2026
pob2 <- readxl::read_xlsx(glue("{datos_ori}/anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")) 
pob2 <- fun_pob(pob2, edad = 1)
pob2 <- fun_edad(pob2, genero = 1)

# Unimos proyecciones anuales y organizamos
panel_pob <- bind_rows(pob1, pob2) %>% 
  arrange(cod_mpio, year, genero, edad) %>%
  mutate(genero = ifelse(genero == "hombres", "1", "2"),
         id_data = 17, variable = "poblacion", id_nivel = "mpio_gen_edad", 
         nivel_value = glue("{cod_mpio}_{genero}_{edad}"), 
         id_time = 1, time = year, value_label = "Población", value = poblacion) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

head(panel_pob)
table(panel_pob$time)
colSums(is.na(panel_pob))

# Exportar base
write_csv(panel_pob, glue("{datos}/base_proyecciones_poblacion_mpio_gen_edad_2005-2026.csv"))
rm(panel_pob, pob1, pob2)
