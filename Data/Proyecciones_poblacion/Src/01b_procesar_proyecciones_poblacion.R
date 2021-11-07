#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 13 oct, 2021
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

# Poblacion 2018-2026
pob2 <- readxl::read_xlsx(glue("{datos_ori}/anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")) 
pob2 <- fun_pob(pob2, edad = 1)
pob2 <- fun_edad(pob2)
pob2 <- pob2 %>% filter(year == "2018" | year == "2026") 

# Codigos de departamento y municipio
cod_dpto <- read_csv("Data/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv") %>%
  select(cod_dpto, cod_mpio) 
pob2 <- pob2 %>% left_join(cod_dpto, by = "cod_mpio")
pob2 <- pob2 %>% group_by(cod_dpto, year, edad) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup()
pob1 <- pob2 %>% group_by(cod_dpto, year) %>%
  summarise(poblaciont = sum(poblacion)) %>%
  ungroup()

pob2 <- pob2 %>% mutate(rango = ifelse(edad < 14, "infancia", 
                                       ifelse(edad > 13 & edad < 29, "juventud",
                                              ifelse(edad > 28 & edad < 65, "adultez", "vejez")))) %>%
  group_by(cod_dpto, year, rango) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% left_join(pob1, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 17, variable = "poblacion_etaria", id_nivel = "dpto_grupo", 
         nivel_value = glue("{cod_dpto}_{rango}"), 
         id_time = 1, time = year, value_label = "Población por grupo etario", value = (poblacion/poblaciont)*100) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(pob2, glue("{datos}/base_proyecciones_poblacion_etaria_dpto_2018-2026.csv"))





