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
fun_pob <- function(data, edad = NULL){

  # Limpiamos NA y organizamos nombres
  data <- data %>% drop_na(`...2`) %>% janitor::row_to_names(row_number = 1)
  names(data) <- tolower(names(data))
  
  if(!is.null(edad)){
    data <- data %>% dplyr::rename(cod_mpio = dpmp, year = año, area = `área geográfica`) 
  } else {
    data <- data %>% dplyr::rename(cod_mpio = dpmp, poblacion = total, year = año) %>% 
      dplyr::select(cod_mpio, year, poblacion)
  }
  return(data)
}

# fun_edad: Organiza poblacion por edades, municipio y anio
fun_edad <- function(data){
  
  # Reshape wide to long
  data <- data %>% dplyr::filter(area == "Total") %>%
    dplyr::select(cod_mpio, year, starts_with(c("hombres", "mujeres"))) %>%
    pivot_longer(cols = starts_with(c("hombres", "mujeres")), names_to = "edad", values_to = "poblacion")
  
  # Limpiamos edades
  data$edad <- gsub("hombres_", "", data$edad) 
  data$edad <- gsub("mujeres_", "", data$edad) 
  data$edad <- gsub("y más", "", data$edad)
  data$edad <- as.numeric(data$edad)
  
  # Agrupamos poblacion por edades 
  data <- data %>% 
    mutate_all(as.numeric) %>%
    group_by(cod_mpio, year, edad) %>%
    summarise(poblacion = sum(poblacion)) %>%
    ungroup()
  
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


table(panel_pob$year)

# Guardamos panel
saveRDS(panel_pob, glue("{datos}/panel_poblacion_mpios_1985-2035.rds"))
rm(pob1, pob2)