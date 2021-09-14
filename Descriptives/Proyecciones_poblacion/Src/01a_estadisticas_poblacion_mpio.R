#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 14 sept, 2021
# Estadisticas descriptivas Proyecciones poblacion
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr)
# .rs.restartR()

# Abrir funciones de graficas
source("Descriptives/Herramientas/Src/01a_funciones_graficas.R")

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Proyecciones_poblacion/Output"
datos <- "Descriptives/Proyecciones_poblacion/Input"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Estructura de la población por grupos de edad de 2006 a 2016
# Filtramos datos y creamos quinquenios de edad

pob <- read_csv(glue("{datos_ori}/base_proyecciones_poblacion_mpio_edad_2005-2026.csv")) %>%
  filter(time >= 2010 & time <= 2021) %>%
  mutate(edad = as.numeric(gsub(".*_", "", nivel_value)), cat_pob = "")

pob$cat_pob[pob$edad <= 4] <- "0-4"
pob$cat_pob[pob$edad > 4 & pob$edad <= 9] <- "5-9" 
pob$cat_pob[pob$edad > 9 & pob$edad <= 14] <- "10-14"
pob$cat_pob[pob$edad > 14 & pob$edad <= 19] <- "15-19" 
pob$cat_pob[pob$edad > 19 & pob$edad <= 24] <- "20-24"
pob$cat_pob[pob$edad > 24 & pob$edad <= 29] <- "25-29" 
pob$cat_pob[pob$edad > 29 & pob$edad <= 34] <- "30-34"
pob$cat_pob[pob$edad > 34 & pob$edad <= 39] <- "35-39"
pob$cat_pob[pob$edad > 39 & pob$edad <= 44] <- "40-44"
pob$cat_pob[pob$edad > 44 & pob$edad <= 49] <- "45-49"
pob$cat_pob[pob$edad > 49 & pob$edad <= 54] <- "50-54"
pob$cat_pob[pob$edad > 54 & pob$edad <= 59] <- "55-59"
pob$cat_pob[pob$edad > 59 & pob$edad <= 64] <- "60-64"
pob$cat_pob[pob$edad > 64 & pob$edad <= 69] <- "65-69"
pob$cat_pob[pob$edad > 69 & pob$edad <= 74] <- "70-74"
pob$cat_pob[pob$edad > 74 & pob$edad <= 79] <- "75-79"
pob$cat_pob[pob$edad > 79 & pob$edad <= 84] <- "80-84"
pob$cat_pob[pob$edad >= 85 & pob$edad] <- "85 y más"

# Colapsamos poblacion a nivel de categoria de edad
panel_edad <- pob %>% 
  mutate(nivel_value = as.numeric(gsub("_.*", "", nivel_value))) %>%
  group_by(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, cat_pob) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(nivel_pob = cat_pob)

# Exportamos
write_csv(panel_edad, glue("{datos}/estadisticas_poblacion_mpio_edad_2010-2021.csv"))
rm(panel_edad, pob)







