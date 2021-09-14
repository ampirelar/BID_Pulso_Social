#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 14 sept, 2021
# Estadisticas descriptivas Poblacion por clases sociales
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

datos_ori <- "Data/Pobreza_monetaria/Output"
datos <- "Descriptives/Pobreza_monetaria/Input"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Calculamos el % de la poblacion en clase pobre y clase media para 2012-2020

# Abrir datos (Nacionales)
data_clas <- read_csv(glue("{datos_ori}/base_clases_sociales_col_2012-2020.csv")) 

# Poblacion nacional anual
pob_year <- data_clas %>% 
  group_by(time) %>%
  summarise(value_all = sum(value)) %>%
  ungroup()

# Participacion por clase social anual
data_clas <- data_clas %>% 
  left_join(pob_year, by = "time") %>%
  mutate(part = 100*(value/value_all))

# Exportamos
write_csv(data_clas, glue("{datos}/estadisticas_clases_sociales_col_2012-2020.csv"))



