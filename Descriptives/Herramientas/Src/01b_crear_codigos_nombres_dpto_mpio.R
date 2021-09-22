#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 22 sept, 2021
# Guardar codigos de departamento y municipio
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

datos_ori <- "Data/Proyecciones_poblacion/Input"
datos <- "Descriptives/Herramientas/Input"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Lista de codigos de departamento y municipio ----
#-------------------------------------------------------#

# Datos de poblacion tienen todos los municipios y departamentos
pob1 <- readxl::read_xlsx(glue("{datos_ori}/anexo-sexo-proyecciones-poblacion-Municipal_Sexo_1985-2017.xlsx")) %>%
  drop_na(`...2`) %>% row_to_names(row_number = 1) %>% clean_names() %>%
  distinct(dp, dpmp, dpnom, municipio) %>%
  rename(cod_dpto = dp, cod_mpio = dpmp, nom_dpto = dpnom, nom_mpio = municipio) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio))

# Exportamos csv 
write_csv(pob1, glue("{datos}/base_nombres_codigos_dpto_mpio.csv"))
