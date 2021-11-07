#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 21 octubre, 2021
# Procesamiento de datos de asistencia a educacion superior
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

datos_ori <- "Data/Encuesta_Calidad_de_Vida/Input/microdatos"
datos <- "Data/Encuesta_Calidad_de_Vida/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos (total) ----
#-------------------------------------------------------#

# Asistencia escolar 13-17 anios (secundaria)
educ_ori <- read_csv(glue("{datos_ori}/educacion.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, edu_sup_18_28, totalp1, edu_sup_18_28_perc) %>%
  mutate(edu_sup = edu_sup_18_28/totalp1, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Asistencia a educacion superior por dpto y year
educ_dpto <- educ %>%
  drop_na(cod_dpto, edu_sup_18_28) %>%
  group_by(cod_dpto, year) %>%
  summarise(edu_sup_18_28 = sum(edu_sup_18_28), total = sum(totalp1)) %>%
  ungroup() %>%
  mutate(educ_sup = 100*(edu_sup_18_28/total))

table(educ_dpto$year)

# Organizar base datos
educ_dpto <- educ_dpto %>% 
  mutate(id_data = 2, variable = "educ_superior", value_label = "Jóvenes de 18 a 28 años han asistido a una institución de educación superior (%)", 
         id_nivel = "dpto", id_time = 1, time = year, value = educ_sup, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value, value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(educ_dpto, glue("{datos}/base_educ_superior_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabeceras, centros poblados y rural disperso
#--------------------------#

# Asistencia a educacion superior por zona y year
educ_zona <- educ %>%
  drop_na(zona, edu_sup_18_28) %>%
  group_by(zona, year) %>%
  summarise(edu_sup_18_28 = sum(edu_sup_18_28), total = sum(totalp1)) %>%
  ungroup() %>%
  mutate(educ_sup = 100*(edu_sup_18_28/total)) %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

# Organizar base datos
educ_zona <- educ_zona %>% 
  mutate(id_data = 2, variable = "educ_superior", value_label = "Jóvenes de 18 a 28 años han asistido a una institución de educación superior (%)", 
         id_nivel = "zona", id_time = 1, time = year, value = educ_sup, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value, value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(educ_zona, glue("{datos}/base_educ_superior_zona_2010-2020.csv"))


