#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 octubre, 2021
# Procesamiento de datos de educacion promedio entre indigenas y no indigenas
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
# 1. Anios educacion ----
#-------------------------------------------------------#

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
ind <- read_csv(glue("{datos_ori}/Pobl_indigena.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento, anios_educ = grado_aprovado) %>%
  select(cod_dpto, year, zona, pobl_indigena, anios_educ) %>%
  mutate(zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Rezago escolar por departamento, year y zona 
ind_educ <- ind %>%
  drop_na(cod_dpto, anios_educ) %>%
  group_by(cod_dpto, year, zona, pobl_indigena) %>%
  summarise(anios_educ = mean(anios_educ, na.rm = T)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(ind_educ$year, ind_educ$zona)
table(ind_educ$cod_dpto, ind_educ$year)
colSums(is.na(ind_educ))

# Agrupamos zonas por dpto
ind_dpto <- ind_educ %>%
  group_by(cod_dpto, year, pobl_indigena) %>%
  summarise(anios_educ = mean(anios_educ, na.rm = T)) %>%
  ungroup() %>%
  mutate(pobl_indigena = as.character(pobl_indigena), pobl_indigena = ifelse(pobl_indigena == "1", "ind", "no_ind")) %>%
  pivot_wider(names_from = "pobl_indigena", values_from = "anios_educ") %>%
  mutate(dif_educ = ind - no_ind)

# Organizar base datos
ind_dpto <- ind_dpto %>% 
  mutate(id_data = 2, variable = "dif_educ_ind", value_label = "Diferencias en los años de educación promedio entre indígenas y no indígenas", 
         id_nivel = "dpto", id_time = 1, time = year, value = dif_educ, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value, value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(ind_dpto, glue("{datos}/base_dif_educ_ind_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Rezago escolar por departamento, year y zona 
ind_zona <- ind %>%
  drop_na(anios_educ) %>%
  group_by(year, zona, pobl_indigena) %>%
  summarise(anios_educ = mean(anios_educ, na.rm = T)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

# Agrupamos zonas por dpto
ind_zona <- ind_zona %>%
  group_by(zona, year, pobl_indigena) %>%
  summarise(anios_educ = mean(anios_educ, na.rm = T)) %>%
  ungroup() %>%
  mutate(pobl_indigena = as.character(pobl_indigena), pobl_indigena = ifelse(pobl_indigena == "1", "ind", "no_ind")) %>%
  pivot_wider(names_from = "pobl_indigena", values_from = "anios_educ") %>%
  mutate(dif_educ = ind - no_ind)

# Organizar base datos
ind_zona <- ind_zona %>% 
  mutate(id_data = 2, variable = "dif_educ_ind", 
         value_label = "Diferencias en los años de educación promedio entre indígenas y no indígenas", 
         id_nivel = "zona", id_time = 1, time = year, value = dif_educ, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value, value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(ind_zona, glue("{datos}/base_dif_educ_ind_zonas_2010-2020.csv"))

#-------------------------------------------------------#
# 2. Graduacion de secundaria ----
#-------------------------------------------------------#

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
ind <- read_csv(glue("{datos_ori}/Pobl_indigena.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento) %>%
  select(cod_dpto, year, zona, pobl_indigena, edu_secundaria, total) %>%
  mutate(zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Personas con educacion secundaria por departamento, year y zona 
ind_educ <- ind %>%
  drop_na(cod_dpto, edu_secundaria) %>%
  group_by(cod_dpto, year, zona, pobl_indigena) %>%
  summarise(edu_secundaria = sum(edu_secundaria, na.rm = T), total = sum(total, na.rm = T)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

# Agrupamos zonas por dpto
ind_dpto <- ind_educ %>%
  group_by(cod_dpto, year, pobl_indigena) %>%
  summarise(edu_secundaria = sum(edu_secundaria, na.rm = T), total = sum(total, na.rm = T)) %>%
  ungroup() %>%
  mutate(pobl_indigena = as.character(pobl_indigena), pobl_indigena = ifelse(pobl_indigena == "1", "ind", "no_ind"),
         part_sec = 100*(edu_secundaria/total)) %>%
  select(-c(edu_secundaria, total)) %>%
  pivot_wider(names_from = "pobl_indigena", values_from = "part_sec") %>%
  mutate(dif_sec = ind - no_ind)

# Organizar base datos
ind_dpto <- ind_dpto %>% 
  mutate(id_data = 2, variable = "dif_sec_ind", value_label = "Diferencias en la terminación de educación secundaria entre indígenas y no indígenas", 
         id_nivel = "dpto", id_time = 1, time = year, value = dif_sec, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value, value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(ind_dpto, glue("{datos}/base_dif_sec_ind_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Personas con educacion secundaria por year y zona 
ind_zonas <- ind %>%
  drop_na(zona, edu_secundaria) %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(zona, year, pobl_indigena) %>%
  summarise(edu_secundaria = sum(edu_secundaria, na.rm = T), total = sum(total, na.rm = T)) %>%
  ungroup() %>%
  mutate(pobl_indigena = as.character(pobl_indigena), pobl_indigena = ifelse(pobl_indigena == "1", "ind", "no_ind"),
         part_sec = 100*(edu_secundaria/total)) %>%
  select(-c(edu_secundaria, total)) %>%
  pivot_wider(names_from = "pobl_indigena", values_from = "part_sec") %>%
  mutate(dif_sec = ind - no_ind)

# Organizar base datos
ind_zonas <- ind_zonas %>% 
  mutate(id_data = 2, variable = "dif_sec_ind", value_label = "Diferencias en la terminación de educación secundaria entre indígenas y no indígenas", 
         id_nivel = "zona", id_time = 1, time = year, value = dif_sec, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value, value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(ind_zonas, glue("{datos}/base_dif_sec_ind_zonas_2010-2020.csv"))
