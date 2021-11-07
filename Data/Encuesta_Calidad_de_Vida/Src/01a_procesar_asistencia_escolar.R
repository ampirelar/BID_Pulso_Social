#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 octubre, 2021
# Procesamiento de datos de asistencia escolar total y por quintil de ingreso
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

# Asistencia a primaria
educ_ori <- read_csv(glue("{datos_ori}/educacion.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

table(educ_ori$year, educ_ori$zona)
table(educ_ori$cod_dpto, educ_ori$year)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, edu_primaria, asistencia_edu_prim, 
                totalp2, asistencia_edu_prim_perc) %>%
  mutate(asistencia_esc = asistencia_edu_prim/totalp2, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Asistencia escolar por departamento, year y zona 
prim <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(edu_primaria = sum(edu_primaria), asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(totalp2)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(prim$year, prim$zona)
table(prim$cod_dpto, prim$year)
colSums(is.na(prim))

# Agrupamos zonas por dpto
prim_dpto <- prim %>%
  group_by(cod_dpto, year) %>%
  summarise(edu_primaria = sum(edu_primaria), asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(asistencia_edu_prim/total))

table(prim_dpto$cod_dpto, prim_dpto$year)

# Organizar base datos
prim_dpto <- prim_dpto %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_primaria", value_label = "Asistencia escolar a educación primaria (%)", 
         id_nivel = "dpto", id_time = 1, time = year, value = asistencia_esc, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

table(prim_dpto$time)

# Exportar base
write_csv(prim_dpto, glue("{datos}/base_asistencia_escolar_primaria_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Asistencia escolar por zonas y year
zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(edu_primaria = sum(edu_primaria), asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(totalp2)) %>%
  ungroup()

table(zonas$year, zonas$zona)

# Agrupamos dptos por zona
prim_zona <- zonas %>%
  group_by(zona, year) %>%
  summarise(edu_primaria = sum(edu_primaria), asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(asistencia_edu_prim/total))

table(prim_zona$zona, prim_zona$year)

# Organizar base datos
prim_zona <- prim_zona %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_primaria", value_label = "Asistencia escolar a educación primaria (%)", 
         id_nivel = "zona", id_time = 1, time = year, value = asistencia_esc, 
         nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(prim_zona, glue("{datos}/base_asistencia_escolar_primaria_zonas_2010-2020.csv"))

rm(educ, educ_ori, zonas, prim_zona, prim, prim_dpto)

#-------------------------------------------------------#
# 2. Organizar datos (quintil) ----
#-------------------------------------------------------#

# Asistencia a primaria por quintil de ingreso
educ_ori <- read_csv(glue("{datos_ori}/educacion_quintiles.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

table(educ_ori$year, educ_ori$zona)
table(educ_ori$cod_dpto, educ_ori$year)
table(educ_ori$quin, educ_ori$year)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, quin, asistencia_edu_prim, totalp2, asistencia_edu_prim_perc) %>%
  mutate(asistencia_esc = asistencia_edu_prim/totalp2, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Asistencia escolar por departamento, year, quintil y zona 
prim <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona, quin) %>%
  summarise(asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(totalp2)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(prim$year, prim$zona)
table(prim$cod_dpto, prim$year)
colSums(is.na(prim))

# Agrupamos zonas por dpto
prim_dpto <- prim %>%
  group_by(cod_dpto, year, quin) %>%
  summarise(asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(asistencia_edu_prim/total),
         asistencia_esc = ifelse(asistencia_edu_prim + total == 0, NA, asistencia_esc))

# Organizar base datos
prim_dpto <- prim_dpto %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_primaria", 
         value_label = glue("Asistencia escolar a educación primaria por quintil de ingreso (%) - quintil {quin}"), 
         id_nivel = "dpto", id_time = 1, time = year, value = asistencia_esc, 
         nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

table(prim_dpto$time)

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Asistencia escolar por zonas y year
zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona, quin) %>%
  summarise(asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(totalp2)) %>%
  ungroup()

# Agrupamos dptos por zona
prim_zona <- zonas %>%
  group_by(zona, year, quin) %>%
  summarise(asistencia_edu_prim = sum(asistencia_edu_prim), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(asistencia_edu_prim/total),
         asistencia_esc = ifelse(asistencia_edu_prim + total == 0, NA, asistencia_esc))

table(prim_zona$zona, prim_zona$year)

# Organizar base datos
prim_zona <- prim_zona %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_primaria", 
         value_label = glue("Asistencia escolar a educación primaria por quintil de ingreso (%) - quintil {quin}"), 
         id_nivel = "zona", id_time = 1, time = year, value = asistencia_esc, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

#--------------------------#
# C. Exportar ----
#--------------------------#

# Guardamos una base por quintil y nivel territorial
lapply(unique(prim_dpto$value_label), function(x){
  
  print(x)
  quintil <- gsub(".*- quintil ", "", x)
  quintil <- tolower(quintil) %>% str_trim()
  print(quintil)
  dptos_df <- prim_dpto %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{quintil}"))
  zonas_df <- prim_zona %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{quintil}"))
  
  # Exportar base
  write_csv(dptos_df, glue("{datos}/base_asistencia_escolar_primaria_dpto_quintil_{quintil}_2010-2020.csv"))
  write_csv(zonas_df, glue("{datos}/base_asistencia_escolar_primaria_zonas_quintil_{quintil}_2010-2020.csv"))
  
})
