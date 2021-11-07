#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 octubre, 2021
# Procesamiento de datos de asistencia escolar de menores de 5 anios total y por quintil de ingreso
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

# Asistencia escolar menores 5 anios
educ_ori <- read_csv(glue("{datos_ori}/menores5.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, menor5_estudia, total, menor5_estudia_p) %>%
  mutate(asistencia_esc = menor5_estudia/total, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Asistencia escolar por departamento, year y zona 
esc <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(esc$year, esc$zona)
table(esc$cod_dpto, esc$year)
colSums(is.na(esc))

# Agrupamos zonas por dpto
esc_dpto <- esc %>%
  group_by(cod_dpto, year) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(menor5_estudia/total))

table(esc_dpto$cod_dpto, esc_dpto$year)
colSums(is.na(esc_dpto))

# Organizar base datos
esc_dpto <- esc_dpto %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_menor5", value_label = "Asistencia escolar de menores de 5 años (%)", 
         id_nivel = "dpto", id_time = 1, time = year, value = asistencia_esc, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

table(esc_dpto$time)

# Exportar base
write_csv(esc_dpto, glue("{datos}/base_asistencia_escolar_menor5_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Asistencia escolar por zonas y year
zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup()

# Agrupamos dptos por zona
esc_zona <- zonas %>%
  group_by(zona, year) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(menor5_estudia/total))

# Organizar base datos
esc_zona <- esc_zona %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_menor5", 
         value_label = "Asistencia escolar de menores de 5 años (%)", 
         id_nivel = "zona", id_time = 1, time = year, value = asistencia_esc, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(esc_zona, glue("{datos}/base_asistencia_escolar_menor5_zonas_2010-2020.csv"))

rm(educ, educ_ori, esc, esc_dpto, esc_zona, zonas)

#-------------------------------------------------------#
# 2. Organizar datos (quintiles) ----
#-------------------------------------------------------#

# Asistencia escolar de menores de 5 anios por quintil de ingreso
quint_ori <- read_csv(glue("{datos_ori}/menores5_quintiles.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
quint <- quint_ori %>%
  dplyr::select(zona, cod_dpto, year, quin, menor5_estudia, total, menor5_estudia_p) %>%
  mutate(asistencia_esc = menor5_estudia/total, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Asistencia escolar por departamento, year y zona 
esc <- quint %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona, quin) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(esc$year, esc$zona)
table(esc$cod_dpto, esc$year)
colSums(is.na(esc))

# Agrupamos zonas por dpto
esc_dpto <- esc %>%
  group_by(cod_dpto, year, quin) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(menor5_estudia/total))

table(esc_dpto$cod_dpto, esc_dpto$year)
colSums(is.na(esc_dpto))

# Organizar base datos
esc_dpto <- esc_dpto %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_menor5", 
         value_label = glue("Asistencia escolar de menores de 5 años por quintil de ingreso (%) - quintil {quin}"), 
         id_nivel = "dpto", id_time = 1, time = year, value = asistencia_esc, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
# write_csv(esc_dpto, glue("{datos}/base_asistencia_escolar_menor5_dpto_quintil_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Asistencia escolar por zonas, quintil y year
zonas <- quint %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona, quin) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup()

# Agrupamos dptos por zona
esc_zona <- zonas %>%
  group_by(zona, year, quin) %>%
  summarise(menor5_estudia = sum(menor5_estudia), total = sum(total)) %>%
  ungroup() %>%
  mutate(asistencia_esc = 100*(menor5_estudia/total))

# Organizar base datos
esc_zona <- esc_zona %>% 
  mutate(id_data = 2, variable = "asistencia_escolar_menor5", 
         value_label = glue("Asistencia escolar de menores de 5 años por quintil de ingreso (%) - quintil {quin}"), 
         id_nivel = "zona", id_time = 1, time = year, value = asistencia_esc, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
# write_csv(esc_zona, glue("{datos}/base_asistencia_escolar_menor5_zonas_quintil_2010-2020.csv"))

#--------------------------#
# C. Exportar ----
#--------------------------#

# Guardamos una base por quintil y nivel territorial
lapply(unique(esc_dpto$value_label), function(x){
  
  print(x)
  quintil <- gsub(".*- quintil ", "", x)
  quintil <- tolower(quintil) %>% str_trim()
  print(quintil)
  dptos_df <- esc_dpto %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{quintil}"))
  zonas_df <- esc_zona %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{quintil}"))
  
  # Exportar base
  write_csv(dptos_df, glue("{datos}/base_asistencia_escolar_menor5_dpto_quintil_{quintil}_2010-2020.csv"))
  write_csv(zonas_df, glue("{datos}/base_asistencia_escolar_menor5_zonas_quintil_{quintil}_2010-2020.csv"))
  
})
