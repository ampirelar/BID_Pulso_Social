#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 octubre, 2021
# Procesamiento de datos de rezago escolar 13-17 años total y por quintil
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
  dplyr::select(zona, cod_dpto, year, rezago_secund, totalp5, rezago_secund_perc) %>%
  mutate(rezago_esc = rezago_secund/totalp5, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Rezago escolar por departamento, year y zona 
esc <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(rezago_secund = sum(rezago_secund), totalp5 = sum(totalp5)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

# Agrupamos zonas por dpto
esc_dpto <- esc %>%
  group_by(cod_dpto, year) %>%
  summarise(rezago_secund = sum(rezago_secund), total = sum(totalp5)) %>%
  ungroup() %>%
  mutate(rezago_esc = 100*(rezago_secund/total))

# Organizar base datos
esc_dpto <- esc_dpto %>% 
  mutate(id_data = 2, variable = "rezago_escolar_13-17", value_label = "Rezago escolar de 13 a 17 años (%)", 
         id_nivel = "dpto", id_time = 1, time = year, value = rezago_esc, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(esc_dpto, glue("{datos}/base_rezago_escolar_13-17_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Asistencia escolar por zonas y year
zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(rezago_secund = sum(rezago_secund), totalp5 = sum(totalp5)) %>%
  ungroup()

# Agrupamos dptos por zona
esc_zona <- zonas %>%
  group_by(zona, year) %>%
  summarise(rezago_secund = sum(rezago_secund), total = sum(totalp5)) %>%
  ungroup() %>%
  mutate(rezago_esc = 100*(rezago_secund/total))

# Organizar base datos
esc_zona <- esc_zona %>% 
  mutate(id_data = 2, variable = "rezago_escolar_13-17", value_label = "Rezago escolar de 13 a 17 años (%)", 
         id_nivel = "zona", id_time = 1, time = year, value = rezago_esc, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
write_csv(esc_zona, glue("{datos}/base_rezago_escolar_13-17_zonas_2010-2020.csv"))

rm(educ, educ_ori, esc, esc_dpto, esc_zona, zonas)

#-------------------------------------------------------#
# 2. Organizar datos (quintiles) ----
#-------------------------------------------------------#

# Asistencia a primaria por quintil de ingreso
educ_ori <- read_csv(glue("{datos_ori}/educacion_quintiles.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, quin, rezago_secund, totalp5, rezago_secund_perc) %>%
  mutate(rezago_esc = rezago_secund/totalp5, zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Rezago escolar por departamento, year, quintil y zona 
sec <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona, quin) %>%
  summarise(rezago_secund = sum(rezago_secund), totalp5 = sum(totalp5)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

# Agrupamos zonas por dpto
sec_dpto <- sec %>%
  group_by(cod_dpto, year, quin) %>%
  summarise(rezago_secund = sum(rezago_secund), total = sum(totalp5)) %>%
  ungroup() %>%
  mutate(rezago_esc = 100*(rezago_secund/total),
         rezago_esc = ifelse(rezago_secund + total == 0, NA, rezago_esc))

# Organizar base datos
sec_dpto <- sec_dpto %>% 
  mutate(id_data = 2, variable = "rezago_escolar_13-17", 
         value_label = glue("Rezago escolar de 13 a 17 años por quintil de ingreso (%) - quintil {quin}"), 
         id_nivel = "dpto", id_time = 1, time = year, value = rezago_esc, nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
# write_csv(sec_dpto, glue("{datos}/base_rezago_escolar_13-17_dpto_quintil_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Asistencia escolar por zonas, quintil y year
zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona, quin) %>%
  summarise(rezago_secund = sum(rezago_secund), totalp5 = sum(totalp5)) %>%
  ungroup()

# Agrupamos dptos por zona
esc_zona <- zonas %>%
  group_by(zona, year, quin) %>%
  summarise(rezago_secund = sum(rezago_secund), total = sum(totalp5)) %>%
  ungroup() %>%
  mutate(rezago_esc = 100*(rezago_secund/total))

# Organizar base datos
esc_zona <- esc_zona %>% 
  mutate(id_data = 2, variable = "rezago_escolar_13-17", 
         value_label = glue("Rezago escolar de 13 a 17 años por quintil de ingreso (%) - quintil {quin}"), 
         id_nivel = "zona", id_time = 1, time = year, value = rezago_esc, nivel_value = zona) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(nivel_value, time)

# Exportar base
# write_csv(esc_zona, glue("{datos}/base_rezago_escolar_13-17_zonas_quintil_2010-2020.csv"))

#--------------------------#
# C. Exportar ----
#--------------------------#

# Guardamos una base por quintil y nivel territorial
lapply(unique(sec_dpto$value_label), function(x){
  
  print(x)
  quintil <- gsub(".*- quintil ", "", x)
  quintil <- tolower(quintil) %>% str_trim()
  print(quintil)
  dptos_df <- sec_dpto %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{quintil}"))
  zonas_df <- esc_zona %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{quintil}"))
  
  # Exportar base
  write_csv(dptos_df, glue("{datos}/base_rezago_escolar_13-17_dpto_quintil_{quintil}_2010-2020.csv"))
  write_csv(zonas_df, glue("{datos}/base_rezago_escolar_13-17_zonas_quintil_{quintil}_2010-2020.csv"))
  
})
