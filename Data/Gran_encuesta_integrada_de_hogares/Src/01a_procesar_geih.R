#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 6 sept, 2021
# Procesamiento de datos GEIH
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

datos_ori <- "Data/Gran_encuesta_integrada_de_hogares/Input"
datos <- "Data/Gran_encuesta_integrada_de_hogares/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Procesar datos originales ----
#-------------------------------------------------------#

# Etiquetas nombres departamentos
nom_dpto <- read_xlsx("Descriptives/Herramientas/Input/base_nombres_departamentos.xlsx")

#--------------------------#
# A. General ----
#--------------------------#

# Leer base original
geih <- read_excel(glue("{datos_ori}/anexo_GEIH_dep_20.xls"), sheet = "Departamentos anual")

# Organizamos nombres de las columnas
geih <- geih %>% .[-c(1:7), ] %>% janitor::row_to_names(row_number = 1) 
colnames(geih)[1] <- "variable"
geih <- geih %>% 
  drop_na(variable) %>% filter(variable == "TD" | variable %in% nom_dpto$nivel_label) %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>%
  rename(year_2020 = `year_2020*`) %>% janitor::clean_names()

# Pasamos de wide a long
geih <- geih %>% 
  mutate(year_2001 = as.numeric(year_2001), year_2020 = as.numeric(year_2020)) %>%
  fill(starts_with("year_"),.direction = "up") %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "tasa_desempleo") %>%
  filter(variable != "TD")

# Pegamos codigos de departamento y estructuramos base 
data_geih <- geih %>%
  rename(nivel_label = variable, value = tasa_desempleo, time = year) %>%
  left_join(nom_dpto, by = "nivel_label") %>%
  mutate(id_data = 1, variable = "tasa_desempleo", value_label = "Tasa de desempleo (%)", 
         id_nivel = "dpto", id_time = 1, time = as.numeric(gsub("year_", "", time))) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

table(data_geih$time)

# Exportar base
write_csv(data_geih, glue("{datos}/base_tasa_desempleo_dpto_2001-2020.csv"))
rm(geih, data_geih)

#--------------------------#
# B. Genero ----
#--------------------------#  

# Hombres
geih_hombre <- read_excel(glue("{datos_ori}/anexo_GEIH_dep_20.xls"), sheet = "Departamentos anual hombres")

# Organizamos nombres de las columnas
geih_hombre <- geih_hombre %>% .[-c(1:7), ] %>% janitor::row_to_names(row_number = 1) 

colnames(geih_hombre)[1] <- "variable"
geih_hombre <- geih_hombre %>% 
  drop_na(variable) %>% filter(variable == "TD" | variable %in% nom_dpto$nivel_label) %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>%
  rename(year_2020 = `year_2020*`) %>% janitor::clean_names()

# Pasamos de wide a long
geih_hombre <- geih_hombre %>% 
  mutate(year_2001 = as.numeric(year_2001), year_2020 = as.numeric(year_2020)) %>%
  fill(starts_with("year_"),.direction = "up") %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "tasa_desempleo") %>%
  filter(variable != "TD")

# Pegamos codigos de departamento y estructuramos base 
data_geih_h <- geih_hombre %>%
  rename(nivel_label = variable, value = tasa_desempleo, time = year) %>%
  left_join(nom_dpto, by = "nivel_label") %>%
  mutate(id_data = 1, variable = "tasa_desempleo", value_label = "Tasa de desempleo (%)", 
         id_nivel = "dpto_gen", id_time = 1, time = as.numeric(gsub("year_", "", time)),
         nivel_value = glue("{nivel_value}_1")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

table(data_geih_h$time)
rm(geih_hombre)

# Mujeres
geih_mujer <- read_excel(glue("{datos_ori}/anexo_GEIH_dep_20.xls"), sheet = "Departamentos anual mujeres")

# Organizamos nombres de las columnas
geih_mujer <- geih_mujer %>% .[-c(1:7), ] %>% janitor::row_to_names(row_number = 1) 
colnames(geih_mujer)[1] <- "variable"
geih_mujer <- geih_mujer %>% 
  drop_na(variable) %>% filter(variable == "TD" | variable %in% nom_dpto$nivel_label) %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>%
  rename(year_2020 = `year_2020*`) %>% janitor::clean_names()

# Pasamos de wide a long
geih_mujer <- geih_mujer %>% 
  mutate(year_2001 = as.numeric(year_2001), year_2020 = as.numeric(year_2020)) %>%
  fill(starts_with("year_"),.direction = "up") %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "tasa_desempleo") %>%
  filter(variable != "TD")

# Pegamos codigos de departamento y estructuramos base 
data_geih_m <- geih_mujer %>%
  rename(nivel_label = variable, value = tasa_desempleo, time = year) %>%
  left_join(nom_dpto, by = "nivel_label") %>%
  mutate(id_data = 1, variable = "tasa_desempleo", value_label = "Tasa de desempleo (%)", 
         id_nivel = "dpto_gen", id_time = 1, time = as.numeric(gsub("year_", "", time)),
         nivel_value = glue("{nivel_value}_2")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Unimos hombres y mujeres
data_geih <- bind_rows(data_geih_h, data_geih_m)
table(data_geih$time)

# Exportamos
write_csv(data_geih, glue("{datos}/base_tasa_desempleo_dpto_gen_2001-2020.csv"))
rm(geih_mujer, data_geih_h, data_geih_m, data_geih)


