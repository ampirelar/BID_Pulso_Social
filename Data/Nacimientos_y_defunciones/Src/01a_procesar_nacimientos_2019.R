#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 22 sept, 2021
# Procesamiento de datos de Defunciones y Nacimientos 2019
# Nota: 2020 no tiene aun la informacion de nacimientos por grupo de edad de la madre
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

datos_ori <- "Data/Nacimientos_y_defunciones/Input"
datos <- "Data/Nacimientos_y_defunciones/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Poblacion ----
#-------------------------------------------------------#

# Abrimos datos de poblacion por genero y edad
pob_edad <- read_csv("Data/Proyecciones_poblacion/Output/base_proyecciones_poblacion_mpio_gen_edad_2005-2026.csv") %>%
  mutate(genero = str_extract(nivel_value, "_.*"), genero = substr(genero, 1, 2), 
         genero = as.numeric(gsub("_", "", genero)), edad = as.numeric(gsub(".*_", "", nivel_value)),
         mpio = as.numeric(gsub("_.*", "", nivel_value)))

# Solo mujeres entre 15-19 anios
pob_joven <- pob_edad %>% 
  filter(genero == 2 & edad >= 15 & edad <= 19) %>% 
  select(mpio, time, value) %>% rename(nivel_value = mpio, poblacion = value) %>%
  group_by(nivel_value, time) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup()

table(pob_joven$time)

#-------------------------------------------------------#
# 1. Municipal ----
# FECUNDIDAD EN MUJERES JÓVENES (NACIMIENTOS POR 1,000 MUJERES DE 15 A 19 AÑOS DE EDAD)
#-------------------------------------------------------#

# Leer base original 2019 y organizar
nac19 <- read_excel(glue("{datos_ori}/2019/nacimientos2019-cuadro7.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(total = na_3, dpto = na) %>%
  select(dpto, departamento, municipio, total, starts_with("de"), sin_informacion) %>%
  pivot_longer(cols = starts_with(c("de_", "sin_informacion")), names_to = "edad", values_to = "nacimientos") %>%
  mutate(edad = gsub("de_", "", edad), edad = gsub("_anos", "", edad), edad = gsub("_", "-", edad),
         edad = gsub("sin-informacion", "Sin informacion", edad), time = 2019, municipio = as.numeric(municipio)) %>%
  rename(nivel_value = municipio)

# Solo nacimientos de mujeres entre 15-19 anios y pegamos poblacion 15-19
nac19 <- nac19 %>% filter(edad == "15-19") %>% 
  left_join(pob_joven, by = c("nivel_value", "time")) %>%
  mutate(nacimientos = as.numeric(nacimientos))
  
# Municipales
nac_mpio <- nac19 %>% 
  filter(is.na(dpto)) %>% 
  select(-c(dpto, departamento, edad, total)) %>%
  mutate(id_data = 19, variable = "fecundidad_joven", value_label = "Tasa de fecundidad de mujeres jóvenes", 
         id_nivel = "mpio", id_time = 1, value = (nacimientos/poblacion)*1000) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#-------------------------------------------------------#
# 2. Departamental y nacional ----
#-------------------------------------------------------#

# Poblacion departamental
pob_dpto <- nac19 %>%
  filter(!(departamento == "03" & dpto == "Total Nacional")) %>%
  select(departamento, poblacion) %>% 
  group_by(departamento) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup()

# Poblacion nacional
pob_joven_col <- pob_joven %>% 
  group_by(time) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup()

# Departamental
nac_dpto <- nac19 %>% 
  filter(!is.na(dpto) & dpto != "Total Nacional") %>% 
  select(-c(dpto, nivel_value, edad, total, poblacion)) %>%
  left_join(pob_dpto, by = "departamento") %>%
  # Borramos departamento sin informacion o bebes nacidos en el extranjero
  filter(departamento != "01" & departamento != "75") %>%
  mutate(id_data = 19, nivel_value = as.numeric(departamento), 
         variable = "fecundidad_joven", value_label = "Tasa de fecundidad de mujeres jóvenes", 
         id_nivel = "dpto", id_time = 1, value = (nacimientos/poblacion)*1000) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Nacional
nac_col <- nac19 %>% 
  filter(dpto == "Total Nacional") %>%
  select(-poblacion) %>%
  left_join(pob_joven_col, by = "time") %>%
  mutate(id_data = 19, nivel_value = 1, 
         variable = "fecundidad_joven", value_label = "Tasa de fecundidad de mujeres jóvenes", 
         id_nivel = "nacional", id_time = 1, value = (nacimientos/poblacion)*1000) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(nac_col, glue("{datos}/base_fecundidad_joven_col_2019.csv"))
write_csv(nac_dpto, glue("{datos}/base_fecundidad_joven_dpto_2019.csv"))
write_csv(nac_mpio, glue("{datos}/base_fecundidad_joven_mpio_2019.csv"))
