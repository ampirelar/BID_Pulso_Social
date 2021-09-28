#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 26 sept, 2021
# Procesamiento de datos de Defunciones y Nacimientos 2019 - 2020
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
# 1.1 Municipal ----
# Nacimientos
#-------------------------------------------------------#

# Leer base original 2019 y organizar
nac19 <- read_excel(glue("{datos_ori}/2019/nacimientos2019-cuadro7.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(nac = na_3, dpto = na) %>%
  select(dpto, departamento, municipio, nac) %>%
  mutate(time = 2019, municipio = as.numeric(municipio)) %>%
  rename(nivel_value = municipio)

nac20 <- read_excel(glue("{datos_ori}/2020/nacimientos2020p-cuadro3.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(nac = na_3, dpto = na) %>%
  select(dpto, departamento, municipio, nac) %>%
  mutate(time = 2020, municipio = as.numeric(municipio)) %>%
  rename(nivel_value = municipio)

#-------------------------------------------------------#
# 1.2 Municipal ----
# Defunciones
#-------------------------------------------------------#

def19 <- read_excel(glue("{datos_ori}/2019/defunciones2019-cuadro3.xlsx")) %>% 
  janitor::row_to_names(row_number = 2) %>% janitor::clean_names()

def19$hombres_2 = as.numeric(def19$hombres_2)
def19$mujeres_2 = as.numeric(def19$mujeres_2)
def19$indeterminado_2 = as.numeric(def19$indeterminado_2)

def19 <- def19 %>% mutate(def = hombres_2 + mujeres_2 + indeterminado_2) %>%
  rename(dpto = na, mpio = na_2) %>%
  select(dpto, departamento, municipio, def) %>%
  mutate(time = 2019, municipio = as.numeric(municipio)) %>%
  rename(nivel_value = municipio)

def20 <- read_excel(glue("{datos_ori}/2020/defunciones2020p-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names()

def20$hombres_2 = as.numeric(def20$hombres_2)
def20$mujeres_2 = as.numeric(def20$mujeres_2)
def20$indeterminado_2 = as.numeric(def20$indeterminado_2)

def20 <- def20 %>% mutate(def = hombres_2 + mujeres_2 + indeterminado_2) %>%
  rename(dpto = na, mpio = na_2) %>%
  select(dpto, departamento, municipio, def) %>%
  mutate(time = 2020, municipio = as.numeric(municipio)) %>%
  rename(nivel_value = municipio)

nac_t <- left_join(nac19, def19, by = "nivel_value")
nac_tt <- left_join(nac20, def20, by = "nivel_value")
# Municipales

nac_mpio1 <- nac_t %>% 
  filter(is.na(dpto.x)) %>% 
  select(-c(dpto.x, departamento.x, dpto.y, departamento.y)) %>%
  mutate(id_data = 19, variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil", 
         id_nivel = "mpio", id_time = 1, value = (def/nac)*1000, time = "2019") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_mpio2 <- nac_tt %>% 
  filter(is.na(dpto.x)) %>% 
  select(-c(dpto.x, departamento.x, dpto.y, departamento.y)) %>%
  mutate(id_data = 19, variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil", 
         id_nivel = "mpio", id_time = 1, value = (def/nac)*1000, time = "2020") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#-------------------------------------------------------#
# 2. Departamental y nacional ----
#-------------------------------------------------------#

# Departamental
nac_dpto1 <- nac_t %>% 
  filter(!is.na(dpto.x) & dpto.x != "Total Nacional") %>% 
  select(-c(dpto.x, dpto.y, departamento.y, nivel_value)) %>%
  # Borramos departamento sin informacion o bebes nacidos en el extranjero
  filter(departamento.x != "01" & departamento.x != "75") %>%
  mutate(id_data = 19, nivel_value = as.numeric(departamento.x), 
         variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil", 
         id_nivel = "dpto", id_time = 1, value = (def/nac)*1000, time ="2019") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_dpto2 <- nac_tt %>% 
  filter(!is.na(dpto.x) & dpto.x != "Total Nacional") %>% 
  select(-c(dpto.x, dpto.y, departamento.y, nivel_value)) %>%
  # Borramos departamento sin informacion o bebes nacidos en el extranjero
  filter(departamento.x != "01" & departamento.x != "75") %>%
  mutate(id_data = 19, nivel_value = as.numeric(departamento.x), 
         variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil", 
         id_nivel = "dpto", id_time = 1, value = (def/nac)*1000, time ="2020") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Nacional
nac_col1 <- nac_t %>% 
  filter(dpto.x == "Total Nacional") %>%
  mutate(id_data = 19, nivel_value = 1, 
         variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil", 
         id_nivel = "nacional", id_time = 1, value = (def/nac)*1000, time = "2019") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_col2 <- nac_tt %>% 
  filter(dpto.x == "Total Nacional") %>%
  mutate(id_data = 19, nivel_value = 1, 
         variable = "mortalidad_inf", value_label = "Tasa de mortalidad infantil", 
         id_nivel = "nacional", id_time = 1, value = (def/nac)*1000, time = "2020") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Unir datos

nac_col <- rbind(nac_col1, nac_col2)
nac_dpto <- rbind(nac_dpto1, nac_dpto2)
nac_mpio <- rbind(nac_mpio1, nac_mpio2)

# Exportar datos
write_csv(nac_col, glue("{datos}/base_mortalidad_inf_col_2019-2020.csv"))
write_csv(nac_dpto, glue("{datos}/base_mortalidad_inf_dpto_2019-2020.csv"))
write_csv(nac_mpio, glue("{datos}/base_mortalidad_inf_mpio_2019-2020.csv"))
