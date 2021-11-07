#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 04 oct, 2021
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
# 1 Municipal ----
# Defunciones - Prematuras
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

# Municipales

nac_mpio1 <- def19 %>% 
  filter(is.na(dpto)) %>% 
  select(-c(dpto, departamento)) %>%
  mutate(id_data = 19, variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "mpio", id_time = 1, value = def, time = "2019") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_mpio2 <- def20 %>% 
  filter(is.na(dpto)) %>% 
  select(-c(dpto, departamento)) %>%
  mutate(id_data = 19, variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "mpio", id_time = 1, value = def, time = "2020") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#-------------------------------------------------------#
# 2. Departamental y nacional ----
#-------------------------------------------------------#

#Departamentos

nac_dpto1 <- def19 %>% 
  filter(!is.na(dpto) & dpto != "Total Nacional") %>% 
  select(-c(dpto)) %>%
  # Borramos departamento sin informacion o bebes nacidos en el extranjero
  filter(departamento != "01" & departamento != "75") %>%
  mutate(id_data = 19, nivel_value = as.numeric(departamento), 
         variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "dpto", id_time = 1, value = def, time ="2019") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_dpto2 <- def20 %>% 
  filter(!is.na(dpto) & dpto != "Total Nacional") %>% 
  select(-c(dpto)) %>%
  # Borramos departamento sin informacion o bebes nacidos en el extranjero
  filter(departamento != "01" & departamento != "75") %>%
  mutate(id_data = 19, nivel_value = as.numeric(departamento), 
         variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "dpto", id_time = 1, value = def, time ="2020") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#Nacional

nac_col1 <- def19 %>% 
  filter(dpto == "Total Nacional") %>%
  mutate(id_data = 19, nivel_value = 1, 
         variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "nacional", id_time = 1, value = def, time = "2019") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_col2 <- def20 %>% 
  filter(dpto == "Total Nacional") %>%
  mutate(id_data = 19, nivel_value = 1, 
         variable = "defunciones_prematuras", value_label = "Número de defunciones prematuras", 
         id_nivel = "nacional", id_time = 1, value = def, time = "2020") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Unir datos

nac_col <- rbind(nac_col1, nac_col2)
nac_dpto <- rbind(nac_dpto1, nac_dpto2)
nac_mpio <- rbind(nac_mpio1, nac_mpio2)

# Exportar datos
write_csv(nac_col, glue("{datos}/defunciones_prematuras_col_2019-2020.csv"))
write_csv(nac_dpto, glue("{datos}/defunciones_prematuras_dpto_2019-2020.csv"))
write_csv(nac_mpio, glue("{datos}/defunciones_prematuras_mpio_2019-2020.csv"))


