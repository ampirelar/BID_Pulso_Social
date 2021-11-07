#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 12 oct, 2021
# Procesamiento de datos de suicidios 2010 -2020
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

datos_ori <- "Data/Nacimientos_y_defunciones/Input"
datos <- "Data/Nacimientos_y_defunciones/Output"
options(scipen = 999)

# Adulto y joven----

dpto <- read_csv(glue("{datos}/base_suicidios_edad_dpto_2010-2020.csv"))

path <- getwd()
path <- gsub("01_Datos", "02_Descriptivas", path)
options(scipen = 999)

cod_dpto <- read_excel(glue("{path}/Herramientas/Input/base_nombres_departamentos.xlsx")) %>%
  rename(cod_dpto = "nivel_value")

dpto <- dpto %>% mutate(rango = as.character(substr(nivel_value, 3, 5)),
                        rango = as.character(gsub("_", "", rango)),
                        rango = as.character(gsub("a", "", rango)))

dpto <- dpto %>% filter(rango == "15" | rango == "45") %>%
  mutate(rango = ifelse(rango < 45, "15_a_44_anios", "45_a_64_anios"),
         cod_dpto = as.character(substr(nivel_value, 1, 2)),
         cod_dpto = as.numeric(gsub("_", "", cod_dpto))) %>% left_join(cod_dpto, by = "cod_dpto") %>%
  mutate(nivel_label = glue("{nivel_label.x}_{rango}"), nivel_label = as.character(gsub(" ", "_", nivel_label)),
         variable = "suicidios_15_64") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

write_csv(dpto, glue("{datos}/base_suicidios_15_64_anios_dpto_2010-2020.csv"))










