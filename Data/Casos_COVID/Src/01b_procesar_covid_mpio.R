#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 14 oct, 2021
# Procesamiento de datos de Covid
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl, janitor)
#.rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Casos_COVID/Input"
datos <- "Data/Casos_COVID/Output"
options(scipen = 999)

#Extraer base de municipios----

mpio_cov <- read_csv(glue("{datos}/casoscovid_mpio.csv"))

cov_muer <- mpio_cov %>% rename(value = "muertes_mpio", nivel_value = "cod_ciudad") %>%
  mutate(id_data = "10", variable = "fallecidos", id_nivel = "mpio", id_time = "2", 
         time = glue("{mes}_{ano}"), value_label = "Fallecidos de Covid - 19") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

mpio_cov <- mpio_cov %>% rename(value = "casos_mpio", nivel_value = "cod_ciudad") %>%
  mutate(id_data = "10", variable = "casos", id_nivel = "mpio", id_time = "2", 
         time = glue("{mes}_{ano}"), value_label = "Casos totales de Covid - 19") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(mpio_cov, glue("{datos}/base_casos_covid_mpio_2020-2021.csv"))
write_csv(cov_muer, glue("{datos}/base_fallecidos_covid_mpio_2020-2021.csv"))

#Departamentos mes----

mpio_cov <- read_csv(glue("{datos}/casoscovid_mpio.csv")) %>% mutate(cod_dpto = as.numeric(substr(cod_dpto, 1, 2)))
mpio_cov$cod_dpto[mpio_cov$cod_dpto == "80"] <- 8
tot <- read_csv(glue("Data/Proyecciones_poblacion/Input/poblacion_dpto_2005-2026.csv")) %>%
  rename(ano = year)

cov_muer <- mpio_cov %>% rename(value = "muertes_mpio") %>% 
  group_by(cod_dpto, ano, mes) %>%
  summarise(value = sum(value), casos_mpio = sum(casos_mpio)) %>%
  ungroup() %>% 
  mutate(id_data = "10", variable = "fallecidos_contagios", id_nivel = "dpto", id_time = "2", value = (value/casos_mpio)*100,
         time = glue("{mes}_{ano}"), value_label = "Porcentaje de fallecidos de Covid - 19", nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

cov_muer2 <- mpio_cov %>% rename(value = "muertes_mpio") %>% 
  group_by(cod_dpto, ano, mes) %>%
  summarise(value = sum(value), casos_mpio = sum(casos_mpio)) %>%
  ungroup() %>% left_join(tot, by = c("cod_dpto", "ano")) %>%
  mutate(id_data = "10", variable = "fallecidos", id_nivel = "dpto", id_time = "2", value = (value/poblacion)*100000,
         time = glue("{mes}_{ano}"), value_label = "Fallecidos de Covid - 19", nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

mpio_cov <-  mpio_cov %>% rename(value = "casos_mpio") %>%
  group_by(cod_dpto, ano, mes) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>% left_join(tot, by = c("cod_dpto", "ano")) %>%
  mutate(id_data = "10", variable = "casos", id_nivel = "dpto", id_time = "2", value = (value/poblacion)*100000,
         time = glue("{mes}_{ano}"), value_label = "Casos totales de Covid - 19", nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(mpio_cov, glue("{datos}/base_casos_covid_dpto_2020-2021.csv"))
write_csv(cov_muer2, glue("{datos}/base_fallecidos_covid_dpto_2020-2021.csv"))
write_csv(cov_muer, glue("{datos}/base_fallecidos_contagios_covid_dpto_2020-2021.csv"))

#Departamentos trimestre----

mpio_cov <- read_csv(glue("{datos}/casoscovid_mpio.csv")) %>% mutate(cod_dpto = as.numeric(substr(cod_dpto, 1, 2))) %>%
  mutate(trimestre = ifelse(mes < 4, "primer",
                            ifelse(mes > 3 & mes < 7, "segundo",
                                   ifelse(mes > 6 & mes < 10, "tercero", "cuarto"))))
mpio_cov$cod_dpto[mpio_cov$cod_dpto == "80"] <- 8


cov_muer <- mpio_cov %>% rename(value = "muertes_mpio") %>% 
  group_by(cod_dpto, ano, trimestre) %>%
  summarise(value = sum(value), casos_mpio = sum(casos_mpio)) %>%
  ungroup() %>% 
  mutate(id_data = "10", variable = "fallecidos_contagios", id_nivel = "dpto", id_time = "3", value = (value/casos_mpio)*100,
         time = glue("{trimestre}_{ano}"), value_label = "Porcentaje de fallecidos de Covid - 19", nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

cov_muer2 <- mpio_cov %>% rename(value = "muertes_mpio") %>% 
  group_by(cod_dpto, ano, trimestre) %>%
  summarise(value = sum(value), casos_mpio = sum(casos_mpio)) %>%
  ungroup() %>% left_join(tot, by = c("cod_dpto", "ano")) %>%
  mutate(id_data = "10", variable = "fallecidos", id_nivel = "dpto", id_time = "3", value = (value/poblacion)*100000,
         time = glue("{trimestre}_{ano}"), value_label = "Fallecidos de Covid - 19", nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

mpio_cov <-  mpio_cov %>% rename(value = "casos_mpio") %>%
  group_by(cod_dpto, ano, trimestre) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>% left_join(tot, by = c("cod_dpto", "ano")) %>%
  mutate(id_data = "10", variable = "casos", id_nivel = "dpto", id_time = "3", value = (value/poblacion)*100000,
         time = glue("{trimestre}_{ano}"), value_label = "Casos totales de Covid - 19", nivel_value = cod_dpto) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(mpio_cov, glue("{datos}/base_casos_covid_trim_dpto_2020-2021.csv"))
write_csv(cov_muer2, glue("{datos}/base_fallecidos_covid_trim_dpto_2020-2021.csv"))
write_csv(cov_muer, glue("{datos}/base_fallecidos_contagios_covid_trim_dpto_2020-2021.csv"))



