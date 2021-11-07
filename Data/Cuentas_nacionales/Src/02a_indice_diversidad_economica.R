#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 octubre, 2021
# Construir Indice de diversidad economica
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

datos_ori <- "Data/Cuentas_nacionales/Input"
datos <- "Data/Cuentas_nacionales/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Indice diversidad  ----
#-------------------------------------------------------#

data_cuentas <- read_csv(glue("{datos}/part_sector_dpto_sector_2005-2020.csv"))

# Indice de diversidad basado en Hirschman-Herfindahl
data_ind <- data_cuentas %>%
  mutate(value_sq = (value/100)^2, cod_dpto = gsub("_.*", "", nivel_value)) %>%
  group_by(cod_dpto, time) %>%
  summarise(ind_div = sum(value_sq)) %>%
  ungroup()

# Organizar datos; indice de 0-100
data_ind <- data_ind %>%
  mutate(id_data = 21, variable = "ind_div_econ", 
         value_label = "Índice de diversidad económica", id_nivel = "dpto", 
         id_time = 1, nivel_value = as.numeric(cod_dpto), value = ind_div*100) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(data_ind, glue("{datos}/base_ind_div_econ_dpto_2005-2020.csv"))

