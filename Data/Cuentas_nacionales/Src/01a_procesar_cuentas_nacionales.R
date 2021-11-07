#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 4 octubre, 2021
# Procesamiento de datos de Cuentas Nacionales: Indice de diversidad economica
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
# 1. Organizar datos ----
#-------------------------------------------------------#

# Etiquetas nombres departamentos
nom_dpto <- read_xlsx("Data/Herramientas/Input/base_nombres_departamentos.xlsx")
delete <- c("Fuente", "pprovisional", "ppreliminar", "Actualizado el 25 de junio de 2021", "Producto Interno Bruto por departamento - Base 2015")

# Cuentas nacionales: PIB por sector (12 sectores), a nivel departamental, 2005-2020
cuentas <- read_excel(glue("{datos_ori}/anexo-2020-preliminar-departamento-resultado.xlsx"), sheet = "Cuadro 1") %>%
  rename(variable = `...21`) %>%
  mutate(variable = gsub(":.*", "", variable)) %>%
  dplyr::select(-c(`...1`:Índice)) %>%
  filter(!(variable %in% delete)) %>%
  filter(!(is.na(variable) & is.na(`...22`) & is.na(`...23`))) %>%
  dplyr::select(-`...22`) %>%
  rename(year_2005 = `...24`, year_2006 = `...25`, year_2007 = `...26`, year_2008 = `...27`,
         year_2009 = `...28`, year_2010 = `...29`, year_2011 = `...30`, year_2012 = `...31`,
         year_2013 = `...32`, year_2014 = `...33`, year_2015 = `...34`, year_2016 = `...35`,
         year_2017 = `...36`, year_2018 = `...37`, year_2019 = `...38`, year_2020 = `...39`,
         actividad = `...23`) %>%
  mutate(variable = ifelse(variable %in% nom_dpto$nivel_label, variable, NA),
         year_2019 = as.numeric(year_2019), year_2020 = as.numeric(year_2020)) %>%
  fill(variable,.direction = "down") %>%
  drop_na(actividad) %>%
  filter(actividad != "ACTIVIDADES ECONÓMICAS")

# Pasar de wide a long
cuentas_long <- cuentas %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "part_sector") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  arrange(variable, year) %>%
  group_by(variable, year) %>%
  mutate(id_act = row_number()) %>%
  ungroup() %>%
  # Filtramos para tener las 12 agrupaciones + impuestos
  filter(id_act <= 12 | id_act == 14) %>%
  # Pegar codigo departamental
  rename(nivel_label = variable) %>%
  left_join(nom_dpto, by = "nivel_label")

# Organizar datos
data_cuentas <- cuentas_long %>%
  mutate(id_data = 21, variable = "part_sector", 
         value_label = "Participación del sector en PIB departamental", id_nivel = "dpto_sector", 
         id_time = 1, time = year, nivel_value = glue("{nivel_value}_{id_act}"), 
         nivel_label = glue("{nivel_label}_{actividad}"), value = part_sector) %>%
  dplyr::select(id_data, variable, id_nivel, actividad, nivel_value, nivel_label, id_time, time, value_label, value)

# Exportar datos
write_csv(data_cuentas %>% dplyr::select(nivel_value, nivel_label) %>% distinct(), 
          glue("{datos}/etiquetas_dpto_sector.csv"))

write_csv(data_cuentas %>% dplyr::select(-nivel_label), 
          glue("{datos}/part_sector_dpto_sector_2005-2020.csv"))






