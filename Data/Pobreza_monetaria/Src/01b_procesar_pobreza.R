#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 15 sept, 2021
# Procesamiento de datos de incidencia de pobreza monetaria y pobreza extrema
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

datos_ori <- "Data/Pobreza_monetaria/Input"
datos <- "Data/Pobreza_monetaria/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Pobreza monetaria ----
#-------------------------------------------------------#

# Etiquetas nombres departamentos y zonas
nom_dpto <- read_xlsx("Descriptives/Herramientas/Input/base_nombres_departamentos.xlsx")
zonas <- c("Cabeceras", "Otras cabeceras", "Centros poblados y rural disperso")

#--------------------------#
# A. Por departamentos y nacional ----
#--------------------------#

# Leer base original
monetaria <- read_excel(glue("{datos_ori}/anexo_pobreza_monetaria_20_departamento.xls"), sheet = "IP Act.Met.") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) 

# Organizar datos
colnames(monetaria)[1] <- "departamento"
monetaria <- monetaria %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "pobreza") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = departamento)

# Total departamental anual
monetaria_dpto <- monetaria %>% 
  left_join(nom_dpto, by = "nivel_label") %>%
  mutate(id_data = 13, variable = "pobreza_monetaria", value_label = "Pobreza monetaria (%)", 
         id_nivel = "dpto", id_time = 1, time = year, value = pobreza) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value)

# Total nacional anual
monetaria_col <- monetaria %>% filter(nivel_label == "Total Nacional") %>%
  mutate(id_data = 13, variable = "pobreza_monetaria", value_label = "Pobreza monetaria (%)", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1, time = year, value = pobreza) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(monetaria_col, glue("{datos}/base_pobreza_monetaria_col_2012-2020.csv"))
write_csv(monetaria_dpto, glue("{datos}/base_pobreza_monetaria_dpto_2012-2020.csv"))

rm(list = ls(pattern = "monetaria"))

#--------------------------#
# B. Por zona (nacional, cabecera, rural) ----
#--------------------------#

# Leer base original
monetaria <- read_excel(glue("{datos_ori}/anexo_pobreza_monetaria_20_nacional.xls"), sheet = "Pobreza Monetaria Act.Met.") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) 

# Tomamos solo pobreza monetaria (no incluimos cuadro de pobreza extrema)
colnames(monetaria)[1] <- "zona"
monetaria <- monetaria[which(monetaria$zona == "Barranquilla A.M.")[1]:which(monetaria$zona == "Otras cabeceras")[1], ]

# Organizar datos
monetaria <- monetaria %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "pobreza") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = zona)

# Areas metropolitanas
monetaria_am <- monetaria %>% 
  filter(nivel_label != "Nacional" & !(nivel_label %in% zonas)) %>%
  mutate(id_data = 13, nivel_value = glue("{nivel_label}_pobreza"), variable = "pobreza_monetaria", 
         value_label = "Pobreza monetaria (%)", id_nivel = "area_metropolitana", 
         id_time = 1, time = year, value = pobreza, nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Cabeceras y zonas rurales (centros poblados y rural disperso)
monetaria_zonas <- monetaria %>% 
  filter(nivel_label %in% zonas) %>%
  mutate(id_data = 13, variable = "pobreza_monetaria", value_label = "Pobreza monetaria (%)", 
         id_nivel = "zona", id_time = 1, time = year, nivel_value = glue("{nivel_label}_pobreza"), 
         value = pobreza, nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(monetaria_am, glue("{datos}/base_pobreza_monetaria_am_2012-2020.csv"))
write_csv(monetaria_zonas, glue("{datos}/base_pobreza_monetaria_zonas_2012-2020.csv"))

rm(list = ls(pattern = "monetaria"))

#-------------------------------------------------------#
# 2. Pobreza extrema ----
#-------------------------------------------------------#

#--------------------------#
# A. Por departamentos y nacional ----
#--------------------------#

# Leer base original
extrema <- read_excel(glue("{datos_ori}/anexo_pobreza_monetaria_20_departamento.xls"), sheet = "IPE Act.Met.") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) 

# Organizar datos
colnames(extrema)[1] <- "departamento"
extrema <- extrema %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "pobreza") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = departamento)

# Total departamental anual
extrema_dpto <- extrema %>% 
  left_join(nom_dpto, by = "nivel_label") %>%
  mutate(id_data = 13, variable = "pobreza_extrema", value_label = "Pobreza monetaria extrema (%)", 
         id_nivel = "dpto", id_time = 1, time = year, value = pobreza) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value)

# Total nacional anual
extrema_col <- extrema %>% filter(nivel_label == "Total Nacional") %>%
  mutate(id_data = 13, variable = "pobreza_extrema", value_label = "Pobreza monetaria extrema (%)", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1, time = year, value = pobreza) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(extrema_col, glue("{datos}/base_pobreza_extrema_col_2012-2020.csv"))
write_csv(extrema_dpto, glue("{datos}/base_pobreza_extrema_dpto_2012-2020.csv"))

rm(list = ls(pattern = "extrema"))

#--------------------------#
# B. Por zona (nacional, cabecera, rural) ----
#--------------------------#

# Leer base original
extrema <- read_excel(glue("{datos_ori}/anexo_pobreza_monetaria_20_nacional.xls"), sheet = "Pobreza Monetaria Act.Met.") %>%
  drop_na(`...3`) 

colnames(extrema)[1] <- "zona"

# Tomamos solo pobreza extrema (no incluimos cuadro de pobreza monetaria)
extrema <- extrema[which(is.na(extrema$zona))[2]:which(extrema$zona == "Otras cabeceras")[2], ]

# Organizar datos
extrema <- extrema %>% janitor::row_to_names(row_number = 1)
colnames(extrema)[1] <- "zona"

extrema <- extrema %>%
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "pobreza") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = zona)

# Areas metropolitanas
extrema_am <- extrema %>% 
  filter(nivel_label != "Nacional" & !(nivel_label %in% zonas)) %>%
  mutate(id_data = 13, nivel_value = glue("{nivel_label}_extrema"), variable = "pobreza_extrema", 
         value_label = "Pobreza extrema (%)", id_nivel = "area_metropolitana", 
         id_time = 1, time = year, value = pobreza, nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Cabeceras y zonas rurales (centros poblados y rural disperso)
extrema_zonas <- extrema %>% 
  filter(nivel_label %in% zonas) %>%
  mutate(id_data = 13, variable = "pobreza_extrema", value_label = "Pobreza extrema (%)", 
         id_nivel = "zona", id_time = 1, time = year, nivel_value = glue("{nivel_label}_extrema"), 
         value = pobreza, nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(extrema_am, glue("{datos}/base_pobreza_extrema_am_2012-2020.csv"))
write_csv(extrema_zonas, glue("{datos}/base_pobreza_extrema_zonas_2012-2020.csv"))

rm(list = ls(pattern = "extrema"))
