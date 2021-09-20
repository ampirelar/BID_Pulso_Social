#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 15 sept, 2021
# Procesamiento de datos de Indice de Pobreza Multidimensional
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

datos_ori <- "Data/Indice_pobreza_multidimensional/Input"
datos <- "Data/Indice_pobreza_multidimensional/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Municipal 2018 ----
#-------------------------------------------------------#

# Abrimos shp de IPM municipal y organizamos 
ipm <- sf::st_read(glue("{datos_ori}/MPM_FuenteCensal_2018.shp")) %>% 
  janitor::clean_names() %>% select(dpto_ccdgo, mpio_ccnct, mpio_cnmbr, mpm) %>%
  mutate(mpio_cnmbr = str_to_title(mpio_cnmbr), dpto_ccdgo = as.numeric(dpto_ccdgo), mpio_ccnct = as.numeric(mpio_ccnct))
sf::st_geometry(ipm) <- NULL
head(ipm)

ipm <- ipm %>%
  mutate(id_data = 15, id_time = 1, time = 2018, variable = "ipm", value_label = "IPM (%)", 
         id_nivel = "mpio", nivel_value = mpio_ccnct)
         
# Municipal
ipm_mpio <- ipm %>% 
  mutate(value = mpm) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(ipm_mpio, glue("{datos}/base_ipm_mpios_2018.csv"))
rm(list = ls(pattern = "ipm"))

#-------------------------------------------------------#
# 2. Departamental 2018-2020 ----
#-------------------------------------------------------#

# Codigos nombres departamentos
nom_dpto <- read_xlsx("Descriptives/Herramientas/Input/base_nombres_departamentos.xlsx")

# Abrir IPM
ipm <- read_excel(glue("{datos_ori}/anexo_dptal_pobreza_multidimensional_20.xls"), sheet = "IPM_Departamentos") %>%
  drop_na(`...3`)

# Organizar datos, wide a long
colnames(ipm)[1] <- "departamento"
ipm <- ipm %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(nivel_label = na, total_2018 = total, total_2019 = total_2, total_2020 = total_3) %>%
  select(nivel_label, starts_with("total")) %>%
  mutate(across(.cols = starts_with("total"), .fns = as.numeric)) %>%
  left_join(nom_dpto, by = "nivel_label") %>%
  pivot_longer(cols = starts_with("total"), names_to = "time", values_to = "value")
  
ipm_dpto <- ipm %>%
  mutate(time = as.numeric(gsub("total_", "", time)), id_data = 15, id_time = 1, 
         variable = "ipm", value_label = "IPM (%)", id_nivel = "dpto") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(ipm_dpto, glue("{datos}/base_ipm_dpto_2018-2020.csv"))
rm(list = ls(pattern = "ipm"))

#-------------------------------------------------------#
# 3. Nacional, zonas, regiones 2010-2020 ----
#-------------------------------------------------------#

# Abrir IPM
ipm <- read_excel(glue("{datos_ori}/anexo_nal_pobreza_multidimensional_20.xls"), sheet = "IPM_Dominios") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1)

# Organizar datos, wide a long
colnames(ipm)[1] <- "zona"
ipm <- ipm %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  drop_na(zona) %>%
  mutate(zona = gsub("\\*", "", zona)) %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = zona) %>%
  mutate(time = year, id_data = 15, id_time = 1, variable = "ipm", value_label = "IPM (%)")

# Nacional
ipm_col <- ipm %>% 
  filter(nivel_label == "Nacional") %>%
  mutate(id_nivel = "nacional", nivel_value = 1) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Zonas
zonas <- c("Cabecera", "Centros poblados y rural disperso")

ipm_zonas <- ipm %>% 
  filter(nivel_label %in% zonas) %>%
  mutate(id_nivel = "zona", nivel_value = glue("{nivel_label}_ipm"), 
         nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Regiones
ipm_region <- ipm %>% 
  filter(!(nivel_label %in% zonas) & nivel_label != "Nacional") %>%
  mutate(id_nivel = "region", nivel_value = glue("{nivel_label}_ipm"), 
         nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(ipm_col, glue("{datos}/base_ipm_col_2010-2020.csv"))
write_csv(ipm_zonas, glue("{datos}/base_ipm_zonas_2010-2020.csv"))
write_csv(ipm_region, glue("{datos}/base_ipm_region_2010-2020.csv"))


