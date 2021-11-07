#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 sept, 2021
# Procesamiento de datos IPM
#-------------------------------------------------------#


#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
#.rs.restartR()

datos_ori <- "Data/Indice_pobreza_multidimensional/Input"
datos <- "Data/Indice_pobreza_multidimensional/Output"
path <- getwd()
path <- gsub("01_Datos", "02_Descriptivas", path)
options(scipen = 999)

nom_dpto <- read_xlsx(glue("{path}/Herramientas/Input/base_nombres_departamentos.xlsx"))

#----------------------------------
#Hacinamiento cr?tico zonas
#----------------------------------

ipm <- read_excel(glue("{datos_ori}/anexo_nal_pobreza_multidimensional_20.xls"), sheet = "IPM_Variables") %>%
  drop_na(`...4`) %>% janitor::row_to_names(row_number = 1)

colnames(ipm)[1] <- "zona"
colnames(ipm)[2] <- "variable"

ipm$`2017`[is.na(ipm$`2017`)] <- "."

ipm <- ipm %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>% filter(variable == "Analfabetismo" | variable == "Hacinamiento crítico") %>%
  fill(zona,.direction = "down") %>% mutate(zona = gsub("\\*", "", zona)) %>% filter(variable == "Hacinamiento crítico") %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = zona) %>%
  mutate(time = year, id_data = 15, id_time = 1, variable = "hacinamiento", value_label = "Hacinamiento crítico (%)")

# Nacional
ipm_col <- ipm %>% 
  filter(nivel_label == "Nacional") %>%
  mutate(id_nivel = "nacional", nivel_value = 1) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Zonas
zonas <- c("Cabeceras", "Centros poblados y rural disperso")

ipm_zonas <- ipm %>% 
  filter(nivel_label %in% zonas) %>%
  mutate(id_nivel = "zona", nivel_value = glue("{nivel_label}_hacinamiento"), 
         nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(ipm_col, glue("{datos}/base_hacinamiento_col_2010-2020.csv"))
write_csv(ipm_zonas, glue("{datos}/base_hacinamiento_zonas_2010-2020.csv"))

#----------------------------------
#Hacinamiento cr?tico departamentos
#----------------------------------

ipm <- read_excel(glue("{datos_ori}/anexo_dptal_pobreza_multidimensional_20.xls"), sheet = "IPM_Variables_Departamento ") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1)

ipm_$nivel_label[ipm_$nivel_label == "Bogot?"] <- "Bogot? D.C."

ipm <- ipm %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(nivel_label = departamento, total_2018 = total, total_2019 = total_2, total_2020 = total_3) %>%
  select(nivel_label, variable, starts_with("total")) %>% filter(variable == "Analfabetismo" | variable == "Hacinamiento cr?tico") %>%
  fill(nivel_label,.direction = "down") %>% filter(variable == "Hacinamiento cr?tico") 

ipm$nivel_label[ipm$nivel_label == "Bogot?"] <- "Bogot? D.C."

ipm <- ipm %>% mutate(across(.cols = starts_with("total"), .fns = as.numeric)) %>%
  left_join(nom_dpto, by = "nivel_label") %>%
  pivot_longer(cols = starts_with("total"), names_to = "time", values_to = "value")

ipm_dpto <- ipm %>%
  mutate(time = as.numeric(gsub("total_", "", time)), id_data = 15, id_time = 1, 
         variable = "hacinamiento", value_label = "Hacinamiento cr?tico (%)", id_nivel = "dpto") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(ipm_dpto, glue("{datos}/base_hacinamiento_dpto_2018-2020.csv"))
rm(list = ls(pattern = "ipm"))


