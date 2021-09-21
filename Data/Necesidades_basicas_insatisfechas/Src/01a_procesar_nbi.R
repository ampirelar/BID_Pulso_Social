#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 21 sept, 2021
# Procesamiento de datos de Necesidades Basicas Insatisfechas
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

datos_ori <- "Data/Necesidades_basicas_insatisfechas/Input"
datos <- "Data/Necesidades_basicas_insatisfechas/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. General ----
#-------------------------------------------------------#

#--------------------------#
# A. Municipal ----
#--------------------------#

# Leer base original
nbi <- read_excel(glue("{datos_ori}/CNPV-2018-NBI-CENTROS-POBLADOS.xlsx")) %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  select(-starts_with(c("na", "total"))) %>%
  rename(value = personas_en_necesidades_basicas_insatisfechas_por_categorias,
         nivel_value = codigo_de_municipio) %>%
  filter(clase == "Cabecera municipal") %>%
  select(nivel_value, value) %>%
  mutate(nivel_value = as.numeric(nivel_value), value = as.numeric(value))

# Municipal
nbi_mpio <- nbi %>%
  mutate(id_data = 16, variable = "nbi", value_label = "NBI (%)", 
         id_nivel = "mpio", id_time = 1, time = 2018) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(nbi_mpio, glue("{datos}/base_nbi_mpio_2018.csv"))
rm(list = ls(pattern = "nbi"))

#--------------------------#
# B. Departamental, nacional y zonas ----
#--------------------------#

# Etiquetas nombres departamentos y zonas
nom_dpto <- read_xlsx("Descriptives/Herramientas/Input/base_nombres_departamentos.xlsx")

# Leer base original y organizar
nbi <- read_excel(glue("{datos_ori}/CNPV-2018-NBI.xlsx")) %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  select(departamentos, total, cabeceras, centros_poblados_y_rural_disperso) %>%
  rename(nivel_value = departamentos, nbi_total = total, nbi_cabe = cabeceras, nbi_rural = centros_poblados_y_rural_disperso) %>%
  filter(nivel_value != "Código Departamento") %>%
  filter(nbi_total != "Prop de Personas en NBI (%)") %>%
  mutate(nbi_total = as.numeric(nbi_total), nbi_cabe = as.numeric(nbi_cabe), nbi_rural = as.numeric(nbi_rural)) %>%
  mutate(id_data = 16, variable = "nbi", value_label = "NBI (%)", id_time = 1, time = 2018)

# Departamental
nbi_dpto <- nbi %>% 
  filter(nivel_value != "00") %>%
  mutate(nivel_value = as.numeric(nivel_value), id_nivel = "dpto", value = nbi_total) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Nacional
nbi_col <- nbi %>% 
  filter(nivel_value == "00") %>%
  mutate(nivel_value = 1, id_nivel = "nacional", value = nbi_total) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Zonas
nbi_zonas <- nbi %>% 
  filter(nivel_value == "00") %>%
  mutate(id_nivel = "zona") %>%
  select(-nbi_total) %>%
  pivot_longer(cols = c(nbi_cabe, nbi_rural), names_to = "zona", values_to = "value") %>%
  mutate(nivel_value = ifelse(zona == "nbi_cabe", "Cabeceras", "Centros Poblados y Rural Disperso"),
         nivel_value = glue("{nivel_value}_nbi"), nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(nbi_dpto, glue("{datos}/base_nbi_dpto_2018.csv"))
write_csv(nbi_col, glue("{datos}/base_nbi_col_2018.csv"))
write_csv(nbi_zonas, glue("{datos}/base_nbi_2018.csv"))

#-------------------------------------------------------#
# 2. Etnia ----
#-------------------------------------------------------#

#--------------------------#
# A. Municipal ----
#--------------------------#

# Categorias
# 1: Indigena
# 2: Gitano o rrom
# 3: Raizal
# 4: Palenquero
# 5: Negro, mulato,afrodescendiente, afrocolombiano
# 6: Ningun grupo etnico
# 99: Sin informacion

# Leer base original
nbi <- read_excel(glue("{datos_ori}/CNPV-2018-NBI-AUTORRECONOCIMIENTO-ETNICO.xlsx"), sheet = "MUNICIPAL") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  select(-starts_with(c("na", "total"))) %>%
  rename(value = personas_en_necesidades_basicas_insatisfechas_por_categorias,
         nivel_value = codigo_divipola, etnia = autoreconocimiento_etnico) %>%
  select(nivel_value, etnia, value) %>%
  mutate(nivel_value = as.numeric(nivel_value), value = as.numeric(value))

# Organizamos categorias de etnias
nbi <- nbi %>%
  mutate(etnia_val = ifelse(etnia == "Indígena", 1,
                            ifelse(etnia == "Gitano o Rrom", 2,
                                   ifelse(etnia == "Raizal", 3,
                                          ifelse(etnia == "Palenquero", 4,
                                                 ifelse(etnia == "Negro, mulato,afrodescendiente, afrocolombiano", 5,
                                                        ifelse(etnia == "Ningún grupo étnico", 6,
                                                               ifelse(etnia == "Sin información", 99, NA))))))))

# Base municipal
nbi_mpio <- nbi %>%
  mutate(nivel_value = glue("{nivel_value}_{etnia_val}")) %>%
  mutate(id_data = 16, variable = "nbi", value_label = "NBI (%)", 
         id_nivel = "mpio_etnia", id_time = 1, time = 2018) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(nbi_mpio, glue("{datos}/base_nbi_mpio_etnia_2018.csv"))
rm(list = ls(pattern = "nbi"))

#--------------------------#
# B. Departamental ----
#--------------------------#

# Leer base original
nbi <- read_excel(glue("{datos_ori}/CNPV-2018-NBI-AUTORRECONOCIMIENTO-ETNICO.xlsx"), sheet = "DEPARTAMENTAL") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  select(-starts_with(c("na", "total"))) %>%
  rename(value = personas_en_necesidades_basicas_insatisfechas_por_categorias,
         nivel_value = codigo_divipola, etnia = autoreconocimiento_etnico) %>%
  select(nivel_value, etnia, value) %>%
  mutate(nivel_value = as.numeric(nivel_value), value = as.numeric(value))

# Organizamos categorias de etnias
nbi <- nbi %>%
  mutate(etnia_val = ifelse(etnia == "Indígena", 1,
                            ifelse(etnia == "Gitano o Rrom", 2,
                                   ifelse(etnia == "Raizal", 3,
                                          ifelse(etnia == "Palenquero", 4,
                                                 ifelse(etnia == "Negro, mulato,afrodescendiente, afrocolombiano", 5,
                                                        ifelse(etnia == "Ningún grupo étnico", 6,
                                                               ifelse(etnia == "Sin información", 99, NA))))))))

# Base municipal
nbi_dpto <- nbi %>%
  mutate(nivel_value = glue("{nivel_value}_{etnia_val}")) %>%
  mutate(id_data = 16, variable = "nbi", value_label = "NBI (%)", 
         id_nivel = "dpto_etnia", id_time = 1, time = 2018) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(nbi_dpto, glue("{datos}/base_nbi_dpto_etnia_2018.csv"))
rm(list = ls(pattern = "nbi"))

#--------------------------#
# C. Nacional ----
#--------------------------#

# Leer base original
nbi <- read_excel(glue("{datos_ori}/CNPV-2018-NBI-AUTORRECONOCIMIENTO-ETNICO.xlsx"), sheet = "TOTAL NACIONAL") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(etnia = na) %>%
  select(etnia, personas_en_nbi_percent) %>%
  rename(value = personas_en_nbi_percent) %>%
  select(etnia, value) %>%
  mutate(nivel_value = 1, value = as.numeric(value))

# Organizamos categorias de etnias
nbi <- nbi %>%
  mutate(etnia_val = ifelse(etnia == "Indígena", 1,
                            ifelse(etnia == "Gitano o Rrom", 2,
                                   ifelse(etnia == "Raizal", 3,
                                          ifelse(etnia == "Palenquero", 4,
                                                 ifelse(etnia == "Negro, mulato,afrodescendiente, afrocolombiano", 5,
                                                        ifelse(etnia == "Ningún grupo étnico", 6,
                                                               ifelse(etnia == "Sin información", 99, NA))))))))

# Base municipal
nbi_col <- nbi %>%
  drop_na(etnia_val) %>%
  mutate(nivel_value = glue("{nivel_value}_{etnia_val}")) %>%
  mutate(id_data = 16, variable = "nbi", value_label = "NBI (%)", 
         id_nivel = "nacional_etnia", id_time = 1, time = 2018) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(nbi_col, glue("{datos}/base_nbi_col_etnia_2018.csv"))
rm(list = ls(pattern = "nbi"))
