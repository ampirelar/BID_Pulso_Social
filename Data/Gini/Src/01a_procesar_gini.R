#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 21 sept, 2021
# Procesamiento de datos de Indice GINI
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

datos_ori <- "Data/Gini/Input"
datos <- "Data/Gini/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Departamental y nacional ----
#-------------------------------------------------------#

# Etiquetas nombres departamentos y zonas
nom_dpto <- read_xlsx("Descriptives/Herramientas/Input/base_nombres_departamentos.xlsx")
zonas <- c("Cabeceras", "Otras cabeceras", "Centros poblados y rural disperso")

# Leer base original
gini <- read_excel(glue("{datos_ori}/anexo_pobreza_monetaria_20_departamento.xls"), sheet = "Gini") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) 

# Organizar datos
colnames(gini)[1] <- "departamento"
gini <- gini %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(.cols = starts_with("year"), .fns = as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "gini") %>%
  mutate(year = as.numeric(gsub("year_", "", year)), id_data = 13, variable = "gini", 
         value_label = "Coeficiente de Gini", id_time = 1, time = year, value = gini ) %>%
  rename(nivel_label = departamento)

# Total departamental anual
gini_dpto <- gini %>% 
  left_join(nom_dpto, by = "nivel_label") %>%
  mutate(id_nivel = "dpto") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value)

# Nacional
gini_col <- gini %>% 
  filter(nivel_label == "Total Nacional") %>%
  mutate(nivel_value = 1, id_nivel = "nacional") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar datos
write_csv(gini_dpto, glue("{datos}/base_gini_dpto_2002-2020.csv"))
write_csv(gini_col, glue("{datos}/base_gini_col_2002-2020.csv"))
rm(list = ls(pattern = "gini"))

#-------------------------------------------------------#
# 2. Zonas y principales ciudades ----
#-------------------------------------------------------#

zonas <- c("Cabeceras", "Otras cabeceras", "Centros poblados y rural disperso")

# Leer base original
gini <- read_excel(glue("{datos_ori}/anexo_pobreza_monetaria_20_nacional.xls"), sheet = "Gini") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) 

# Organizar datos
colnames(gini)[1] <- "zona"
gini <- gini %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(starts_with("year"), as.numeric)) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "gini") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  rename(nivel_label = zona)

# Areas metropolitanas
gini_am <- gini %>% 
  filter(nivel_label != "Nacional" & !(nivel_label %in% zonas)) %>%
  mutate(id_data = 13, nivel_value = glue("{nivel_label}_gini"), variable = "gini", 
         value_label = "Coeficiente de Gini", id_nivel = "area_metropolitana", 
         id_time = 1, time = year, value = gini, nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Cabeceras y zonas rurales (centros poblados y rural disperso)
gini_zonas <- gini %>% 
  filter(nivel_label %in% zonas) %>%
  mutate(id_data = 13, variable = "gini", value_label = "Coeficiente de Gini", 
         id_nivel = "zona", id_time = 1, time = year, nivel_value = glue("{nivel_label}_gini"), 
         value = gini, nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(gini_am, glue("{datos}/base_gini_am_2002-2020.csv"))
write_csv(gini_zonas, glue("{datos}/base_gini_zonas_2002-2020.csv"))

rm(list = ls(pattern = "gini"))

#-------------------------------------------------------#
# 3. Internacional ----
#-------------------------------------------------------#

# Leer base original
gini <- read_csv(glue("{datos_ori}/economic-inequality-gini-index.csv")) %>%
  janitor::clean_names() %>%
  rename(nivel_label = entity, time = year, value = gini_index) %>%
  mutate(id_data = 13, variable = "gini", value_label = "Coeficiente de Gini", 
         id_nivel = "pais", id_time = 1, value = as.numeric(value))

# Pegamos codigos paises
countries <- read_xlsx("Descriptives/Herramientas/Input/base_codigos_paises.xlsx") %>%
  select(-iso_codes)

gini$nivel_label[gini$nivel_label == "Cote d'Ivoire"] <- "Ivory Coast"
gini$nivel_label[gini$nivel_label == "Czechia"] <- "Czech Republic"
gini$nivel_label[gini$nivel_label == "Eswatini"] <- "Swaziland"
gini$nivel_label[gini$nivel_label == "North Macedonia"] <- "Macedonia"
gini$nivel_label[gini$nivel_label == "Timor"] <- "East Timor"
gini$nivel_label[gini$nivel_label == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
gini$nivel_label[gini$nivel_label == "Congo"] <- "Republic of the Congo"

# Pegamos codigos de pais
gini <- gini %>% left_join(countries, by = "nivel_label")
colSums(is.na(gini))

# Correcciones en codigos de pais
gini$nivel_value[str_detect(gini$nivel_label, "China")] <- 86
gini$nivel_value[str_detect(gini$nivel_label, "India")] <- 91
gini$nivel_value[str_detect(gini$nivel_label, "Argentina")] <- 54
gini$nivel_value[str_detect(gini$nivel_label, "Indonesia")] <- 62
gini$nivel_value[str_detect(gini$nivel_label, "Suriname")] <- 597
gini$nivel_value[str_detect(gini$nivel_label, "Micronesia")] <- 691

gini_country <- gini %>%
  select(id_data, variable, id_nivel, nivel_label, nivel_value, id_time, time, value_label, value)

# Exportamos datos
write_csv(gini_country, glue("{datos}/base_gini_pais_1981-2019.csv"))
