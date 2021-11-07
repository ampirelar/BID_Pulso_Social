#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 20 oct, 2021
# Procesamiento de datos de Poblacion por clases sociales (pobreza monetaria)
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
# 1. Procesar datos originales ----
#-------------------------------------------------------#

# Leer base original
pob <- read_excel(glue("{datos_ori}/anexo-pobreza-monetaria-caracterizacion-clases-sociales-2020.xls"), sheet = "Clases Sociales Abs. Act.Met")

# Corregimos nombres de columnas
colnames(pob)[1] <- "variable"
col_names <- pob[9,] %>% janitor::row_to_names(row_number = 1) 
col_names <- names(col_names)
col_names <- col_names[!is.na(col_names)]
col_names <- c("variable", col_names)
pob <- pob %>% drop_na(variable) 

# Identificamos clases sociales 
pob$variable <- gsub("Población según clases sociales - ", "", pob$variable)
pob$variable <- gsub("23.*", "", pob$variable)
pob$variable <- trimws(pob$variable)

# Organizar anios 
pob <- pob %>% filter(!(variable == "Cifras en miles" | variable == "Dominio"))
names(pob) <- col_names
pob <- pob %>% 
  rename_at(vars(contains("20")), funs(paste0("year_", .))) %>% 
  janitor::clean_names() %>%
  mutate(across(starts_with("year"), ~ifelse(is.na(.x), variable, .x)))

# Creamos base de clases sociales 
pob_long <- pob %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "poblacion")

pobres <- pob_long[which(pob_long$variable == "Pobres")[1]:which(pob_long$variable == "Centros poblados y rural disperso")[9], ] %>%
  mutate(clase = "Pobres") %>% filter(variable != clase)

vulnerables <- pob_long[which(pob_long$variable == "Vulnerables")[1]:which(pob_long$variable == "Centros poblados y rural disperso")[9*2], ] %>%
  mutate(clase = "Vulnerables") %>% filter(variable != clase)

media <- pob_long[which(pob_long$variable == "Clase media")[1]:which(pob_long$variable == "Centros poblados y rural disperso")[9*3], ] %>%
  mutate(clase = "Clase media") %>% filter(variable != clase)

alta <- pob_long[which(pob_long$variable == "Clase alta")[1]:which(pob_long$variable == "Centros poblados y rural disperso")[9*4], ] %>%
  mutate(clase = "Clase alta") %>% filter(variable != clase)

# Pasamos poblacion de miles a personas
data_clases <- bind_rows(pobres, vulnerables, media, alta) %>%
  mutate(year = as.numeric(gsub("year_", "", year)), poblacion = 1000*as.numeric(poblacion))

rm(pobres, vulnerables, alta, media, pob, pob_long)

#-------------------------------------------------------#
# 2. Estadisticas por nivel de desagregacion ----
#-------------------------------------------------------#

# Poblacion total
pob_total <- data_clases %>%
  group_by(variable, year) %>%
  summarise(poblacion_total = sum(poblacion, na.rm = T)) %>%
  ungroup()

# Total nacional
clases_col <- data_clases %>% filter(variable == "Total nacional") %>%
  left_join(pob_total, by = c("variable", "year")) %>%
  mutate(id_data = 13, variable = "clase_social", 
  value_label = glue("Población por clase social (%) - {clase}"), 
         id_nivel = "nacional", id_time = 1, time = year,
         nivel_value = 1, value = 100*(poblacion/poblacion_total)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Areas metropolitanas
clases_am <- data_clases %>% 
  filter(variable != "Total nacional" & variable != "Cabeceras" & variable != "Centros poblados y rural disperso") %>%
  left_join(pob_total, by = c("variable", "year")) %>%
  mutate(id_data = 13, nivel_value = variable, variable = "clase_social", 
         value_label = glue("Población por clase social (%) - {clase}"), 
         id_nivel = "area_metropolitana", id_time = 1, time = year, 
         clase = gsub(" ", "_", clase), value = 100*(poblacion/poblacion_total)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Cabeceras y zonas rurales (centros poblados y rural disperso)
clases_zona <- data_clases %>% 
  filter(variable == "Cabeceras" | variable == "Centros poblados y rural disperso") %>%
  left_join(pob_total, by = c("variable", "year")) %>%
  mutate(id_data = 13, nivel_value = variable, variable = "clase_social", 
         value_label = glue("Población por clase social (%) - {clase}"), 
         id_nivel = "zona", id_time = 1, time = year, 
         value = 100*(poblacion/poblacion_total)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

df_clases <- data.frame(clases = c("pobres", "vulnerables", "clase media", "clase alta"), id = c(1, 2, 3, 4))

# Guardamos una base por clase y nivel territorial
lapply(unique(clases_col$value_label), function(x){
  
  print(x)
  clase <- gsub(".*-", "", x)
  clase <- tolower(clase) %>% str_trim()
  y <- df_clases %>% dplyr::filter(clases == clase) 
  print(clase)
  nacional <- clases_col %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{y$id}"))
  areas <- clases_am %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{y$id}"))
  zonas <- clases_zona %>% dplyr::filter(value_label == x) %>% mutate(variable = glue("{variable}_{y$id}"))
  
  # Exportar base
  write_csv(nacional, glue("{datos}/base_clases_sociales_{clase}_col_2012-2020.csv"))
  write_csv(areas, glue("{datos}/base_clases_sociales_{clase}_am_2012-2020.csv"))
  write_csv(zonas, glue("{datos}/base_clases_sociales_{clase}_zona_2012-2020.csv"))

})

rm(list = ls(pattern = "clase"))
