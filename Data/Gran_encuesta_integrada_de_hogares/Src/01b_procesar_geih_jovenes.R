#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 22 sept, 2021
# Procesamiento de datos de GEIH: Tasa de desempleo juventud (14-28 anios)
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

datos_ori <- "Data/Gran_encuesta_integrada_de_hogares/Input"
datos <- "Data/Gran_encuesta_integrada_de_hogares/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Nacional ----
#-------------------------------------------------------#

# Abrimos datos originales y organizamos
joven <- read_excel(glue("{datos_ori}/anexo_GEIH_juventud_abr21_jun21.xls"), sheet = "Tnal Trimestre Movil ") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  dplyr::select(na, starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic"))) %>%
  rename(variable = na)

joven <- joven[which(joven$variable == "TD")[1]:which(joven$variable == "TD")[3], ]
joven <- joven %>% filter(variable == "TD") 
joven$variable[1] <- "TD-total"
joven$variable[2] <- "TD-hombres"
joven$variable[3] <- "TD-mujeres"

# Wide a long
joven <- joven %>% 
  rename(ene_mar_1 = ene_mar, abr_jun_1 = abr_jun, jul_sep_1 = jul_sep, oct_dic_1 = oct_dic) %>%
  pivot_longer(cols = starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic")), names_to = "trim", values_to = "td") %>%
  mutate(year = as.numeric(gsub(".*_", "",trim)), year = year+2000, td = as.numeric(td)) %>%
  arrange(variable, year)

# Nacional
joven_col <- joven %>%
  filter(variable == "TD-total") %>%
  group_by(year, variable) %>%
  summarise(td = mean(td)) %>%
  ungroup() %>%
  mutate(id_data = 1, variable = "tasa_desempleo", value_label = "Tasa de desempleo", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1, time = year, value = td) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Nacional por genero
joven_col_gen <- joven %>%
  filter(variable != "TD-total") %>%
  group_by(year, variable) %>%
  summarise(td = mean(td)) %>%
  ungroup() %>%
  mutate(id_data = 1, value_label = "Tasa de desempleo", id_nivel = "nacional", 
         id_time = 1, time = year, value = td,
         nivel_value = ifelse(variable == "TD-hombres", "1_1", "1_2"),
         variable = "tasa_desempleo",) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  arrange(nivel_value, time)

# Exportar datos
write_csv(joven_col, glue("{datos}/base_tasa_desempleo_joven_col_2001-2021.csv"))
write_csv(joven_col_gen, glue("{datos}/base_tasa_desempleo_joven_col_gen_2001-2021.csv"))
rm(joven, joven_col, joven_col_gen)
  
#-------------------------------------------------------#
# 2. Zonas (cabecera, centros poblados) ----
#-------------------------------------------------------#

# Abrimos datos originales y organizamos
joven <- read_excel(glue("{datos_ori}/anexo_GEIH_juventud_abr21_jun21.xls"), sheet = "Tnal Trimestre Movil ") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  dplyr::select(na, starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic"))) %>%
  rename(variable = na)

joven <- joven[which(joven$variable == "TD")[4]:which(joven$variable == "TD")[9], ]
joven <- joven %>% filter(variable == "TD") 
joven$variable[1] <- "TD-cabecera-total"
joven$variable[2] <- "TD-cabecera-hombres"
joven$variable[3] <- "TD-cabecera-mujeres"
joven$variable[4] <- "TD-rural-total"
joven$variable[5] <- "TD-rural-hombres"
joven$variable[6] <- "TD-rural-mujeres"

# Wide a long
joven <- joven %>% 
  rename(ene_mar_1 = ene_mar, abr_jun_1 = abr_jun, jul_sep_1 = jul_sep, oct_dic_1 = oct_dic) %>%
  pivot_longer(cols = starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic")), names_to = "trim", values_to = "td") %>%
  mutate(year = as.numeric(gsub(".*_", "",trim)), year = year+2000, td = as.numeric(td)) %>%
  arrange(variable, year)

# Total Cabeceras y Centros poblados
joven_col <- joven %>%
  filter(variable == "TD-cabecera-total" | variable == "TD-rural-total") %>%
  group_by(year, variable) %>%
  summarise(td = mean(td)) %>%
  ungroup() %>%
  mutate(id_data = 1, value_label = "Tasa de desempleo", 
         id_nivel = "zona", id_time = 1, time = year, value = td,
         nivel_value = ifelse(variable == "TD-cabecera-total", "1_Cabecera", "1_Centros_Poblados_y_Rural_Disperso"),
         variable = "tasa_desempleo") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Zonas por genero
joven_col_gen <- joven %>%
  filter(variable != "TD-cabecera-total" & variable != "TD-rural-total") %>%
  group_by(year, variable) %>%
  summarise(td = mean(td)) %>%
  ungroup() %>%
  mutate(id_data = 1, value_label = "Tasa de desempleo", id_nivel = "nacional", id_time = 1, time = year, value = td)

joven_col_gen$variable[joven_col_gen$variable == "TD-cabecera-hombres"] <- "1_Cabecera_1"
joven_col_gen$variable[joven_col_gen$variable == "TD-cabecera-mujeres"] <- "1_Cabecera_2"
joven_col_gen$variable[joven_col_gen$variable == "TD-rural-hombres"] <- "1_Centros_Poblados_y_Rural_Disperso_1"
joven_col_gen$variable[joven_col_gen$variable == "TD-rural-mujeres"] <- "1_Centros_Poblados_y_Rural_Disperso_2"

joven_col_gen <- joven_col_gen %>%
  mutate(nivel_value = variable, variable = "tasa_desempleo") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  arrange(nivel_value, time)

# Exportar datos
write_csv(joven_col, glue("{datos}/base_tasa_desempleo_joven_zonas_2001-2021.csv"))
write_csv(joven_col_gen, glue("{datos}/base_tasa_desempleo_joven_zonas_gen_2001-2021.csv"))
rm(joven, joven_col, joven_col_gen)

#-------------------------------------------------------#
# 3. 23 ciudades y areas ----
#-------------------------------------------------------#

# Abrimos datos originales y organizamos
joven <- read_excel(glue("{datos_ori}/anexo_GEIH_juventud_abr21_jun21.xls"), sheet = "23 áreas trim movil") 
joven <- joven %>% .[-c(1:7), ] %>% janitor::row_to_names(row_number = 1) 
colnames(joven)[1] <- "variable"

ciudades <- c("BOGOTÁ", "MEDELLÍN A.M", "CALI A.M.", "BARRANQUILLA A.M.", "BUCARAMANGA A.M.",
              "MANIZALES A.M.", "PASTO", "PEREIRA A.M.", "CÚCUTA A.M.", "IBAGUÉ", "MONTERÍA",
              "CARTAGENA", "VILLAVICENCIO", "TUNJA", "FLORENCIA", "POPAYÁN", "VALLEDUPAR",
              "QUIBDÓ", "NEIVA", "RIOHACHA", "SANTA MARTA", "ARMENIA", 'SINCELEJO')

joven <- joven %>% 
  drop_na(variable) %>% janitor::clean_names() %>% 
  filter(variable == "TD" | variable %in% ciudades) %>%
  dplyr::select(variable, starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic"))) %>%
  rename(ene_mar_1 = ene_mar, abr_jun_1 = abr_jun, jul_sep_1 = jul_sep, oct_dic_1 = oct_dic)

# Pasamos de wide a long
joven <- joven %>% 
  fill(starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic")),.direction = "up") %>%
  pivot_longer(cols = starts_with(c("ene_mar", "abr_jun", "jul_sep", "oct_dic")), names_to = "trim", values_to = "td") %>%
  filter(variable != "TD") %>%
  mutate(year = as.numeric(gsub(".*_", "",trim)), year = year+2006, td = as.numeric(td))
  
# Estructuramos base 
joven_am <- joven %>%
  rename(nivel_label = variable, value = td, time = year) %>%
  group_by(time, nivel_label) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  mutate(id_data = 1, variable = "tasa_desempleo", value_label = "Tasa de desempleo", 
         id_nivel = "area_metropolitana", id_time = 1, 
         nivel_value = str_to_title(nivel_label), nivel_value = gsub(" ", "_", nivel_value)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  arrange(nivel_value, time)

# Exportar datos
write_csv(joven_am, glue("{datos}/base_tasa_desempleo_joven_am_2007-2021.csv"))
rm(joven, joven_am)


