#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 28 sept, 2021
# Procesamiento de datos de EDS
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

datos_ori <- "Data/Encuesta_de_demografía_y_salud/Input"
datos <- "Data/Encuesta_de_demografía_y_salud/Output"
options(scipen = 999)

#-------------#
# Nacional ----
#-------------#

eds_nac <- read_excel(glue("{datos_ori}/DHS_full_data_2015.xlsx")) %>% janitor::clean_names()

eds_nac <- eds_nac %>% filter(country_name == "Colombia") %>% mutate(value = as.numeric(gsub(",", ".", value)))

eds_mod <- eds_nac %>% filter(characteristic_category == "Total" & indicator_id_short == "AMo") %>% 
  mutate(id_data = 3, variable = "anticonceptivo_modern", value_label = "Método de anticonceptivo moderno", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_anticonceptivo_modern_col_eds_1986-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Total" & indicator_id_short == "AAn") %>% 
  mutate(id_data = 3, variable = "anticonceptivo_algun", value_label = "Uso algún metodo anticonceptivo", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_anticonceptivo_algun_col_eds_1986-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Total" & indicator_id_short == "ADs") %>% 
  mutate(id_data = 3, variable = "dda_planif_mod_satis", value_label = "Demanda de planificación familiar satisfecha con métodos modernos", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_dda_planif_mod_satis_col_eds_1990-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Total" & indicator_id_short == "FP_CUSA_W_TRA") %>% 
  mutate(id_data = 3, variable = "tradicional_metodo", value_label = "Uso de algún método tradicional", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_tradicional_metodo_col_eds_1986-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Total" & indicator_id_short == "ATd") %>% 
  mutate(id_data = 3, variable = "total_dda_planificacion", value_label = "Total de demanda de métodos de planificación familiar", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_total_dda_planificacion_col_eds_1990-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Total" & indicator_id_short == "ATd") %>% 
  mutate(id_data = 3, variable = "neces_inst_planificacion", value_label = "Necesidad de planificación insatisfecha", 
         id_nivel = "nacional", nivel_value = 1, id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_neces_inst_planificacion_col_eds_1990-2015.csv"))
rm(eds_mod)

#-----------------#
#Departamental ----
#-----------------#

eds_mod <- eds_nac %>% filter(characteristic_category == "Region" & indicator_id_short == "AMo") %>% 
  mutate(id_data = 3, variable = "anticonceptivo_modern", value_label = "Método de anticonceptivo moderno", 
         id_nivel = "region", nivel_value = chartr("áéíóú", "aeiou",str_to_lower(gsub(" ", "_", characteristic_label))),
         id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>% arrange(nivel_value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_anticonceptivo_modern_reg_eds_1986-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Region" & indicator_id_short == "AAn") %>% 
  mutate(id_data = 3, variable = "anticonceptivo_algun", value_label = "Uso algún metodo anticonceptivo", 
         id_nivel = "region", nivel_value = chartr("áéíóú", "aeiou",str_to_lower(gsub(" ", "_", characteristic_label))),
         id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>% arrange(nivel_value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_anticonceptivo_algun_reg_eds_1986-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>%  filter (indicator_id_short == "ADs" )
eds_mod <- eds_mod %>% filter (characteristic_category == "Region" )
eds_mod <- eds_mod %>% filter(characteristic_label == c("Atlántica", "Oriental", "Pacífica", "Central", "Bogotá", "Territorios Nacionales")) %>%
  mutate(id_data = 3, variable = "dda_planif_mod_satis", value_label = "Demanda de planificación familiar satisfecha con métodos modernos", 
         id_nivel = "region", nivel_value = chartr("áéíóú", "aeiou",str_to_lower(gsub(" ", "_", characteristic_label))),
         id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>% arrange(nivel_value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_dda_planif_mod_satis_reg_eds_1990-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>% filter(characteristic_category == "Region" & indicator_id_short == "FP_CUSA_W_TRA") %>% 
  mutate(id_data = 3, variable = "tradicional_metodo", value_label = "Uso de algún método tradicional", 
         id_nivel = "region", nivel_value = chartr("áéíóú", "aeiou",str_to_lower(gsub(" ", "_", characteristic_label))),
         id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>% arrange(nivel_value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_tradicional_metodo_reg_eds_1986-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>%  filter (indicator_id_short == "ADs" )
eds_mod <- eds_mod %>% filter (characteristic_category == "Region" )
eds_mod <- eds_mod %>% filter(characteristic_label == c("Atlántica", "Oriental", "Pacífica", "Central", "Bogotá", "Territorios Nacionales")) %>% 
  mutate(id_data = 3, variable = "total_dda_planificacion", value_label = "Total de demanda de métodos de planificación familiar", 
         id_nivel = "region", nivel_value = chartr("áéíóú", "aeiou",str_to_lower(gsub(" ", "_", characteristic_label))),
         id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>% arrange(nivel_value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_total_dda_planificacion_reg_eds_1990-2015.csv"))
rm(eds_mod)

eds_mod <- eds_nac %>%  filter (indicator_id_short == "ADs" )
eds_mod <- eds_mod %>% filter (characteristic_category == "Region" )
eds_mod <- eds_mod %>% filter(characteristic_label == c("Atlántica", "Oriental", "Pacífica", "Central", "Bogotá", "Territorios Nacionales")) %>%
  mutate(id_data = 3, variable = "neces_inst_planificacion", value_label = "Necesidad de planificación insatisfecha", 
         id_nivel = "region", nivel_value = chartr("áéíóú", "aeiou",str_to_lower(gsub(" ", "_", characteristic_label))),
         id_time = 1) %>% rename(time = "survey_year_label") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>% arrange(nivel_value)

# Exportar
write_csv(eds_mod, glue("{datos}/base_neces_inst_planificacion_reg_eds_1990-2015.csv")) 
rm(eds_mod, eds_nac)


# Leer y organizar eds subnacionales

eds_dpto <- {lista <- list.files(path = glue("{datos_ori}/DHS_Subnacional/csv"), pattern = "sdr_subnational_.*")
  print(lista)
  data <- read_csv(glue("{datos_ori}/DHS_Subnacional/csv/{lista}")) %>%
    janitor::clean_names()}

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "fefrtrwtfr") %>%
  mutate(id_data = 3, variable = "fertilidad_tres_ant", value_label = "Tasa de fertilidad últimos tres años en mujeres de 15 a 49 años", 
       id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_fertilidad_tres_ant_dpto_eds_2005-2015.csv"))
rm(eds_dpto)

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "maaafmwm2b") %>%
  mutate(id_data = 3, variable = "edad_prom_matrimonio", value_label = "Edad promedio del primer matrimonio", 
         id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_edad_prom_matrimonio_dpto_eds_2005-2015.csv"))
rm(eds_dpto)

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "sxaafswm2b") %>%
  mutate(id_data = 3, variable = "edad_prom_sex", value_label = "Edad promedio del primer encuentro sexual de mujeres entre 15 y 49 años", 
         id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_edad_prom_sex_dpto_eds_2005-2015.csv"))
rm(eds_dpto)

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "cmecmrcimr") %>%
  mutate(id_data = 3, variable = "mortalidad_inf_eds", value_label = "Tasa de mortalidad infantil en los últimos 10 años", 
         id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_mortalidad_inf_eds_dpto_eds_2005-2015.csv"))
rm(eds_dpto)

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "cmecmrcu5m") %>%
  mutate(id_data = 3, variable = "mortalidad_inf_cinco", value_label = "Tasa de mortalidad infantil en menores de 5 años en los últimos 10 años", 
         id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_mortalidad_inf_cinco_dpto_eds_2005-2015.csv"))
rm(eds_dpto)

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "rhdelpcdhf") %>%
  mutate(id_data = 3, variable = "nacidos_vivos", value_label = "Nacidos vivos en los últimos 5 años (%)", 
         id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_nacidos_vivos_dpto_eds_2005-2015.csv"))
rm(eds_dpto)

eds_dpto <- data %>% filter(levelna == "Departments") %>% rename(nivel_value = "regcode", value = "ededucwseh") %>%
  mutate(id_data = 3, variable = "mujer_secund_super_educ", value_label = "Mujeres con educación secundaria o superior (%)", 
         id_nivel = "dpto", id_time = 1) %>% rename(time = "svyyear") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(eds_dpto, glue("{datos}/base_mujer_secund_super_educ_dpto_eds_2005-2015.csv"))
rm(eds_dpto, data)

