#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 28 sept, 2021
# Procesamiento de datos de Defunciones 2010-2018 + 2019
# Se crean 2 grupos de datos, 2010-2017 y 2018-2020 dado
# que las causas de muerte se catalogan diferente
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

datos_ori <- "Data/Nacimientos_y_defunciones/Input"
datos <- "Data/Nacimientos_y_defunciones/Output"
options(scipen = 999)

#----------------------------------#
#Suicidios Nacional----
#----------------------------------#

def_col19 <- read_csv(glue("{datos}/defunciones_causas_col_2018-2020.csv"))
def_col10 <- read_csv(glue("{datos}/defunciones_causas_col_2010-2017.csv"))

right = function(x,n){
  substring(x,nchar(x)-n+1)
}

x <- c("2", "3", "5", "9", "8")

tot <- read_csv(glue("Data/Proyecciones_poblacion/Input/poblacion_dpto_2005-2026.csv"))

tot <- tot %>% group_by(year) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% rename(time = "year")

def_col19 <- def_col19 %>% mutate(codigo = as.character(right(nivel_value,3)),
                                  variable = "defunciones_ent", 
                                  value_label = "Número de defunciones por enfermedades no transmisibles")

def_col19$codigo[def_col19$codigo == "605"] <- 8
def_col19$codigo[def_col19$codigo == "606"] <- 8
def_col19$codigo[def_col19$codigo == "601"] <- 9

def_col19 <- def_col19 %>% mutate(cod = substr(codigo, 1, 1)) %>%
  filter( cod %in% x) %>% 
  group_by(time, cod) %>%
  summarise(value = sum(value)) %>% 
  ungroup() %>% mutate(variable = "defunciones_ent",
                       value_label = "Número de defunciones por enfermedades no transmisibles",
                       rango = ifelse(cod < 3, "cancer", 
                                      ifelse(cod == 3, "cardiovascular",
                                             ifelse(cod == 5, "otros",
                                                    ifelse(cod == 8, "respiratorias", "diabetes")))),
                       id_data = 19, id_nivel = "nacional_causa", nivel_value = glue("{1}_{rango}"),
                       id_time = 1) %>% left_join(tot, by = "time") %>% mutate(value = (value/poblacion)*100000) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_col10 <- def_col10 %>%  mutate(codigo = as.character(right(nivel_value,3)),
                                   variable = "defunciones_ent",
                                   value_label = "Número de defunciones por enfermedades no transmisibles") 

def_col10$codigo[def_col10$codigo == "605"] <- 8
def_col10$codigo[def_col10$codigo == "606"] <- 8
def_col10$codigo[def_col10$codigo == "601"] <- 9

def_col10 <- def_col10 %>% mutate(cod = substr(codigo, 1, 1)) %>%
  filter( cod %in% x) %>% 
  group_by(time, cod) %>%
  summarise(value = sum(value)) %>% 
  ungroup() %>% mutate(variable = "defunciones_ent",
                       value_label = "Número de defunciones por enfermedades no transmisibles",
                       rango = ifelse(cod < 3, "cancer", 
                                      ifelse(cod == 3, "cardiovascular",
                                             ifelse(cod == 5, "otros",
                                                    ifelse(cod == 8, "respiratorias", "diabetes")))),
                       id_data = 19, id_nivel = "nacional_causa", nivel_value = glue("{1}_{rango}"),
                       id_time = 1) %>% left_join(tot, by = "time") %>% mutate(value = (value/poblacion)*100000) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar
write_csv(def_col10, glue("{datos}/base_defunciones_ent_col_2010-2017.csv"))
write_csv(def_col19, glue("{datos}/base_defunciones_ent_col_2018-2020.csv"))

#----------------------------------#
#Suicidios Departamental----
#----------------------------------#

def_dpto19 <- read_csv(glue("{datos}/defunciones_causas_dpto_2018-2020.csv"))
def_dpto10 <- read_csv(glue("{datos}/defunciones_causas_dpto_2010-2017.csv"))

def_dpto19 <- def_dpto19 %>% mutate(codigo = as.character(right(nivel_value,3)),
                                  variable = "defunciones_ent",
                                  value_label = "Número de defunciones por enfermedades no transmisibles",
                                  cod_dpto = as.numeric(substr(nivel_value, 1, 2)))

def_dpto19$codigo[def_dpto19$codigo == "605"] <- 8
def_dpto19$codigo[def_dpto19$codigo == "606"] <- 8
def_dpto19$codigo[def_dpto19$codigo == "601"] <- 9

def_dpto19 <- def_dpto19 %>% mutate(cod = substr(codigo, 1, 1)) %>%
  filter( cod %in% x) %>% 
  group_by(cod_dpto, time, cod) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>% mutate(variable = "defunciones_ent",
                       value_label = "Número de defunciones por enfermedades no transmisibles",
                       rango = ifelse(cod < 3, "cancer", 
                                      ifelse(cod == 3, "cardiovascular",
                                             ifelse(cod == 5, "otros",
                                                    ifelse(cod == 8, "respiratorias", "diabetes")))),
                       id_data = 19, id_nivel = "dpto_causa", nivel_value = glue("{cod_dpto}_{rango}"),
                       id_time = 1) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_dpto10 <- def_dpto10 %>%  mutate(codigo = as.character(right(nivel_value,3)),
                                     variable = "defunciones_ent",
                                     value_label = "Número de defunciones por enfermedades no transmisibles",
                                     cod_dpto = as.numeric(substr(nivel_value, 1, 2)))

def_dpto10$codigo[def_dpto10$codigo == "605"] <- 8
def_dpto10$codigo[def_dpto10$codigo == "606"] <- 8
def_dpto10$codigo[def_dpto10$codigo == "601"] <- 9

def_dpto10 <- def_dpto10 %>%  mutate(cod = substr(codigo, 1, 1)) %>%
  filter( cod %in% x) %>% 
  group_by(cod_dpto, time, cod) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>% mutate(variable = "defunciones_ent",
                       value_label = "Número de defunciones por enfermedades no transmisibles",
                       rango = ifelse(cod < 3, "cancer", 
                                      ifelse(cod == 3, "cardiovascular",
                                             ifelse(cod == 5, "otros",
                                                    ifelse(cod == 8, "respiratorias", "diabetes")))),
                       id_data = 19, id_nivel = "dpto_causa", nivel_value = glue("{cod_dpto}_{rango}"),
                       id_time = 1) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar

write_csv(def_dpto10, glue("{datos}/base_defunciones_ent_dpto_2010-2017.csv"))
write_csv(def_dpto19, glue("{datos}/base_defunciones_ent_dpto_2018-2020.csv"))

rm(def_dpto10, def_dpto19)

#----------------------------------#
#Suicidios Municipal----
#----------------------------------#

def_mpio19 <- read_csv(glue("{datos}/base_defunciones_causas_mpio_2018-2020.csv"))
def_mpio10 <- read_csv(glue("{datos}/base_defunciones_causas_mpio_2010-2017.csv"))

def_mpio19 <- def_mpio19 %>% mutate(codigo = as.character(right(nivel_value,3)),
                                    variable = "defunciones_ent",
                                    value_label = "Número de defunciones por enfermedades no transmisibles") %>%
  filter(substr(codigo, 1, 1) %in% x) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

def_mpio10 <- def_mpio10 %>%  mutate(codigo = as.character(right(nivel_value,3)),
                                     variable = "defunciones_ent",
                                     value_label = "Número de defunciones por enfermedades no transmisibles") %>%
  filter(substr(codigo, 1, 1) %in% x) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar

write_csv(def_mpio10, glue("{datos}/base_defunciones_ent_mpio_2010-2017.csv"))
write_csv(def_mpio19, glue("{datos}/base_defunciones_ent_mpio_2018-2020.csv"))






