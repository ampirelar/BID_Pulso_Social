#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 14 oct, 2021
# Procesamiento de datos de homicidios 2019 -2020
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

#Organizar bases de datos

def19 <- read_excel(glue("{datos_ori}/2019/defunciones2019-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>% .[,c(1:8)] %>% 
  fill(na_4,.direction = "down") %>% fill(na_3,.direction = "down") %>%
  mutate(cod_enf = as.character(substr(na_5, 1, 3))) %>%
  rename(dpto = na_3, mpio = na_4) %>% filter(cod_enf == "512") %>%
  mutate(time = 2019) %>% rename(value = na_6)

def20 <- read_excel(glue("{datos_ori}/2020/def2020p-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names()%>% .[,c(1:8)] %>% 
  fill(na_4,.direction = "down") %>% fill(na_3,.direction = "down") %>%
  mutate(cod_enf = as.character(substr(na_5, 1, 3))) %>%
  rename(dpto = na_3, mpio = na_4) %>% filter(cod_enf == "512") %>%
  mutate(time = 2020) %>% rename(value = na_6)

tot <- read_csv(glue("Data/Proyecciones_poblacion/Input/poblacion_dpto_2005-2026.csv"))

totn <- tot %>% group_by(year) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% rename(time = "year")

#------------#
# Nacional----
#------------#

nac <- def19 %>% filter(is.na(mpio)) %>% left_join(totn, by = "time") %>%
  mutate(value = (value/poblacion)*100000, id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)",
         id_nivel = "nacional", id_time = 1, nivel_value = glue("1")) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_ <- def20 %>% filter(is.na(mpio)) %>%left_join(totn, by = "time") %>%
  mutate(value = (value/poblacion)*100000, id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)",
         id_nivel = "nacional", id_time = 1, nivel_value = glue("1")) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_col <- rbind(nac, nac_)

#-----------------#
# Departamental----
#-----------------#


dpto <- def19 %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", value = (value/poblacion)*100000,
         value_label = "Agresiones (homicidios)", id_nivel = "dpto",
         id_time = 1, nivel_value = as.numeric(glue("{na}"))) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

dpto_ <- def20 %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", value = (value/poblacion)*100000,
         value_label = "Agresiones (homicidios)", id_nivel = "dpto",
         id_time = 1, nivel_value = as.numeric(glue("{na}"))) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_dpto <- rbind(dpto, dpto_)

#--------------#
# Municipal ----
#--------------#

mpio <- def19 %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)", id_nivel = "mpio",
         id_time = 1, nivel_value = glue("{na_2}")) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

mpio_ <- def20 %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)", id_nivel = "mpio",
         id_time = 1, nivel_value = glue("{na_2}")) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_mpi <- rbind(mpio, mpio_)

#Extraer

write_csv(nac_col, glue("{datos}/homicidios_col_2019-2020.csv"))
write_csv(nac_dpto, glue("{datos}/homicidios_dpto_2019-2020.csv"))
write_csv(nac_mpi, glue("{datos}/homicidios_mpio_2019-2020.csv"))

rm(dpto, dpto_, nac_dpto, nac_mpi, mpio, mpio_, nac_col, nac_, nac)

#------------------------#
# homicidios por género----
#------------------------#

#------------#
# Nacional----
#------------#
tot_def <- rbind(def19, def20)  %>% .[,-c(6)]

nac <- tot_def %>% filter(is.na(mpio)) %>% left_join(totn, by = "time") %>%
  mutate(value = hombres, value = (as.numeric(value)/poblacion)*100000, id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)",
         id_nivel = "nacional_gen", id_time = 1, nivel_value = "1_1") %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_ <- tot_def %>% filter(is.na(mpio)) %>% left_join(totn, by = "time") %>%
  mutate(value = mujeres, value = (as.numeric(value)/poblacion)*100000, id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)",
         id_nivel = "nacional_gen", id_time = 1, nivel_value = "1_2") %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_col <- rbind(nac, nac_) 

write_csv(nac_col, glue("{datos}/base_homicidios_genero_col_2019-2020.csv")) 

rm(nac_col, nac_, nac, def19, def20)

#-----------------#
# Departamental----
#-----------------#

dpto <- tot_def %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", value = (as.numeric(hombres)/poblacion)*100000,
         value_label = "Agresiones (homicidios)", id_nivel = "dpto_gen",
         id_time = 1, nivel_value = glue("{as.numeric(na)}_1")) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

dpto_ <- tot_def %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "homicidios", value = (as.numeric(mujeres)/poblacion)*100000,
         value_label = "Agresiones (homicidios)", id_nivel = "dpto_gen",
         id_time = 1, nivel_value = glue("{as.numeric(na)}_2")) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_dpto <- rbind(dpto, dpto_) 

write_csv(nac_dpto, glue("{datos}/homicidios_genero_dpto_2019-2020.csv"))

rm(dpto, dpto_, nac_dpto)

#--------------#
# Municipal ----
#--------------#

mpio <- tot_def %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)", id_nivel = "mpio_gen",
         id_time = 1, nivel_value = glue("{na_2}_1"), value = hombres) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

mpio_ <- tot_def %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)", id_nivel = "mpio_gen",
         id_time = 1, nivel_value = glue("{na_2}_2"), value = mujeres) %>% filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

nac_mpi <- rbind(mpio, mpio_) %>% arrange(nac_mpi$time)

write_csv(nac_mpi, glue("{datos}/base_homicidios_genero_mpio_2019-2020.csv"))

rm(mpio_, mpio, nac_mpi, tot_def)

#------------------------#
# homicidios por edad----
#------------------------#

#Organizar bases de datos

def19 <- read_excel(glue("{datos_ori}/2019/defunciones2019-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>% 
  fill(na_4,.direction = "down") %>% fill(na_3,.direction = "down") %>%
  mutate(cod_enf = as.character(substr(na_5, 1, 3))) %>%
  rename(dpto = na_3, mpio = na_4) %>% filter(cod_enf == "512") %>%
  mutate(time = 2019) %>% rename(value = na_6)

cols = c(7:36)   
def19[, cols] = apply(def19[, cols], 2, function(x) as.numeric(as.character(x)))

def20 <- read_excel(glue("{datos_ori}/2020/def2020p-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names()%>% 
  fill(na_4,.direction = "down") %>% fill(na_3,.direction = "down") %>%
  mutate(cod_enf = as.character(substr(na_5, 1, 3))) %>%
  rename(dpto = na_3, mpio = na_4) %>% filter(cod_enf == "512") %>%
  mutate(time = 2020) %>% rename(value = na_6)

def20[, cols] = apply(def20[, cols], 2, function(x) as.numeric(as.character(x)))

#------------#
# Nacional----
#------------#

tot_def <- rbind(def19, def20)  %>% .[,-c(6:15)] 
tot_def <- tot_def %>% mutate(edad_5_a_14 = rowSums(.[6:8]), edad_15_a_44 = rowSums(.[9:11]),
                              edad_45_a_64 = rowSums(.[12:14]), edad_65_o_mas = rowSums(.[15:23])) %>% 
  select(na, na_2, dpto, mpio, edad_5_a_14, edad_15_a_44, edad_45_a_64, edad_65_o_mas, time) %>%
  pivot_longer(cols = starts_with("edad_"), names_to = "edad", values_to = "homicidios") %>%
  mutate(edad = as.character(gsub("edad_", "", edad)))

nac <- tot_def %>% filter(is.na(mpio)) %>% left_join(totn, by = "time") %>%
  mutate(value = homicidios, value = (as.numeric(value)/poblacion)*100000, id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)",
         id_nivel = "nacional_edad", id_time = 1, nivel_value = glue("1_{edad}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(nac, glue("{datos}/homicidios_edad_col_2019-2020.csv")) 

rm(nac, def19, def20)

#-----------------#
# Departamental----
#-----------------#

dpto <- tot_def %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, 
         variable = "homicidios", value = (as.numeric(homicidios)/poblacion)*100000,
         value_label = "Agresiones (homicidios)", id_nivel = "dpto_edad",
         id_time = 1, nivel_value = glue("{as.numeric(na)}_{edad}"), nivel_label = glue("{nom_dpto}_{edad}"),
         nivel_label = gsub(" ", "_", nivel_label)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

write_csv(dpto, glue("{datos}/homicidios_edad_dpto_2019-2020.csv"))

rm(dpto)

#--------------#
# Municipal ----
#--------------#

mpio <- tot_def %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(id_data = 19, variable = "homicidios",
         value_label = "Agresiones (homicidios)", id_nivel = "mpio_gen",
         id_time = 1, nivel_value = glue("{na_2}_{edad}"), value = homicidios) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(mpio, glue("{datos}/base_homicidios_edad_mpio_2019-2020.csv"))

rm(tot_def)




