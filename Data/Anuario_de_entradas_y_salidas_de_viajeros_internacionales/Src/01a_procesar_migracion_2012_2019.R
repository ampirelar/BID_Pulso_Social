#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 14 oct, 2021
# Procesamiento de datos de Migración
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

datos_ori <- "Data/Anuario_de_entradas_y_salidas_de_viajeros_internacionales/Input"
datos <- "Data/Anuario_de_entradas_y_salidas_de_viajeros_internacionales/Output"
options(scipen = 999)

# Leer y organizar migracion a nivel nacional

info <- read_xlsx(glue("{datos_ori}/Anuario_Anexos_2019_movimientos_internacionales.xlsx"), sheet = "Cuadro 1") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  mutate(id_data = 11, variable = "saldo", id_nivel = "nacional", nivel_value = 1,
         id_time = 1, time = ano, value_label = "Saldo de entradas y salidas", value = as.numeric(saldo)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(info, glue("{datos}/base_saldo_col_2012-2019.csv"))

# colombianos - extranjeros----

info <- read_xlsx(glue("{datos_ori}/Anuario_Anexos_2019_movimientos_internacionales.xlsx"), sheet = "Cuadro 2") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  mutate(id_data = 11, variable = "saldo", id_nivel = "nacional_col_extr", nivel_value = "1_extranjeros",
         id_time = 1, time = na, value_label = "Saldo de entradas y salidas", value = as.numeric(extranjero_2)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

info2 <- read_xlsx(glue("{datos_ori}/Anuario_Anexos_2019_movimientos_internacionales.xlsx"), sheet = "Cuadro 2") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  mutate(id_data = 11, variable = "saldo", id_nivel = "nacional_col_extr", nivel_value = "1_colombianos",
         id_time = 1, time = na, value_label = "Saldo de entradas y salidas", value = as.numeric(colombiano_2)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

info <- rbind(info, info2)

write_csv(info, glue("{datos}/base_saldo_nac_extr_col_2012-2019.csv"))

# Genero----

info <- read_xlsx(glue("{datos_ori}/Anuario_Anexos_2019_movimientos_internacionales.xlsx"), sheet = "Cuadro 3") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  fill(na,.direction = "down")

cols = c(3:8)
info[, cols] <- apply(info[, cols], 2, function(x) as.numeric(as.character(x)))

info2 <- info %>% filter(na == "Entradas")  %>%
  pivot_longer(cols = starts_with("x20"), names_to = "time", values_to = "value") 

info <- info %>% filter(na == "Salidas") %>%
  pivot_longer(cols = starts_with("x20"), names_to = "time", values_to = "value") 

info2 <- info2 %>% left_join(info, by = c("na_2", "time"))

info2$na_2[info2$na_2 == "Hombres"] <- 1
info2$na_2[info2$na_2 == "Mujeres"] <- 2

info2 <- info2 %>% filter(na_2 != "Total") %>%
  mutate(id_data = 11, variable = "saldo", id_nivel = "nacional_gen", nivel_value = glue("1_{na_2}"),
         id_time = 1, value_label = "Saldo de entradas y salidas", value = value.x - value.y,
         time = gsub("x", "", time)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(info2, glue("{datos}/base_saldo_gen_col_2012-2019.csv"))

# Edad----

info <- read_xlsx(glue("{datos_ori}/Anuario_Anexos_2019_movimientos_internacionales.xlsx"), sheet = "Cuadro 4") %>%
  rename(edad = "Anuario de entradas y salidas de viajeros internacionales") %>%
  mutate(time = ifelse(edad == 2012, 2012,
                       ifelse(edad == 2013, 2013,
                              ifelse(edad == 2014, 2014,
                                     ifelse(edad == 2015, 2015,
                                            ifelse(edad == 2016, 2016,
                                                   ifelse(edad == 2017, 2017,
                                                          ifelse(edad == 2018, 2018,
                                                                 ifelse(edad == 2019, 2019, NA))))))))) %>%
  fill(time,.direction = "down") %>%
  drop_na(`...3`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>% drop_na(na)

cols = c(2:7)
info[, cols] <- apply(info[, cols], 2, function(x) as.numeric(as.character(x)))

info <- info %>% mutate(value = (mujeres_2 + hombres_2) - (mujeres + hombres), time = x2012,
                        na = as.numeric(substr(na, 1, 2)), 
                        na = ifelse(na < 15, 1,
                                    ifelse(na > 14 & na < 29, 2,
                                           ifelse(na > 29 & na < 64, 3, 4))), edad = na) %>%
  select(na, time, value, edad) %>% drop_na() %>%
  group_by(na, time, edad) %>%
  summarise(value = sum(value)) %>%
  ungroup()

info$edad[info$edad == 1] <- "0_a_15_anios"
info$edad[info$edad == 2] <- "16_a_30_anios"
info$edad[info$edad == 3] <- "31_a_65_anios"
info$edad[info$edad == 4] <- "66_mas_anios"

info <- info %>% mutate(id_data = 11, variable = "saldo", id_nivel = "nacional_edad", nivel_value = glue("1_{edad}"),
       id_time = 1, value_label = "Saldo de entradas y salidas", nivel_label = glue("nacional_{edad}"),
       time = gsub("x", "", time)) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value) %>%
  arrange(time)

write_csv(info, glue("{datos}/base_saldo_edad_col_2012-2019.csv"))
