#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 25 sept, 2021
# Procesamiento de datos COVID
#-------------------------------------------------------#


#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
.rs.restartR()

datos_ori <- "Data/Casos_COVID/Input"
datos <- "Data/Casos_COVID/Output"
options(scipen = 999)

#---------------#
#Total dpto ----
#---------------#

covid <- read_csv(glue("{datos_ori}/Casos_positivos_de_COVID-19_en_Colombia.csv")) %>% janitor::clean_names()

covid$codigo_divipola_departamento[covid$codigo_divipola_departamento == "13001"] <- "13"
covid$codigo_divipola_departamento[covid$codigo_divipola_departamento == "47001"] <- "47"
covid$codigo_divipola_departamento[covid$codigo_divipola_departamento == "8001"] <- "8"

covid$fecha_de_notificacion = as.Date(covid$fecha_de_notificacion, format = "%d/%m/%Y")

covid <- covid %>% mutate(año = as.numeric(format(fecha_de_notificacion, '%Y')))
cov <- covid %>% filter(año == "2020")

cov <- cov %>%  mutate(codigo_divipola_departamento = paste("muni", codigo_divipola_departamento, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_departamento, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov2 <- data.frame(total)
cov2 <- tibble::rownames_to_column(cov2, "value") %>% mutate(time = "2020")

cov <- covid %>% filter(año == "2021")

cov <- cov %>%  mutate(codigo_divipola_departamento = paste("muni", codigo_divipola_departamento, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_departamento, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov3 <- data.frame(total)
cov3 <- tibble::rownames_to_column(cov3, "value") %>% mutate(time = "2021")

data_cov <- bind_rows(cov2, cov3)

data_cov <- data_cov %>% mutate(id_data = 10, variable = "casos", value_label = "Casos totales de covid - 19", id_time = "1",
                                id_nivel = "dpto", id_time = 1, nivel_value = as.numeric(gsub("muni_", "", value))) %>%
  rename(muni = value, value = total) %>% select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(data_cov, glue("{datos}/base_casos_dpto_2020-2021.csv"))


#---------------------------#
#Total defunciones dpto ----
#---------------------------#

cov <- covid %>% filter(recuperado == "Fallecido") %>% filter(año == "2020")

cov <- cov %>%  mutate(codigo_divipola_departamento = paste("muni", codigo_divipola_departamento, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_departamento, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov2 <- data.frame(total)
cov2 <- tibble::rownames_to_column(cov2, "value") %>% mutate(time = "2020")

cov <- covid %>% filter(recuperado == "Fallecido") %>% filter(año == "2021")

cov <- cov %>%  mutate(codigo_divipola_departamento = paste("muni", codigo_divipola_departamento, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_departamento, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov3 <- data.frame(total)
cov3 <- tibble::rownames_to_column(cov3, "value") %>% mutate(time = "2021")

data_cov <- bind_rows(cov2, cov3)

data_cov <- data_cov %>% mutate(id_data = 10, variable = "fallecidos", value_label = "Fallecidos de covid - 19", id_time = "1",
                                id_nivel = "dpto", id_time = 1, nivel_value = as.numeric(gsub("muni_", "", value))) %>%
  rename(muni = value, value = total) %>% select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(data_cov, glue("{datos}/base_fallecidos_dpto_2020-2021.csv"))

#---------------------------#
#Total recuperados dpto ----
#---------------------------#

cov <- covid %>% filter(recuperado == "Recuperado") %>% filter(año == "2020")

cov <- cov %>%  mutate(codigo_divipola_departamento = paste("muni", codigo_divipola_departamento, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_departamento, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov2 <- data.frame(total)
cov2 <- tibble::rownames_to_column(cov2, "value") %>% mutate(time = "2020")

cov <- covid %>% filter(recuperado == "Recuperado") %>% filter(año == "2021")

cov <- cov %>%  mutate(codigo_divipola_departamento = paste("muni", codigo_divipola_departamento, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_departamento, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov3 <- data.frame(total)
cov3 <- tibble::rownames_to_column(cov3, "value") %>% mutate(time = "2021")

data_cov <- bind_rows(cov2, cov3)

data_cov <- data_cov %>% mutate(id_data = 10, variable = "recuperados", value_label = "Recuperados de covid - 19", id_time = "1",
                                id_nivel = "dpto", id_time = 1, nivel_value = as.numeric(gsub("muni_", "", value))) %>%
  rename(muni = value, value = total) %>% select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(data_cov, glue("{datos}/base_recuperados_dpto_2020-2021.csv"))
