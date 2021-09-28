#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 26 sept, 2021
# Procesamiento de datos COVID
#-------------------------------------------------------#


#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
.rs.restartR()

datos_ori <- "C:/Users/DAVID/OneDrive - Universidad EAFIT/2021_BID_PulsoSocial/01_Analisis_empirico/01_Datos/Data/Casos_COVID/Input"
datos <- "C:/Users/DAVID/OneDrive - Universidad EAFIT/2021_BID_PulsoSocial/01_Analisis_empirico/01_Datos/Data/Casos_COVID/Output"
options(scipen = 999)

#---------------#
#Total mpio ----
#---------------#

covid <- read_csv(glue("{datos_ori}/Casos_positivos_de_COVID-19_en_Colombia.csv")) %>% janitor::clean_names()

covid <- covid %>%  .[,-c(17:23)] %>% .[,-c(8:15)]

covid$fecha_de_notificacion = as.Date(covid$fecha_de_notificacion, format = "%d/%m/%Y")

covid <- covid %>% mutate(año = as.character(format(fecha_de_notificacion, '%b/%Y')))
cov <- covid %>% filter(año == "mar./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov2 <- data.frame(total)
cov2 <- tibble::rownames_to_column(cov2, "value") %>% mutate(time = "Mar/2020")

cov <- covid %>% filter(año == "abr./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov3 <- data.frame(total)
cov3 <- tibble::rownames_to_column(cov3, "value") %>% mutate(time = "Abr/2020")

cov <- covid %>% filter(año == "may./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov4 <- data.frame(total)
cov4 <- tibble::rownames_to_column(cov4, "value") %>% mutate(time = "May/2020")

cov <- covid %>% filter(año == "jun./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov5 <- data.frame(total)
cov5 <- tibble::rownames_to_column(cov5, "value") %>% mutate(time = "Jun/2020")

cov <- covid %>% filter(año == "jul./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov6 <- data.frame(total)
cov6 <- tibble::rownames_to_column(cov6, "value") %>% mutate(time = "Jul/2020")

cov <- covid %>% filter(año == "ago./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov7 <- data.frame(total)
cov7 <- tibble::rownames_to_column(cov7, "value") %>% mutate(time = "Ago/2020")

cov <- covid %>% filter(año == "sept./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov, contains("muni_")))
cov8 <- data.frame(total)
cov8 <- tibble::rownames_to_column(cov8, "value") %>% mutate(time = "Sept/2020")

data_cov <- bind_rows(cov2, cov3, cov4, cov5, cov6, cov7, cov8)

data_cov <- data_cov %>% mutate(id_data = 10, variable = "casos", value_label = "Casos totales de covid - 19", id_time = "1",
                                id_nivel = "mpio", id_time = 2, nivel_value = as.numeric(gsub("muni_", "", value))) %>%
  rename(muni = value, value = total) %>% select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(data_cov, glue("{datos}/base_recuperados_mpio_Mar2020-Sept2020.csv"))

covid <- read_csv(glue("{datos_ori}/Casos_positivos_de_COVID-19_en_Colombia.csv")) %>% janitor::clean_names()

nom_dpto <- read_csv("C:/Users/DAVID/OneDrive - Universidad EAFIT/2021_BID_PulsoSocial/01_Analisis_empirico/01_Datos/Descriptives/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv")

covid <- covid %>%  .[,-c(17:23)] %>% .[,-c(8:15)]

covid$fecha_de_notificacion = as.Date(covid$fecha_de_notificacion, format = "%d/%m/%Y")

covid <- covid %>% mutate(año = as.character(format(fecha_de_notificacion, '%b/%Y')))
cov <- covid %>% filter(año == "oct./2020") 

cov <- cov %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

value = colSums( select(cov, contains("muni_")))
cov2 <- data.frame(value)
cov2 <- tibble::rownames_to_column(cov2, "mpio") %>% mutate(time = "Oct/2020")

cov <- covid %>% filter(año == "nov./2020") 
cov_ <- cov %>% .[c(1:110000),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                       valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_ <- data.frame(total)
cov3_ <- tibble::rownames_to_column(cov3_, "cod_mpio")
cov3_ <- cov3_ %>% mutate(cod_mpio = gsub("muni_", "", cov3_$cod_mpio))


cov_ <- cov %>% .[c(110001:226189),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_1 <- data.frame(total)
cov3_1 <- tibble::rownames_to_column(cov3_1, "cod_mpio")
cov3_1 <- cov3_1 %>% mutate(cod_mpio = gsub("muni_", "", cov3_1$cod_mpio))

nom_dpto$cod_mpio = as.character(nom_dpto$cod_mpio)

cov3 <- left_join(nom_dpto, cov3_, by = "cod_mpio")
cov3 <- left_join(cov3, cov3_1, by = "cod_mpio")
cov3[is.na(cov3)] <- 0

cov3 <- cov3 %>% mutate(value = total.x + total.y) %>% select(mpio, value) %>% filter(value > 0)

cov3 <- cov3 %>% mutate(time = "Nov/2020")

cov <- covid %>% filter(año == "dic./2020") 
cov_ <- cov %>% .[c(1:200000),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_ <- data.frame(total)
cov3_ <- tibble::rownames_to_column(cov3_, "cod_mpio")
cov3_ <- cov3_ %>% mutate(cod_mpio = gsub("muni_", "", cov3_$cod_mpio))


cov_ <- cov %>% .[c(200001:378229),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_1 <- data.frame(total)
cov3_1 <- tibble::rownames_to_column(cov3_1, "cod_mpio")
cov3_1 <- cov3_1 %>% mutate(cod_mpio = gsub("muni_", "", cov3_1$cod_mpio))

nom_dpto$cod_mpio = as.character(nom_dpto$cod_mpio)

cov4 <- left_join(nom_dpto, cov3_, by = "cod_mpio")
cov4 <- left_join(cov4, cov3_1, by = "cod_mpio")
cov4[is.na(cov4)] <- 0

cov4 <- cov4 %>% mutate(value = total.x + total.y) %>% select(mpio, value) %>% filter(value > 0)

cov4 <- cov4 %>% mutate(time = "Dic/2020")

cov <- covid %>% filter(año == "ene./2021") 
cov_ <- cov %>% .[c(1:100000),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_ <- data.frame(total)
cov3_ <- tibble::rownames_to_column(cov3_, "cod_mpio")
cov3_ <- cov3_ %>% mutate(cod_mpio = gsub("muni_", "", cov3_$cod_mpio))


cov_ <- cov %>% .[c(100001:200000),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_1 <- data.frame(total)
cov3_1 <- tibble::rownames_to_column(cov3_1, "cod_mpio")
cov3_1 <- cov3_1 %>% mutate(cod_mpio = gsub("muni_", "", cov3_1$cod_mpio))

cov_ <- cov %>% .[c(200001:300000),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_2 <- data.frame(total)
cov3_2 <- tibble::rownames_to_column(cov3_2, "cod_mpio")
cov3_2 <- cov3_2 %>% mutate(cod_mpio = gsub("muni_", "", cov3_2$cod_mpio))

cov_ <- cov %>% .[c(300001:411461),]
cov_ <- cov_ %>%  mutate(codigo_divipola_municipio = paste("muni", codigo_divipola_municipio, sep = "_"),
                         valor_muni = 1) %>% spread(key = codigo_divipola_municipio, value = valor_muni, fill = 0)

total = colSums( select(cov_, contains("muni_")))
cov3_3 <- data.frame(total)
cov3_3 <- tibble::rownames_to_column(cov3_3, "cod_mpio")
cov3_3 <- cov3_3 %>% mutate(cod_mpio = gsub("muni_", "", cov3_3$cod_mpio))

nom_dpto$cod_mpio = as.character(nom_dpto$cod_mpio)

cov5 <- left_join(nom_dpto, cov3_, by = "cod_mpio")
cov5 <- left_join(cov5, cov3_1, by = "cod_mpio")
cov5[is.na(cov5)] <- 0
cov5 <- cov5 %>% mutate(value = total.x + total.y) %>% select(cod_mpio, value)
cov5 <- left_join(cov5, cov3_2, by = "cod_mpio")
cov5[is.na(cov5)] <- 0
cov5 <- cov5 %>% mutate(value = total + value) %>% select(cod_mpio, value)
cov5 <- left_join(cov5, cov3_3, by = "cod_mpio")
cov5[is.na(cov5)] <- 0

cov5 <- cov5 %>% mutate(value = total + value) %>% select(cod_mpio, value) %>% filter(value > 0)

cov5 <- cov5 %>% mutate(time = "Ene/2021") %>% rename(mpio = "cod_mpio")

cov2$value = as.numeric(cov2$value)
cov2 <- cov2 %>% mutate(mpio = as.character(gsub("muni_", "", mpio)))

data_cov <- bind_rows(cov2, cov3, cov4, cov5)

data_cov <- data_cov %>% mutate(id_data = 10, variable = "casos", value_label = "Casos totales de covid - 19", id_time = "1",
                                id_nivel = "mpio", id_time = 2, nivel_value = mpio) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

# Exportar base
write_csv(data_cov, glue("{datos}/base_recuperados_mpio_Oct2020-Ene2021.csv"))


# Exportar base
write_csv(data_cov, glue("{datos}/base_casos_dpto_2020-2021.csv"))
