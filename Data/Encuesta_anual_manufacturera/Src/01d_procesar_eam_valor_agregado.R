#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 22 sept, 2021
# Procesamiento de datos EAM
#-------------------------------------------------------#


#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
#.rs.restartR()
warnings()
datos_ori <- "Data/Encuesta_anual_manufacturera/Input"
datos <- "Data/Encuesta_anual_manufacturera/Output"
options(scipen = 999)

nom_dpto <- read_xlsx("Data/Herramientas/Input/base_nombres_departamentos.xlsx")

#----------------------------------
#Valor agregado 2019
#----------------------------------

eam2019 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2019.xlsx"), sheet = "2.5")

eam_pers <- eam2019 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),]
eam_pers <- eam_pers %>% .[ ,-(2:9)] %>% .[ ,-(3:4)] %>% mutate(time = "2019") %>% janitor::clean_names()

eam_pers$departamentos[eam_pers$departamentos == "Total"] <- "Nacional"
eam_pers$departamentos[eam_pers$departamentos == "Norte Santander"] <- "Norte de Santander"
eam_pers$departamentos[eam_pers$departamentos == "Valle"] <- "Valle del Cauca"
eam_pers$departamentos[eam_pers$departamentos == "Otros Departamentos"] <- "Otros departamentos"
eam_pers$departamentos[eam_pers$departamentos == "Bogotá"] <- "Bogotá D.C."

data_eam_2019 <- eam_pers %>% filter(departamentos %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2019") %>% 
  rename(nivel_label = departamentos, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2018
#----------------------------------

eam2018 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2018.xlsx"), sheet = "2.5")

eam_2018 <- eam2018 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>%
  .[ ,-(2:9)] %>% .[ ,-(3:4)] %>% mutate(time = "2018") %>% janitor::clean_names()

eam_2018$departamentos[eam_2018$departamentos == "Total"] <- "Nacional"
eam_2018$departamentos[eam_2018$departamentos == "Norte Santander"] <- "Norte de Santander"
eam_2018$departamentos[eam_2018$departamentos == "Valle"] <- "Valle del Cauca"
eam_2018$departamentos[eam_2018$departamentos == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2018$departamentos[eam_2018$departamentos == "Bogotá"] <- "Bogotá D.C."

data_eam_2018 <- eam_2018 %>% filter(departamentos %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2018") %>% 
  rename(nivel_label = departamentos, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2017
#----------------------------------

eam2017 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2017.xlsx"), sheet = "2.5")

eam_2017 <- eam2017 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>%
  .[ ,-(11:12)] %>% .[ ,-(2:9)] %>% mutate(time = "2017") %>% janitor::clean_names()

eam_2017$departamentos[eam_2017$departamentos == "Total"] <- "Nacional"
eam_2017$departamentos[eam_2017$departamentos == "Norte Santander"] <- "Norte de Santander"
eam_2017$departamentos[eam_2017$departamentos == "Valle"] <- "Valle del Cauca"
eam_2017$departamentos[eam_2017$departamentos == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2017$departamentos[eam_2017$departamentos == "Bogotá"] <- "Bogotá D.C."

data_eam_2017 <- eam_2017 %>% filter(departamentos %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2017") %>% 
  rename(nivel_label = departamentos, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2016
#----------------------------------

eam2016 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2016.xlsx"), sheet = "2.5")

eam_2016 <- eam2016 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>%
  .[ ,-(11:12)] %>% .[ ,-(2:9)] %>% mutate(time = "2016") %>% janitor::clean_names()

eam_2016$departamentos[eam_2016$departamentos == "Total"] <- "Nacional"

eam_2016$departamentos[eam_2016$departamentos == "Norte Santander"] <- "Norte de Santander"
eam_2016$departamentos[eam_2016$departamentos == "Valle"] <- "Valle del Cauca"
eam_2016$departamentos[eam_2016$departamentos == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2016$departamentos[eam_2016$departamentos == "Bogotá"] <- "Bogotá D.C."

data_eam_2016 <- eam_2016 %>% filter(departamentos %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2016") %>% 
  rename(nivel_label = departamentos, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2015
#----------------------------------

eam2015 <- read_excel(glue("{datos_ori}/Anexos_EAM_principales_variables (1).xls"), sheet = "2.5")

eam_2015 <- eam2015 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% 
  .[ ,-(11:12)] %>% .[ ,-(2:9)] %>% mutate(time = "2015") %>% janitor::clean_names()

eam_2015$departamentos[eam_2015$departamentos == "Total"] <- "Nacional"
eam_2015$departamentos[eam_2015$departamentos == "Norte Santander"] <- "Norte de Santander"
eam_2015$departamentos[eam_2015$departamentos == "Valle"] <- "Valle del Cauca"
eam_2015$departamentos[eam_2015$departamentos == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2015$departamentos[eam_2015$departamentos == "Bogotá"] <- "Bogotá D.C."

data_eam_2015 <- eam_2015 %>% filter(departamentos %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2015") %>% 
  rename(nivel_label = departamentos, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2014
#----------------------------------

eam2014 <- read_excel(glue("{datos_ori}/Anexos_EAM_2014_def/Anexos EAM 2014/c2_5_14.xls"))

eam_2014 <- eam2014 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% 
  .[ ,-(11:12)] %>% .[ ,-(2:9)] %>% mutate(time = "2014") %>% janitor::clean_names()

eam_2014$departamentos[eam_2014$departamentos == "Total"] <- "Nacional"
eam_2014$departamentos[eam_2014$departamentos == "Norte Santander"] <- "Norte de Santander"
eam_2014$departamentos[eam_2014$departamentos == "Valle"] <- "Valle del Cauca"
eam_2014$departamentos[eam_2014$departamentos == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2014$departamentos[eam_2014$departamentos == "Bogotá"] <- "Bogotá D.C."

data_eam_2014 <- eam_2014 %>% filter(departamentos %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2014") %>% 
  rename(nivel_label = departamentos, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2013
#----------------------------------

eam2013 <- read_excel(glue("{datos_ori}/Anexos_2013/Anexos EAM 2013 definitivos/c2_5_13.xls"))

eam_2013 <- eam2013 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>%
  .[ ,-(11:12)] %>% .[ ,-(2:9)] %>% mutate(time = "2013") %>% janitor::clean_names()

eam_2013$departamento[eam_2013$departamento == "TOTAL NACIONAL"] <- "Nacional"
eam_2013$departamento[eam_2013$departamento == "Norte Santander"] <- "Norte de Santander"
eam_2013$departamento[eam_2013$departamento == "Valle"] <- "Valle del Cauca"
eam_2013$departamento[eam_2013$departamento == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2013$departamento[eam_2013$departamento == "Bogotá"] <- "Bogotá D.C."
eam_2013$departamento[eam_2013$departamento == "La guajira"] <- "La Guajira"
eam_2013$departamento[eam_2013$departamento == "San andres"] <- "San Andrés"

data_eam_2013 <- eam_2013 %>% filter(departamento %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2013") %>% 
  rename(nivel_label = departamento, value = valor_agregado) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2012
#----------------------------------

eam2012 <- read_excel(glue("{datos_ori}/Anex_2012def/ANEXOS EAM 2012/c2_5_12_con reserva.xls"))

eam_2012 <- eam2012 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(11:12)] %>% 
  .[ -c(1:3),] %>% .[ ,-(2:9)] %>% janitor::clean_names() %>% .[-(2),] %>% mutate(time = "2012")

eam_2012$departamento[eam_2012$departamento == "TOTAL NACIONAL"] <- "Nacional"
eam_2012$departamento[eam_2012$departamento == "Norte Santander"] <- "Norte de Santander"
eam_2012$departamento[eam_2012$departamento == "Valle"] <- "Valle del Cauca"
eam_2012$departamento[eam_2012$departamento == "Otros departamentos(i)"] <- "Otros departamentos"
eam_2012$departamento[eam_2012$departamento == "Bogotá, D.C."] <- "Bogotá D.C."
eam_2012$departamento[eam_2012$departamento == "La guajira"] <- "La Guajira"
eam_2012$departamento[eam_2012$departamento == "San andres"] <- "San Andrés"
eam_2012$departamento[eam_2012$departamento == "Bolivar"] <- "Bolívar"
eam_2012$departamento[eam_2012$departamento == "Cordoba"] <- "Córdoba"

data_eam_2012 <- eam_2012 %>% filter(departamento %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2012") %>% 
  rename(nivel_label = departamento, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2011
#----------------------------------

eam2011 <- read_excel(glue("{datos_ori}/Anexos_2011def/CUADROS EAM/c2_5_11 con reserva.xls"))

eam_2011 <- eam2011 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(11:12)] %>% 
  .[ -c(1:3),] %>% .[ -c(2),] %>% .[ ,-(2:9)] %>% janitor::clean_names() %>% mutate(time = "2011")


eam_2011$departamento [eam_2011$departamento == "TOTAL NACIONAL"] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("ÁÉÍÓÚ", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2011$departamento[eam_2011$departamento == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2011$departamento[eam_2011$departamento == "VALLE"] <- "VALLE DEL CAUCA"
eam_2011$departamento[eam_2011$departamento == "BOGOTA"] <- "BOGOTA D.C."
eam_2011$departamento[eam_2011$departamento == "NARI#O"] <- "NARIÑO"

data_eam_2011 <- eam_2011 %>% filter(departamento %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2011") %>% 
  rename(nivel_label = departamento, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2010
#----------------------------------

eam2010 <- read_excel(glue("{datos_ori}/Anexos_2010/c2_5_10_con reserva.xls"))

eam_2010 <- eam2010 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ -c(1:3),] %>% 
  .[ -c(2),] %>% .[ ,-(11:13)] %>% .[ ,-(2:9)] %>% janitor::clean_names() %>% mutate(time = "2010")

eam_2010$departamento [eam_2010$departamento == "TOTAL NACIONAL"] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2010$departamento[eam_2010$departamento == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2010$departamento[eam_2010$departamento == "VALLE"] <- "VALLE DEL CAUCA"
eam_2010$departamento[eam_2010$departamento == "BOGOTA"] <- "BOGOTA D.C."
eam_2010$departamento[eam_2010$departamento == "NARI#O"] <- "NARIÑO"
eam_2010$departamento[eam_2010$departamento == "OTROS DEPARTAMENTOS(i)"] <- "OTROS DEPARTAMENTOS"

data_eam_2010 <- eam_2010 %>% filter(departamento %in% nom_dpto$nivel_label) %>% 
  mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2010") %>% 
  rename(nivel_label = departamento, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2009
#----------------------------------

eam2009 <- read_excel(glue("{datos_ori}/EAM 2009 Editados 270911/c2_5_09_con reserva.xls"))

eam_2009 <- eam2009 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(11:13)] %>% 
  .[ -c(1:3),] %>% .[ -c(2),] %>% .[ ,-c(2:9)] %>% janitor::clean_names() %>% mutate(time = "2009")


nom_dpto <- read_xlsx("Data/Herramientas/Input/base_nombres_departamentos.xlsx")

eam_2009$departamento[eam_2009$departamento == "Total nacional"] <- "Nacional"
eam_2009$departamento[eam_2009$departamento == "Valle"] <- "Valle del Cauca"
eam_2009$departamento[eam_2009$departamento == "Bogotá"] <- "Bogotá D.C."
eam_2009$departamento[eam_2009$departamento == "Otros departamentos (i)"] <- "Otros departamentos"

data_eam_2009 <- eam_2009 %>% filter(departamento %in% nom_dpto$nivel_label) %>% 
  mutate(id_data = "4", variable = "valor_agregado", id_nivel = "dpto", value_label = "Valor agregado", id_time = "1", time = "2009") %>% 
  rename(nivel_label = departamento, value = valor) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------------------
#Valor agregado por departamento
#----------------------------------------------

data_eam_dpto <- bind_rows(data_eam_2009, data_eam_2010, data_eam_2011, data_eam_2012, data_eam_2013,
                           data_eam_2014, data_eam_2015, data_eam_2016, data_eam_2017, data_eam_2018,
                           data_eam_2019)

write_csv(data_eam_dpto, glue("{datos}/base_valor_agregado_dpto_2009-2019.csv"))

#--------------------------------------
#Valor agregado nacional
#--------------------------------------

eam_2009 <- eam_2009 %>% rename( departamentos = "departamento")
eam_2010 <- eam_2010 %>% rename( departamentos = "departamento")
eam_2011 <- eam_2011 %>% rename( departamentos = "departamento")
eam_2012 <- eam_2012 %>% rename( departamentos = "departamento")
eam_2013 <- eam_2013 %>% rename( departamentos = "departamento") %>% rename( valor = "valor_agregado")

eam_nac <- bind_rows(eam_2009, eam_2010, eam_2011, eam_2012, eam_2013, eam_2014, eam_2015, eam_2016,
                     eam_2017, eam_2018, eam_pers) %>% janitor::clean_names()
eam_nac <- eam_nac %>% filter(departamentos == "Nacional")

data_eam_nac <- eam_nac %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "nacional", 
                                   value_label = "Valor agregado", id_time = "1", nivel_value = "1") %>% 
  rename(nivel_label = departamentos, value = valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(data_eam_nac, glue("{datos}/base_valor_agregado_col_2009-2019.csv"))

#-------------------------------
#?reas metropolitanas y ciudades
#-------------------------------
#----------------------------------
#Valor agregado 2019
#----------------------------------

rm(list = ls())

eam2019 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2019.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_pers <- eam2019 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>%
  .[ ,-(11:12)] %>% .[ ,-(2:9)] %>% mutate(time = "2019") 

eam_pers <- eam_pers %>% mutate(area = gsub("-","", `?rea Metropolitana`)) %>% filter(Valor > 0)
eam_pers <- eam_pers %>% mutate(area = word(`?rea Metropolitana`, 1))

data_eam_2019 <- eam_pers %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `?rea Metropolitana`, value = Valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2018
#----------------------------------

eam2018 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2018.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2018 <- eam2018 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(11:12)] %>% .[ ,-(2:9)]

eam_2018 <- eam_2018 %>% mutate(time = "2018") %>% filter(Valor > 0)

eam_2018 <- eam_2018 %>% mutate(area = gsub("-"," ", `?rea Metropolitana`))
eam_2018 <- eam_2018 %>% mutate(area = word(area, 1))

data_eam_2018 <- eam_2018 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `?rea Metropolitana`, value = Valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2017
#----------------------------------

eam2017 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2017.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2017 <- eam2017 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(11:12)] %>% .[ ,-(2:9)]

eam_2017 <- eam_2017 %>% mutate(time = "2017") %>% filter(Valor > 0)

eam_2017 <- eam_2017 %>% mutate(area = gsub("-"," ", `Area Metropolitana`))
eam_2017 <- eam_2017 %>% mutate(area = word(area, 1))

data_eam_2017 <- eam_2017 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `Area Metropolitana`, value = Valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)


#----------------------------------
#Valor agregado 2016
#----------------------------------

eam2016 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2016.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2016 <- eam2016 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(11:12)] %>% .[,-(2:9)]

eam_2016 <- eam_2016 %>% mutate(time = "2016") %>% filter(Valor > 0)

eam_2016 <- eam_2016 %>% mutate(area = gsub("-"," ", `Area Metropolitana`))
eam_2016 <- eam_2016 %>% mutate(area = word(area, 1))

data_eam_2016 <- eam_2016 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `Area Metropolitana`, value = Valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2015
#----------------------------------

eam2015 <- read_excel(glue("{datos_ori}/Anexos_EAM_principales_variables (1).xls"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2015 <- eam2015 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(11:12)] %>% .[ ,-(2:9)]

eam_2015 <- eam_2015 %>% mutate(time = "2015") %>% filter(Valor > 0)

eam_2015 <- eam_2015 %>% mutate(area = gsub("-"," ", `Area Metropolitana`))
eam_2015 <- eam_2015 %>% mutate(area = word(area, 1))

data_eam_2015 <- eam_2015 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `Area Metropolitana`, value = Valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Valor agregado 2014
#----------------------------------

eam2014 <- read_excel(glue("{datos_ori}/Anexos_EAM_2014_def/Anexos EAM 2014/c2_6_14.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2014 <- eam2014 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(11:12)] %>%
  .[ ,-(2:9)] %>% janitor::clean_names()

eam_2014 <- eam_2014 %>% mutate(time = "2014") %>% filter(valor > 0)

eam_2014 <- eam_2014 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2014 <- eam_2014 %>% mutate(area = word(area, 1))

data_eam_2014 <- eam_2014 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2013
#----------------------------------

eam2013 <- read_excel(glue("{datos_ori}/Anexos_2013/Anexos EAM 2013 definitivos/c2_6_13.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "TOTAL"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2013 <- eam2013 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(11:12)] %>% .[ ,-(2:9)]
eam_2013 <- eam_2013 %>% filter( `VALOR AGREGADO` > 0) %>% janitor::clean_names()

eam_2013 <- eam_2013 %>% mutate(time = "2013")

eam_2013 <- eam_2013 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2013 <- eam_2013 %>% mutate(area = word(area, 1))

data_eam_2013 <- eam_2013 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = valor_agregado) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2012
#----------------------------------

eam2012 <- read_excel(glue("{datos_ori}/Anex_2012def/ANEXOS EAM 2012/c2_6_12_con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "TOTAL"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2012 <- eam2012 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(11:12)] %>% .[ ,-(2:9)]
eam_2012 <- eam_2012 %>% filter( VALOR > 0) %>% janitor::clean_names()

eam_2012 <- eam_2012 %>% mutate(time = "2012")

eam_2012 <- eam_2012 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2012 <- eam_2012 %>% mutate(area = word(area, 1))

data_eam_2012 <- eam_2012 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Valor agregado 2011
#----------------------------------

eam2011 <- read_excel(glue("{datos_ori}/Anexos_2011def/CUADROS EAM/c2_6_11 con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "TOTAL"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2011 <- eam2011 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(11:12)] %>% .[ ,-(2:9)]
eam_2011 <- eam_2011 %>% filter( VALOR > 0) %>% janitor::clean_names()

eam_2011 <- eam_2011 %>% mutate(time = "2011")

eam_2011 <- eam_2011 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2011 <- eam_2011 %>% mutate(area = word(area, 1))

area_m <- area_m %>% mutate (area = chartr("?????", "AEIOU", toupper(area_m$area)))

data_eam_2011 <- eam_2011 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Valor agregado 2010
#----------------------------------

eam2010 <- read_excel(glue("{datos_ori}/Anexos_2010/c2_6_10_con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2010 <- eam2010 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(11:13)]%>% .[ ,-(2:9)]
eam_2010 <- eam_2010 %>% filter( Valor > 0) %>% janitor::clean_names()

eam_2010 <- eam_2010 %>% mutate(time = "2010")

eam_2010 <- eam_2010 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2010 <- eam_2010 %>% mutate(area = word(area, 1))

data_eam_2010 <- eam_2010 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Valor agregado 2009
#----------------------------------

eam2009 <- read_excel(glue("{datos_ori}/EAM 2009 Editados 270911/c2_6_09_con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2009 <- eam2009 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(11:13)] %>% .[ ,-(2:9)]
eam_2009 <- eam_2009 %>% filter( Valor > 0) %>% janitor::clean_names()

eam_2009 <- eam_2009 %>% mutate(time = "2009")

eam_2009 <- eam_2009 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2009 <- eam_2009 %>% mutate(area = word(area, 1))

data_eam_2009 <- eam_2009 %>% mutate(id_data = "4", variable = "valor_agregado", id_nivel = "area_metropolitana", 
                                     value_label = "Valor agregado", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = valor) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#---------------------------------------------------------------
#Valor agregado por ?rea metropolitana y ciudades
#---------------------------------------------------------------

data_eam_A.M <- bind_rows(data_eam_2009, data_eam_2010, data_eam_2011, data_eam_2012, data_eam_2013,
                          data_eam_2014, data_eam_2015, data_eam_2016, data_eam_2017, data_eam_2018,
                          data_eam_2019)

write_csv(data_eam_A.M, glue("{datos}/base_valor_agregado_am_2009-2019.csv"))
