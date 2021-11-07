#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 21 sept, 2021
# Procesamiento de datos EAM
#-------------------------------------------------------#


#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
#.rs.restartR()

datos_ori <- "Data/Encuesta_anual_manufacturera/Input"
datos <- "Data/Encuesta_anual_manufacturera/Output"
options(scipen = 999)

nom_dpto <- read_xlsx("Data/Herramientas/Input/base_nombres_departamentos.xlsx")

#----------------------------------
#Establecimientos 2019
#----------------------------------

eam2019 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2019.xlsx"), sheet = "3.5")

eam_pers <- eam2019 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down")
eam_pers$`DIVISIONES INDUSTRIALES (a)` [eam_pers$`DIVISIONES INDUSTRIALES (a)` == "TOTAL"] <- "TOTAL GRUPO"
eam_pers <- eam_pers %>% filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL GRUPO") %>% .[ ,-(2)] %>% .[ ,-(4:12)]
eam_pers$`NÚMERO DE ESTABLECIMIENTOS` = as.numeric(eam_pers$`NÚMERO DE ESTABLECIMIENTOS`)
eam_pers <- eam_pers  %>%  .[ ,-(3)] %>% mutate(time = "2019")

eam_pers$DEPARTAMENTO[is.na(eam_pers$DEPARTAMENTO)] <- "Nacional"

eam_pers$DEPARTAMENTO[eam_pers$DEPARTAMENTO == "Norte Santander"] <- "Norte de Santander"
eam_pers$DEPARTAMENTO[eam_pers$DEPARTAMENTO == "Valle"] <- "Valle del Cauca"
eam_pers$DEPARTAMENTO[eam_pers$DEPARTAMENTO == "Otros Departamentos"] <- "Otros departamentos"
eam_pers$DEPARTAMENTO[eam_pers$DEPARTAMENTO == "Bogotá"] <- "Bogotá D.C."

data_eam_2019 <- eam_pers %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2019") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NÚMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2018
#----------------------------------

eam2018 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2018.xlsx"), sheet = "3.5")

eam_2018 <- eam2018 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down") %>% 
  filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL") %>% .[ ,-(2)] %>% .[ ,-(4:12)]
eam_2018$`NÚMERO DE ESTABLECIMIENTOS` = as.numeric(eam_2018$`NÚMERO DE ESTABLECIMIENTOS`)
eam_2018 <- eam_2018 %>% .[ ,-(3)] %>% mutate(time = "2018")

eam_2018$DEPARTAMENTO[is.na(eam_2018$DEPARTAMENTO)] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = toupper(nom_dpto$nivel_label))

eam_2018$DEPARTAMENTO[eam_2018$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2018$DEPARTAMENTO[eam_2018$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2018$DEPARTAMENTO[eam_2018$DEPARTAMENTO == "BOGOTÁ"] <- "BOGOTÁ D.C."

data_eam_2018 <- eam_2018 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2018") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NÚMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2017
#----------------------------------

eam2017 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2017.xlsx"), sheet = "3.5")

eam_2017 <- eam2017 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down") %>% 
  filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL") %>% .[ ,-(2)] %>% .[ ,-(3:12)] %>% mutate(time = "2017")
eam_2017$`NUMERO DE ESTABLECIMIENTOS` = as.numeric(eam_2017$`NUMERO DE ESTABLECIMIENTOS`)

eam_2017$DEPARTAMENTO[is.na(eam_2017$DEPARTAMENTO)] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = toupper(nom_dpto$nivel_label))

eam_2017$DEPARTAMENTO[eam_2017$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2017$DEPARTAMENTO[eam_2017$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2017$DEPARTAMENTO[eam_2017$DEPARTAMENTO == "BOGOTÁ"] <- "BOGOTÁ D.C."
eam_2017$DEPARTAMENTO[eam_2017$DEPARTAMENTO == "CÉSAR"] <- "CESAR"

data_eam_2017 <- eam_2017 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2017") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2016
#----------------------------------

eam2016 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2016.xlsx"), sheet = "3.5")

eam_2016 <- eam2016 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down") %>% 
  filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL") %>% .[ ,-(2)] %>% .[ ,-(3:12)] %>% mutate(time = "2016")
eam_2016$`NUMERO DE ESTABLECIMIENTOS` = as.numeric(eam_2016$`NUMERO DE ESTABLECIMIENTOS`)

eam_2016$DEPARTAMENTO[is.na(eam_2016$DEPARTAMENTO)] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("ÁÉÍÓÚ", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2016$DEPARTAMENTO[eam_2016$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2016$DEPARTAMENTO[eam_2016$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2016$DEPARTAMENTO[eam_2016$DEPARTAMENTO == "BOGOTA"] <- "BOGOTA D.C."
eam_2016$DEPARTAMENTO[eam_2016$DEPARTAMENTO == "NARI#O"] <- "NARIÑO"

data_eam_2016 <- eam_2016 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2016") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2015
#----------------------------------

eam2015 <- read_excel(glue("{datos_ori}/Anexos_EAM_principales_variables (1).xls"), sheet = "3.5")

eam_2015 <- eam2015 %>% .[ -c(1:7),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down") %>% 
  filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL") %>% .[ ,-(2)] %>% .[ ,-(3:12)] %>% mutate(time = "2015")
eam_2015$`NUMERO DE ESTABLECIMIENTOS` = as.numeric(eam_2015$`NUMERO DE ESTABLECIMIENTOS`)

eam_2015$DEPARTAMENTO[is.na(eam_2015$DEPARTAMENTO)] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2015$DEPARTAMENTO[eam_2015$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2015$DEPARTAMENTO[eam_2015$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2015$DEPARTAMENTO[eam_2015$DEPARTAMENTO == "BOGOTA"] <- "BOGOTA D.C."
eam_2015$DEPARTAMENTO[eam_2015$DEPARTAMENTO == "NARI#O"] <- "NARIÑO"

data_eam_2015 <- eam_2015 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2015") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2014
#----------------------------------

eam2014 <- read_excel(glue("{datos_ori}/Anexos_EAM_2014_def/Anexos EAM 2014/c3_5_ 14.xls"))

eam_2014 <- eam2014 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down") %>% 
  filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL") %>% .[ ,-(2)] %>% .[ ,-(3:12)] %>% mutate(time = "2014")
eam_2014$`NUMERO DE ESTABLECIMIENTOS` = as.numeric(eam_2014$`NUMERO DE ESTABLECIMIENTOS`)

eam_2014$DEPARTAMENTO[is.na(eam_2014$DEPARTAMENTO)] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2014$DEPARTAMENTO[eam_2014$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2014$DEPARTAMENTO[eam_2014$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2014$DEPARTAMENTO[eam_2014$DEPARTAMENTO == "BOGOTA"] <- "BOGOTA D.C."

data_eam_2014 <- eam_2014 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2014") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2013
#----------------------------------

eam2013 <- read_excel(glue("{datos_ori}/Anexos_2013/Anexos EAM 2013 definitivos/c3_5_ 13.xls"))

eam_2013 <- eam2013 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% fill(DEPARTAMENTO,.direction = "down") %>% 
  filter(`DIVISIONES INDUSTRIALES (a)` == "TOTAL") %>% .[ ,-(2)] %>% .[ ,-(3:12)] %>% mutate(time = "2013")
eam_2013$`NUMERO DE ESTABLECIMIENTOS` = as.numeric(eam_2013$`NUMERO DE ESTABLECIMIENTOS`)

eam_2013$DEPARTAMENTO[is.na(eam_2013$DEPARTAMENTO)] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2013$DEPARTAMENTO[eam_2013$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2013$DEPARTAMENTO[eam_2013$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2013$DEPARTAMENTO[eam_2013$DEPARTAMENTO == "BOGOTA"] <- "BOGOTA D.C."
eam_2013$DEPARTAMENTO[eam_2013$DEPARTAMENTO == "NARI#O"] <- "NARIÑO"

data_eam_2013 <- eam_2013 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2013") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE ESTABLECIMIENTOS`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2012
#----------------------------------

eam2012 <- read_excel(glue("{datos_ori}/Anex_2012def/ANEXOS EAM 2012/c3_5_12_con reserva.xls"))

eam_2012 <- eam2012 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(4:13)] %>% 
  .[ -c(1:4),] %>% .[ -c(2),]

eam_2012$DIVISIONES [is.na(eam_2012$DIVISIONES)] <- "TOTAL"

eam_2012 <- eam_2012 %>% filter(DIVISIONES == "TOTAL") %>% .[,-(2)] %>% .[-(28:67),] %>% mutate(time = "2012")

eam_2012$`NUMERO DE`= as.numeric(eam_2012$`NUMERO DE`)

eam_2012$DEPARTAMENTO [eam_2012$DEPARTAMENTO == "TOTAL NACIONAL"] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2012$DEPARTAMENTO[eam_2012$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2012$DEPARTAMENTO[eam_2012$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2012$DEPARTAMENTO[eam_2012$DEPARTAMENTO == "BOGOTA"] <- "BOGOTA D.C."
eam_2012$DEPARTAMENTO[eam_2012$DEPARTAMENTO == "NARI#O"] <- "NARIÑO"

data_eam_2012 <- eam_2012 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2012") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2011
#----------------------------------

eam2011 <- read_excel(glue("{datos_ori}/Anexos_2011def/CUADROS EAM/c3_5_11_con reserva.xls"))

eam_2011 <- eam2011 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(4:13)] %>% 
  .[ -c(1:3),] %>% .[ -c(2),]

eam_2011$DIVISIONES [is.na(eam_2011$DIVISIONES)] <- "TOTAL"

eam_2011 <- eam_2011 %>% filter(DIVISIONES == "TOTAL") %>% .[,-(2)] %>% .[-(28:67),] %>% mutate(time = "2011")

eam_2011$`NUMERO DE`= as.numeric(eam_2011$`NUMERO DE`)

eam_2011$DEPARTAMENTO [eam_2011$DEPARTAMENTO == "TOTAL NACIONAL"] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2011$DEPARTAMENTO[eam_2011$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2011$DEPARTAMENTO[eam_2011$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA"
eam_2011$DEPARTAMENTO[eam_2011$DEPARTAMENTO == "BOGOTA"] <- "BOGOTA D.C."
eam_2011$DEPARTAMENTO[eam_2011$DEPARTAMENTO == "NARI#O"] <- "NARIÑO"

data_eam_2011 <- eam_2011 %>% filter(DEPARTAMENTO %in% nom_dpto$nivel_label) %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2011") %>% 
  rename(nivel_label = DEPARTAMENTO, value = `NUMERO DE`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2010
#----------------------------------

eam2010 <- read_excel(glue("{datos_ori}/Anexos_2010/c3_5_10_con reserva.xls"))

eam_2010 <- eam2010 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(4:14)] %>% 
  .[ -c(1:3),] %>% .[ -c(2),]

eam_2010$Divisiones [is.na(eam_2010$Divisiones)] <- "TOTAL"

eam_2010 <- eam_2010 %>% filter(Divisiones == "TOTAL") %>% .[,-(2)] %>% .[-(26:67),] %>% mutate(time = "2010")

eam_2010$`Numero de`= as.numeric(eam_2010$`Numero de`)

eam_2010$Departamento [eam_2010$Departamento == "TOTAL NACIONAL"] <- "Nacional"

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("?????", "AEIOU", toupper(nom_dpto$nivel_label)))

eam_2010$Departamento[eam_2010$Departamento == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
eam_2010$Departamento[eam_2010$Departamento == "VALLE"] <- "VALLE DEL CAUCA"
eam_2010$Departamento[eam_2010$Departamento == "BOGOTA"] <- "BOGOTA D.C."
eam_2010$Departamento[eam_2010$Departamento == "NARI#O"] <- "NARIÑO"

data_eam_2010 <- eam_2010 %>% filter(Departamento %in% nom_dpto$nivel_label) %>% 
  mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2010") %>% 
  rename(nivel_label = Departamento, value = `Numero de`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)



#----------------------------------
#Establecimientos 2009
#----------------------------------

eam2009 <- read_excel(glue("{datos_ori}/EAM 2009 Editados 270911/c3_5_09_con reserva.xls"))

eam_2009 <- eam2009 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[ ,-(4:14)] %>% 
  .[ -c(1:3),] %>% .[ -c(2),]

eam_2009$Divisiones [is.na(eam_2009$Divisiones)] <- "Total"

eam_2009 <- eam_2009 %>% filter(Divisiones == "Total") %>% .[,-(2)] %>% .[-(27:67),] %>% mutate(time = "2009")

eam_2009$`Numero de`= as.numeric(eam_2009$`Numero de`)

eam_2009$Departamento [eam_2009$Departamento == "Total nacional"] <- "Nacional"

nom_dpto <- read_xlsx("Data/Herramientas/Input/base_nombres_departamentos.xlsx")

eam_2009$Departamento[eam_2009$Departamento == "Valle"] <- "Valle del Cauca"
eam_2009$Departamento[eam_2009$Departamento == "Bogotá"] <- "Bogotá D.C."

data_eam_2009 <- eam_2009 %>% filter(Departamento %in% nom_dpto$nivel_label) %>% 
  mutate(id_data = "4", variable = "establecimientos", id_nivel = "dpto", value_label = "Número de establecimientos", id_time = "1", time = "2009") %>% 
  rename(nivel_label = Departamento, value = `Numero de`) %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------------------
#Establecimientos por departamento
#----------------------------------------------

data_eam_dpto <- bind_rows(data_eam_2009, data_eam_2010, data_eam_2011, data_eam_2012, data_eam_2013,
                           data_eam_2014, data_eam_2015, data_eam_2016, data_eam_2017, data_eam_2018,
                           data_eam_2019)

write_csv(data_eam_dpto, glue("{datos}/base_establecimientos_dpto_2009-2019.csv"))

#--------------------------------------
#Establecimientos nacional
#--------------------------------------

eam_2009 <- eam_2009 %>% rename(DEPARTAMENTO = "Departamento", `NUMERO DE ESTABLECIMIENTOS` = "Numero de")
eam_2010 <- eam_2010 %>% rename(DEPARTAMENTO = "Departamento", `NUMERO DE ESTABLECIMIENTOS` = "Numero de")
eam_2011 <- eam_2011 %>% rename(`NUMERO DE ESTABLECIMIENTOS` = "NUMERO DE")
eam_2012 <- eam_2012 %>% rename(`NUMERO DE ESTABLECIMIENTOS` = "NUMERO DE")
eam_2018 <- eam_2018 %>% rename(`NUMERO DE ESTABLECIMIENTOS` = "NÚMERO DE ESTABLECIMIENTOS")
eam_pers <- eam_pers %>% rename(`NUMERO DE ESTABLECIMIENTOS` = "NÚMERO DE ESTABLECIMIENTOS")

eam_nac <- bind_rows(eam_2009, eam_2010, eam_2011, eam_2012, eam_2013, eam_2014, eam_2015, eam_2016,
                     eam_2017, eam_2018, eam_pers) %>% janitor::clean_names()
eam_nac <- eam_nac %>% filter(departamento == "Nacional")

data_eam_nac <- eam_nac %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "nacional", 
                                   value_label = "Número de establecimientos", id_time = "1", nivel_value = "1") %>% 
  rename(nivel_label = departamento, value = numero_de_establecimientos) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

write_csv(data_eam_nac, glue("{datos}/base_establecimientos_col_2009-2019.csv"))

#-------------------------------
#?reas metropolitanas y ciudades
#-------------------------------
#----------------------------------
#Establecimientos 2019
#----------------------------------

eam2019 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2019.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_pers <- eam2019 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(4:12)] %>%
  filter(Total > 0)

eam_pers$`N?mero de` = as.numeric(eam_pers$`N?mero de`)
eam_pers <- eam_pers %>% mutate(time = "2019") %>%  .[ ,-(3)]

eam_pers <- eam_pers %>% mutate(area = gsub("-","", `?rea Metropolitana`))
eam_pers <- eam_pers %>% mutate(area = word(`?rea Metropolitana`, 1))

data_eam_2019 <- eam_pers %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `?rea Metropolitana`, value = `N?mero de`) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2018
#----------------------------------

eam2018 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2018.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2018 <- eam2018 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(4:12)] %>%
  filter(Total > 0)

eam_2018$`N?mero de` = as.numeric(eam_2018$`N?mero de`)
eam_2018 <- eam_2018 %>% mutate(time = "2018") %>%
  .[ ,-(3)]

eam_2018 <- eam_2018 %>% mutate(area = gsub("-"," ", `?rea Metropolitana`))
eam_2018 <- eam_2018 %>% mutate(area = word(area, 1))

data_eam_2018 <- eam_2018 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `?rea Metropolitana`, value = `N?mero de`) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2017
#----------------------------------

eam2017 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2017.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2017 <- eam2017 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(4:12)] %>%
  filter(Total > 0)

eam_2017$`Numero de` = as.numeric(eam_2017$`Numero de`)
eam_2017 <- eam_2017 %>% mutate(time = "2017") %>%
  .[ ,-(3)]

eam_2017 <- eam_2017 %>% mutate(area = gsub("-"," ", `Area Metropolitana`))
eam_2017 <- eam_2017 %>% mutate(area = word(area, 1))

data_eam_2017 <- eam_2017 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `Area Metropolitana`, value = `Numero de`) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)


#----------------------------------
#Establecimientos 2016
#----------------------------------

eam2016 <- read_xlsx(glue("{datos_ori}/Anexos_EAM_principales_variables_2016.xlsx"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2016 <- eam2016 %>% .[ -c(1:5),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(4:12)] %>%
  filter(Total > 0)

eam_2016$`Numero de` = as.numeric(eam_2016$`Numero de`)
eam_2016 <- eam_2016 %>% mutate(time = "2016") %>%
  .[ ,-(3)]

eam_2016 <- eam_2016 %>% mutate(area = gsub("-"," ", `Area Metropolitana`))
eam_2016 <- eam_2016 %>% mutate(area = word(area, 1))

data_eam_2016 <- eam_2016 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `Area Metropolitana`, value = `Numero de`) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2015
#----------------------------------

eam2015 <- read_excel(glue("{datos_ori}/Anexos_EAM_principales_variables (1).xls"), sheet = "2.6")
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2015 <- eam2015 %>% .[ -c(1:6),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(4:12)] %>%
  filter(Total > 0)

eam_2015$`Numero de` = as.numeric(eam_2015$`Numero de`)
eam_2015 <- eam_2015 %>% mutate(time = "2015") %>%
  .[ ,-(3)]

eam_2015 <- eam_2015 %>% mutate(area = gsub("-"," ", `Area Metropolitana`))
eam_2015 <- eam_2015 %>% mutate(area = word(area, 1))

data_eam_2015 <- eam_2015 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = `Area Metropolitana`, value = `Numero de`) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Establecimientos 2014
#----------------------------------

eam2014 <- read_excel(glue("{datos_ori}/Anexos_EAM_2014_def/Anexos EAM 2014/c2_6_14.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2014 <- eam2014 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1),] %>% .[ ,-(4:12)] %>%
  filter(Total > 0) %>% janitor::clean_names()

eam_2014$numero_de = as.numeric(eam_2014$numero_de)
eam_2014 <- eam_2014 %>% mutate(time = "2014") %>%
  .[ ,-(3)]

eam_2014 <- eam_2014 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2014 <- eam_2014 %>% mutate(area = word(area, 1))

data_eam_2014 <- eam_2014 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = numero_de) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2013
#----------------------------------

eam2013 <- read_excel(glue("{datos_ori}/Anexos_2013/Anexos EAM 2013 definitivos/c2_6_13.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "TOTAL"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2013 <- eam2013 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(4:12)]
eam_2013 <- eam_2013 %>% filter( `NUMERO DE ESTABLECIMIENTOS` > 0) %>% janitor::clean_names()

eam_2013$numero_de_establecimientos = as.numeric(eam_2013$numero_de_establecimientos)
eam_2013 <- eam_2013 %>% mutate(time = "2013") %>%
  .[ ,-(3)]

eam_2013 <- eam_2013 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2013 <- eam_2013 %>% mutate(area = word(area, 1))

data_eam_2013 <- eam_2013 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = numero_de_establecimientos) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

#----------------------------------
#Establecimientos 2012
#----------------------------------

eam2012 <- read_excel(glue("{datos_ori}/Anex_2012def/ANEXOS EAM 2012/c2_6_12_con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "TOTAL"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2012 <- eam2012 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(4:12)]
eam_2012 <- eam_2012 %>% filter( `NUMERO DE` > 0) %>% janitor::clean_names()

eam_2012$numero_de = as.numeric(eam_2012$numero_de)
eam_2012 <- eam_2012 %>% mutate(time = "2012") %>%
  .[ ,-(3)]

eam_2012 <- eam_2012 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2012 <- eam_2012 %>% mutate(area = word(area, 1))

data_eam_2012 <- eam_2012 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = numero_de) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Establecimientos 2011
#----------------------------------

eam2011 <- read_excel(glue("{datos_ori}/Anexos_2011def/CUADROS EAM/c2_6_11 con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "TOTAL"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2011 <- eam2011 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(4:12)]
eam_2011 <- eam_2011 %>% filter( `NUMERO DE` > 0) %>% janitor::clean_names()

eam_2011$numero_de = as.numeric(eam_2011$numero_de)
eam_2011 <- eam_2011 %>% mutate(time = "2011") %>%
  .[ ,-(3)]

eam_2011 <- eam_2011 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2011 <- eam_2011 %>% mutate(area = word(area, 1))

area_m <- area_m %>% mutate (area = chartr("?????", "AEIOU", toupper(area_m$area)))

data_eam_2011 <- eam_2011 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = numero_de) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Establecimientos 2010
#----------------------------------

eam2010 <- read_excel(glue("{datos_ori}/Anexos_2010/c2_6_10_con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2010 <- eam2010 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(4:13)]
eam_2010 <- eam_2010 %>% filter( `Numero de` > 0) %>% janitor::clean_names()

eam_2010$numero_de = as.numeric(eam_2010$numero_de)
eam_2010 <- eam_2010 %>% mutate(time = "2010") %>%
  .[ ,-(3)]

eam_2010 <- eam_2010 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2010 <- eam_2010 %>% mutate(area = word(area, 1))

data_eam_2010 <- eam_2010 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = numero_de) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#----------------------------------
#Establecimientos 2009
#----------------------------------

eam2009 <- read_excel(glue("{datos_ori}/EAM 2009 Editados 270911/c2_6_09_con reserva.xls"))
area_m <- data_frame( "area" = c("Barranquilla", "Bogot?", "Bucaramanga", "Cali", "Cartagena", "Cauca", "C?cuta", "Manizales", "Medell?n", "Pereira", "Valle", "Resto", "Total"),
                      "nivel_value" = c("Barranquilla_A.M.", "Bogot?_D.C.", "Bucaramanga_A.M.", "Cali_A.M.", "Cartagena", "Cauca", "C?cuta_A.M.", "Manizales_A.M.", "Medell?n_A.M.", "Pereira_A.M.", "Valledupar_A.M.", "Resto_del_pa?s", "Total_ciudades_y_A.M."))

eam_2009 <- eam2009 %>% .[ -c(1:3),] %>% janitor::row_to_names(row_number = 1) %>% .[-c(1:3),] %>% .[ ,-(4:13)]
eam_2009 <- eam_2009 %>% filter( `Numero de` > 0) %>% janitor::clean_names()

eam_2009$numero_de = as.numeric(eam_2009$numero_de)
eam_2009 <- eam_2009 %>% mutate(time = "2009") %>%
  .[ ,-(3)]

eam_2009 <- eam_2009 %>% mutate(area = gsub("-"," ", area_metropolitana))
eam_2009 <- eam_2009 %>% mutate(area = word(area, 1))

data_eam_2009 <- eam_2009 %>% mutate(id_data = "4", variable = "establecimientos", id_nivel = "area_metropolitana", 
                                     value_label = "N?mero de empresas", id_time = "1") %>% left_join(area_m, by = "area") %>%
  rename(nivel_label = area_metropolitana, value = numero_de) %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)
#---------------------------------------------------------------
#Establecimientos por ?rea metropolitana y ciudades
#---------------------------------------------------------------

data_eam_A.M <- bind_rows(data_eam_2009, data_eam_2010, data_eam_2011, data_eam_2012, data_eam_2013,
                          data_eam_2014, data_eam_2015, data_eam_2016, data_eam_2017, data_eam_2018,
                          data_eam_2019)

write_csv(data_eam_A.M, glue("{datos}/base_establecimientos_am_2009-2019.csv"))
