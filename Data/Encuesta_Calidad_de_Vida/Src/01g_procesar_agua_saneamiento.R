#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 15 octubre, 2021
# Procesamiento de datos de Agua y Saneamiento
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

datos_ori <- "Data/Encuesta_Calidad_de_Vida/Input/microdatos"
datos <- "Data/Encuesta_Calidad_de_Vida/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos (Material pared) ----
#-------------------------------------------------------#


educ_ori <- read_csv(glue("{datos_ori}/suelo_y_calidad_servicios.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

names(educ_ori)

table(educ_ori$year, educ_ori$zona)
table(educ_ori$cod_dpto, educ_ori$year)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, 4:16) %>%
  mutate( zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Fuente del agua
esc <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(acueducto_publico = sum(acueducto_publico), acueducto_comunal = sum(acueducto_comunal),
            pozo_bomba = sum(pozo_bomba), pozo_nobomba = sum(pozo_nobomba),
            agua_lluvia = sum(agua_lluvia), rio_quebrada = sum(rio_quebrada), agua_botella_bolsa = sum(agua_botella_bolsa),
            pila_publica = sum(pila_publica), carrotanque = sum(carrotanque), aguatero = sum(aguatero),
            total = sum(total)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(esc$year, esc$zona)
table(esc$cod_dpto, esc$year)
colSums(is.na(esc))

# Agrupamos zonas por dpto
esc_dpt <- esc %>%
  group_by(cod_dpto, year) %>%
  summarise(acueducto_publico = sum(acueducto_publico), acueducto_comunal = sum(acueducto_comunal),
            pozo_bomba = sum(pozo_bomba), pozo_nobomba = sum(pozo_nobomba),
            agua_lluvia = sum(agua_lluvia), rio_quebrada = sum(rio_quebrada), agua_botella_bolsa = sum(agua_botella_bolsa),
            pila_publica = sum(pila_publica), carrotanque = sum(carrotanque), aguatero = sum(aguatero),
            total = sum(total)) %>%
  ungroup() %>% mutate(acueducto_publico = (acueducto_publico/total)*100, acueducto_comunal = (acueducto_comunal/total)*100,
                       pozo_bomba = (pozo_bomba/total)*100, pozo_nobomba = (pozo_nobomba/total)*100,
                       agua_lluvia = (agua_lluvia/total)*100, rio_quebrada = (rio_quebrada/total)*100, agua_botella_bolsa = (agua_botella_bolsa/total)*100,
                       pila_publica = (pila_publica/total)*100, carrotanque = (carrotanque/total)*100, aguatero = (aguatero/total)*100)

table(esc_dpt$cod_dpto, esc_dpt$year)
colSums(is.na(esc_dpt))

# Organizar base datos
esc_dpto <- esc_dpt %>% pivot_longer(cols = 3:12, names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "fuente_agua", value_label = "Fuente del agua",
         id_nivel = "dpto_fuente", id_time = 1, time = year, nivel_value = glue("{cod_dpto}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

table(esc_dpto$time)

# Exportar base
write_csv(esc_dpto, glue("{datos}/base_fuente_agua_dpto_2010-2020.csv"))

# Dividir por categoría

par <- esc_dpt %>% select(cod_dpto, year, acueducto_publico) %>%
  mutate(id_data = 2, variable = "fuente_agua_1", value_label = "Fuente de agua - acueducto público",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = acueducto_publico) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_1_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, acueducto_comunal) %>%
  mutate(id_data = 2, variable = "fuente_agua_2", value_label = "Fuente de agua - acueducto comunal",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = acueducto_comunal) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_2_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, pozo_bomba) %>%
  mutate(id_data = 2, variable = "fuente_agua_3", value_label = "Fuente de agua - pozo bomba",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = pozo_bomba) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_3_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, pozo_nobomba) %>%
  mutate(id_data = 2, variable = "fuente_agua_4", value_label = "Fuente de agua - pozo no bomba",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = pozo_nobomba) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_4_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, agua_lluvia) %>%
  mutate(id_data = 2, variable = "fuente_agua_5", value_label = "Fuente de agua - agua lluvia",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = agua_lluvia) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_5_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, rio_quebrada) %>%
  mutate(id_data = 2, variable = "fuente_agua_6", value_label = "Fuente de agua - rio quebrada",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = rio_quebrada) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_6_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, agua_botella_bolsa) %>%
  mutate(id_data = 2, variable = "fuente_agua_7", value_label = "Fuente de agua - agua botella o bolsa",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = agua_botella_bolsa) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_7_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, pila_publica) %>%
  mutate(id_data = 2, variable = "fuente_agua_8", value_label = "Fuente de agua - pila pública",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = pila_publica) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_8_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, carrotanque) %>%
  mutate(id_data = 2, variable = "fuente_agua_9", value_label = "Fuente de agua - carrotanque",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = carrotanque) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_9_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, aguatero) %>%
  mutate(id_data = 2, variable = "fuente_agua_10", value_label = "Fuente de agua - aguatero",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = aguatero) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_10_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Fuente del agua por zonas y year

zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(acueducto_publico = sum(acueducto_publico), acueducto_comunal = sum(acueducto_comunal),
            pozo_bomba = sum(pozo_bomba), pozo_nobomba = sum(pozo_nobomba),
            agua_lluvia = sum(agua_lluvia), rio_quebrada = sum(rio_quebrada), agua_botella_bolsa = sum(agua_botella_bolsa),
            pila_publica = sum(pila_publica), carrotanque = sum(carrotanque), aguatero = sum(aguatero),
            total = sum(total)) %>%
  ungroup()

table(zonas$year, zonas$zona)

# Agrupamos dptos por zona
esc_zon <- zonas %>%
  group_by(zona, year) %>%
  summarise(acueducto_publico = sum(acueducto_publico), acueducto_comunal = sum(acueducto_comunal),
            pozo_bomba = sum(pozo_bomba), pozo_nobomba = sum(pozo_nobomba),
            agua_lluvia = sum(agua_lluvia), rio_quebrada = sum(rio_quebrada), agua_botella_bolsa = sum(agua_botella_bolsa),
            pila_publica = sum(pila_publica), carrotanque = sum(carrotanque), aguatero = sum(aguatero),
            total = sum(total)) %>%
  ungroup() %>% mutate(acueducto_publico = (acueducto_publico/total)*100, acueducto_comunal = (acueducto_comunal/total)*100,
                       pozo_bomba = (pozo_bomba/total)*100, pozo_nobomba = (pozo_nobomba/total)*100,
                       agua_lluvia = (agua_lluvia/total)*100, rio_quebrada = (rio_quebrada/total)*100, agua_botella_bolsa = (agua_botella_bolsa/total)*100,
                       pila_publica = (pila_publica/total)*100, carrotanque = (carrotanque/total)*100, aguatero = (aguatero/total)*100)

table(esc_zon$zona, esc_zon$year)

# Organizar base datos
esc_zona <- esc_zon %>% pivot_longer(cols = 3:12, names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "fuente_agua", value_label = "Fuente del agua", zona = as.character(gsub(" ", "_", zona)),
         id_nivel = "zona_fuente", id_time = 1, time = year, nivel_value = glue("{zona}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

# Exportar base
write_csv(esc_zona, glue("{datos}/base_fuente_agua_zonas_2010-2020.csv"))

# Dividir por categoría

par <- esc_zon %>% select(zona, year, acueducto_publico) %>%
  mutate(id_data = 2, variable = "fuente_agua_1", value_label = "Fuente de agua - acueducto público",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = acueducto_publico) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_1_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, acueducto_comunal) %>%
  mutate(id_data = 2, variable = "fuente_agua_2", value_label = "Fuente de agua - acueducto comunal",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = acueducto_comunal) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_2_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, pozo_bomba) %>%
  mutate(id_data = 2, variable = "fuente_agua_3", value_label = "Fuente de agua - pozo bomba",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = pozo_bomba) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_3_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, pozo_nobomba) %>%
  mutate(id_data = 2, variable = "fuente_agua_4", value_label = "Fuente de agua - pozo no bomba",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = pozo_nobomba) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_4_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, agua_lluvia) %>%
  mutate(id_data = 2, variable = "fuente_agua_5", value_label = "Fuente de agua - agua lluvia",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = agua_lluvia) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_5_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, rio_quebrada) %>%
  mutate(id_data = 2, variable = "fuente_agua_6", value_label = "Fuente de agua - rio quebrada",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = rio_quebrada) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_6_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, agua_botella_bolsa) %>%
  mutate(id_data = 2, variable = "fuente_agua_7", value_label = "Fuente de agua - agua botella o bolsa",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = agua_botella_bolsa) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_7_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, pila_publica) %>%
  mutate(id_data = 2, variable = "fuente_agua_8", value_label = "Fuente de agua - pila pública",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = pila_publica) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_8_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, carrotanque) %>%
  mutate(id_data = 2, variable = "fuente_agua_9", value_label = "Fuente de agua - carrotanque",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = carrotanque) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_9_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, aguatero) %>%
  mutate(id_data = 2, variable = "fuente_agua_10", value_label = "Fuente de agua - aguatero",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = aguatero) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_fuente_agua_10_zona_2010-2020.csv"))

rm( esc, esc_dpto, esc_zona, zonas, par, esc_zon, esc_dpt)

#--------------------------#
# A. Departamental ----
#--------------------------#

# Saenamiento

esc <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(sanitario_adentro = sum(sanitario_adentro), sanitario_fuera = sum(sanitario_fuera),
            total = sum(total)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(esc$year, esc$zona)
table(esc$cod_dpto, esc$year)
colSums(is.na(esc))

# Agrupamos zonas por dpto
esc_dpt <- esc %>%
  group_by(cod_dpto, year) %>%
  summarise(sanitario_adentro = sum(sanitario_adentro), sanitario_fuera = sum(sanitario_fuera),
            total = sum(total)) %>%
  ungroup() %>% mutate(sanitario_adentro = (sanitario_adentro/total)*100, sanitario_fuera = (sanitario_fuera/total)*100)

table(esc_dpt$cod_dpto, esc_dpt$year)
colSums(is.na(esc_dpt))

# Organizar base datos
esc_dpto <- esc_dpt %>% pivot_longer(cols = 3:4, names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "saneamiento", value_label = "Saneamiento de la vivienda",
         id_nivel = "dpto_tipo", id_time = 1, time = year, nivel_value = glue("{cod_dpto}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

table(esc_dpto$time)

# Exportar base
write_csv(esc_dpto, glue("{datos}/base_saneamiento_dpto_2010-2020.csv"))

# Dividir caracteristicas

par <- esc_dpt %>% select(cod_dpto, year, sanitario_adentro) %>%
  mutate(id_data = 2, variable = "saneamiento_adentro", value_label = "Saneamiento dentro de la vivienda",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = sanitario_adentro) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_saneamiento_adentro_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, sanitario_fuera) %>%
  mutate(id_data = 2, variable = "saneamiento_fuera", value_label = "Saneamiento fuera de la vivienda",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = sanitario_fuera) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_saneamiento_fuera_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Saenamiento por zonas y year

zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(sanitario_adentro = sum(sanitario_adentro), sanitario_fuera = sum(sanitario_fuera),
            total = sum(total)) %>%
  ungroup() 

table(zonas$year, zonas$zona)

# Agrupamos dptos por zona
esc_zon <- zonas %>%
  group_by(zona, year) %>%
  summarise(sanitario_adentro = sum(sanitario_adentro), sanitario_fuera = sum(sanitario_fuera),
            total = sum(total)) %>%
  ungroup() %>% mutate(sanitario_adentro = (sanitario_adentro/total)*100, sanitario_fuera = (sanitario_fuera/total)*100)

table(esc_zon$zona, esc_zon$year)

# Organizar base datos
esc_zona <- esc_zon %>% pivot_longer(cols = 3:4, names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "saneamiento", value_label = "Saneamiento de la vivienda", zona = as.character(gsub(" ", "_", zona)),
         id_nivel = "zona_tipo", id_time = 1, time = year, nivel_value = glue("{zona}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

# Exportar base
write_csv(esc_zona, glue("{datos}/base_saneamiento_zonas_2010-2020.csv"))

# Dividir caracteristicas

par <- esc_zon %>% select(zona, year, sanitario_adentro) %>%
  mutate(id_data = 2, variable = "saneamiento_adentro", value_label = "Saneamiento dentro de la vivienda",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = sanitario_adentro) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_saneamiento_adentro_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, sanitario_fuera) %>%
  mutate(id_data = 2, variable = "saneamiento_fuera", value_label = "Saneamiento fuera de la vivienda",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = sanitario_fuera) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_saneamiento_fuera_zona_2010-2020.csv"))

rm(educ, educ_ori, esc, esc_dpto, esc_zona, zonas)
