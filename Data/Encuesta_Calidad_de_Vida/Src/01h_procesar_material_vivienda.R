#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 15 octubre, 2021
# Procesamiento de datos de Material vivienda
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

# Asistencia escolar 13-17 anios (secundaria)
educ_ori <- read_csv(glue("{datos_ori}/serivicios_infraestructura_vivienda.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

names(educ_ori)

table(educ_ori$year, educ_ori$zona)
table(educ_ori$cod_dpto, educ_ori$year)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, 9:17, total) %>%
  mutate( zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Material pared
esc <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(par_ladrillo = sum(par_ladrillo), par_bahareque_norevo = sum(par_bahareque_norevo),
            par_bahareque_revo = sum(par_bahareque_revo), par_guadua = sum(par_guadua),
            par_latas_desh = sum(par_latas_desh), par_madera = sum(par_madera),
            par_no = sum(par_no), par_prefabricado = sum(par_prefabricado), par_tapia = sum(par_tapia),
            total = sum(total)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(esc$year, esc$zona)
table(esc$cod_dpto, esc$year)
colSums(is.na(esc))

# Agrupamos zonas por dpto
esc_dpt <- esc %>%
  group_by(cod_dpto, year) %>%
  summarise(par_ladrillo = sum(par_ladrillo), par_bahareque_norevo = sum(par_bahareque_norevo),
            par_bahareque_revo = sum(par_bahareque_revo), par_guadua = sum(par_guadua),
            par_latas_desh = sum(par_latas_desh), par_madera = sum(par_madera),
            par_no = sum(par_no), par_prefabricado = sum(par_prefabricado), par_tapia = sum(par_tapia),
            total = sum(total)) %>%
  ungroup() %>% mutate(par_ladrillo = (par_ladrillo/total)*100, par_bahareque_norevo = (par_bahareque_norevo/total)*100,
                       par_bahareque_revo = (par_bahareque_revo/total)*100, par_guadua = (par_guadua/total)*100,
                       par_latas_desh = (par_latas_desh/total)*100, par_madera = (par_madera/total)*100,
                       par_no = (par_no/total)*100, par_prefabricado = (par_prefabricado/total)*100, par_tapia = (par_tapia/total)*100)

table(esc_dpt$cod_dpto, esc_dpt$year)
colSums(is.na(esc_dpt))

# Organizar base datos
esc_dpto <- esc_dpt %>% pivot_longer(cols = starts_with("par_"), names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "material_par", value_label = "Material de la pared",
         id_nivel = "dpto_tipo", id_time = 1, time = year, nivel_value = glue("{cod_dpto}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

table(esc_dpto$time)

# Exportar base
write_csv(esc_dpto, glue("{datos}/base_material_par_dpto_2010-2020.csv"))

# Dividir por categoría

par <- esc_dpt %>% select(cod_dpto, year, par_ladrillo) %>%
  mutate(id_data = 2, variable = "material_par_1", value_label = "Material de la pared - ladrillo",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_ladrillo) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_1_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_bahareque_norevo) %>%
  mutate(id_data = 2, variable = "material_par_2", value_label = "Material de la pared - bahareque no revo",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_bahareque_norevo) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_2_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_bahareque_revo) %>%
  mutate(id_data = 2, variable = "material_par_3", value_label = "Material de la pared - bahareque revo",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_bahareque_revo) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_3_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_guadua) %>%
  mutate(id_data = 2, variable = "material_par_4", value_label = "Material de la pared - guadua",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_guadua) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_4_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_latas_desh) %>%
  mutate(id_data = 2, variable = "material_par_5", value_label = "Material de la pared - latas deshechables",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_latas_desh) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_5_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_madera) %>%
  mutate(id_data = 2, variable = "material_par_6", value_label = "Material de la pared - madera",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_madera) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_6_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_no) %>%
  mutate(id_data = 2, variable = "material_par_7", value_label = "Material de la pared - no hay",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_no) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_7_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_prefabricado) %>%
  mutate(id_data = 2, variable = "material_par_8", value_label = "Material de la pared - prefabricado",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_prefabricado) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_8_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, par_tapia) %>%
  mutate(id_data = 2, variable = "material_par_9", value_label = "Material de la pared - tapia",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = par_tapia) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_9_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

# Material pared por zonas y year

zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(par_ladrillo = sum(par_ladrillo), par_bahareque_norevo = sum(par_bahareque_norevo),
            par_bahareque_revo = sum(par_bahareque_revo), par_guadua = sum(par_guadua),
            par_latas_desh = sum(par_latas_desh), par_madera = sum(par_madera),
            par_no = sum(par_no), par_prefabricado = sum(par_prefabricado), par_tapia = sum(par_tapia),
            total = sum(total)) %>%
  ungroup()

table(zonas$year, zonas$zona)

# Agrupamos dptos por zona
esc_zon <- zonas %>%
  group_by(zona, year) %>%
  summarise(par_ladrillo = sum(par_ladrillo), par_bahareque_norevo = sum(par_bahareque_norevo),
            par_bahareque_revo = sum(par_bahareque_revo), par_guadua = sum(par_guadua),
            par_latas_desh = sum(par_latas_desh), par_madera = sum(par_madera),
            par_no = sum(par_no), par_prefabricado = sum(par_prefabricado), par_tapia = sum(par_tapia),
            total = sum(total)) %>%
  ungroup() %>% mutate(par_ladrillo = (par_ladrillo/total)*100, par_bahareque_norevo = (par_bahareque_norevo/total)*100,
                       par_bahareque_revo = (par_bahareque_revo/total)*100, par_guadua = (par_guadua/total)*100,
                       par_latas_desh = (par_latas_desh/total)*100, par_madera = (par_madera/total)*100,
                       par_no = (par_no/total)*100, par_prefabricado = (par_prefabricado/total)*100, par_tapia = (par_tapia/total)*100)

table(esc_zon$zona, esc_zon$year)

# Organizar base datos
esc_zona <- esc_zon %>% pivot_longer(cols = starts_with("par_"), names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "material_par", value_label = "Material de la pared",  zona = as.character(gsub(" ", "_", zona)),
         id_nivel = "zona_tipo", id_time = 1, time = year, nivel_value = glue("{zona}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

# Exportar base
write_csv(esc_zona, glue("{datos}/base_material_par_zonas_2010-2020.csv"))

# Dividir por categoría

par <- esc_zon %>% select(zona, year, par_ladrillo) %>%
  mutate(id_data = 2, variable = "material_par_1", value_label = "Material de la pared - ladrillo",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)), 
         value = par_ladrillo) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_1_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_bahareque_norevo) %>%
  mutate(id_data = 2, variable = "material_par_2", value_label = "Material de la pared - bahareque no revo",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_bahareque_norevo) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_2_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_bahareque_revo) %>%
  mutate(id_data = 2, variable = "material_par_3", value_label = "Material de la pared - bahareque revo",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_bahareque_revo) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_3_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_guadua) %>%
  mutate(id_data = 2, variable = "material_par_4", value_label = "Material de la pared - guadua",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_guadua) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_4_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_latas_desh) %>%
  mutate(id_data = 2, variable = "material_par_5", value_label = "Material de la pared - latas deshechables",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_latas_desh) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_5_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_madera) %>%
  mutate(id_data = 2, variable = "material_par_6", value_label = "Material de la pared - madera",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_madera) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_6_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_no) %>%
  mutate(id_data = 2, variable = "material_par_7", value_label = "Material de la pared - no hay",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_no) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_7_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_prefabricado) %>%
  mutate(id_data = 2, variable = "material_par_8", value_label = "Material de la pared - prefabricado",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_prefabricado) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_8_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, par_tapia) %>%
  mutate(id_data = 2, variable = "material_par_9", value_label = "Material de la pared - tapia",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = as.character(gsub(" ", "_", zona)),
         value = par_tapia) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_par_9_zona_2010-2020.csv"))

rm(educ, educ_ori, esc, esc_dpto, esc_zona, zonas, esc_zon, par)

#-------------------------------------------------------#
# 1. Organizar datos (Material piso) ----
#-------------------------------------------------------#

# Material piso
educ_ori <- read_csv(glue("{datos_ori}/serivicios_infraestructura_vivienda.csv")) %>%
  clean_names() %>%
  rename(zona = p3, cod_dpto = p1_departamento)

names(educ_ori)

table(educ_ori$year, educ_ori$zona)
table(educ_ori$cod_dpto, educ_ori$year)

# En 2010-2016 se separaban centros poblados y rural disperso, los unimos
# Zonas: 1 Cabecera; 2 Centros poblados; 3 rural dispersa
educ <- educ_ori %>%
  dplyr::select(zona, cod_dpto, year, 18:25) %>%
  mutate( zona = ifelse(zona > 1, 2, zona))

#--------------------------#
# A. Departamental ----
#--------------------------#

# Material piso por departamento, year y zona 
esc <- educ %>%
  drop_na(cod_dpto) %>%
  group_by(cod_dpto, year, zona) %>%
  summarise(piso_alfombra = sum(piso_alfombra), piso_madera = sum(piso_madera),
            piso_marmol = sum(piso_marmol), piso_baldosa = sum(piso_baldosa),
            piso_madera_burda = sum(piso_madera_burda), piso_cemento = sum(piso_cemento),
            piso_tierra = sum(piso_tierra), total = sum(total)) %>%
  ungroup() %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso"))

table(esc$year, esc$zona)
table(esc$cod_dpto, esc$year)
colSums(is.na(esc))

# Agrupamos zonas por dpto
esc_dpt <- esc %>%
  group_by(cod_dpto, year) %>%
  summarise(piso_alfombra = sum(piso_alfombra), piso_madera = sum(piso_madera),
            piso_marmol = sum(piso_marmol), piso_baldosa = sum(piso_baldosa),
            piso_madera_burda = sum(piso_madera_burda), piso_cemento = sum(piso_cemento),
            piso_tierra = sum(piso_tierra), total = sum(total)) %>%
  ungroup() %>% mutate(piso_alfombra = (piso_alfombra/total)*100, piso_madera = (piso_madera/total)*100,
                       piso_marmol = (piso_marmol/total)*100, piso_baldosa = (piso_baldosa/total)*100,
                       piso_madera_burda = (piso_madera_burda/total)*100, piso_cemento = (piso_cemento/total)*100,
                       piso_tierra = (piso_tierra/total)*100)

table(esc_dpt$cod_dpto, esc_dpt$year)
colSums(is.na(esc_dpt))

# Organizar base datos
esc_dpto <- esc_dpt %>% pivot_longer(cols = starts_with("piso_"), names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "material_piso", value_label = "Material del piso", material = as.character(gsub("_p", "", material)), 
         id_nivel = "dpto_tipo", id_time = 1, time = year, nivel_value = glue("{cod_dpto}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

table(esc_dpto$time)

# Exportar base
write_csv(esc_dpto, glue("{datos}/base_material_piso_dpto_2010-2020.csv"))

#Dividir por categoria

par <- esc_dpt %>% select(cod_dpto, year, piso_alfombra) %>%
  mutate(id_data = 2, variable = "material_piso_1", value_label = "Material del piso - alfombra",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_alfombra) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_1_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, piso_madera) %>%
  mutate(id_data = 2, variable = "material_piso_2", value_label = "Material del piso - madera",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_madera) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_2_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, piso_marmol) %>%
  mutate(id_data = 2, variable = "material_piso_3", value_label = "Material del piso - marmol",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_marmol) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_3_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, piso_baldosa) %>%
  mutate(id_data = 2, variable = "material_piso_4", value_label = "Material del piso - baldosa",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_baldosa) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_4_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, piso_madera_burda) %>%
  mutate(id_data = 2, variable = "material_piso_5", value_label = "Material del piso - madera burda",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_madera_burda) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_5_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, piso_cemento) %>%
  mutate(id_data = 2, variable = "material_piso_6", value_label = "Material del piso - cemento",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_cemento) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_6_dpto_2010-2020.csv"))

par <- esc_dpt %>% select(cod_dpto, year, piso_tierra) %>%
  mutate(id_data = 2, variable = "material_piso_7", value_label = "Material del piso - tierra",
         id_nivel = "dpto", id_time = 1, time = year, nivel_value = cod_dpto, value = piso_tierra) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_7_dpto_2010-2020.csv"))

#--------------------------#
# B. Zonas ----
# Cabecera, centros poblados-rural
#--------------------------#

#  Material piso por zonas y year

zonas <- educ %>%
  mutate(zona = as.character(zona), zona = ifelse(zona == "1", "Cabecera", "Centros poblados y rural disperso")) %>%
  group_by(year, zona) %>%
  summarise(piso_alfombra = sum(piso_alfombra), piso_madera = sum(piso_madera),
            piso_marmol = sum(piso_marmol), piso_baldosa = sum(piso_baldosa),
            piso_madera_burda = sum(piso_madera_burda), piso_cemento = sum(piso_cemento),
            piso_tierra = sum(piso_tierra), total = sum(total)) %>%
  ungroup()

table(zonas$year, zonas$zona)

# Agrupamos dptos por zona
esc_zon <- zonas %>%
  group_by(zona, year) %>%
  summarise(piso_alfombra = sum(piso_alfombra), piso_madera = sum(piso_madera),
            piso_marmol = sum(piso_marmol), piso_baldosa = sum(piso_baldosa),
            piso_madera_burda = sum(piso_madera_burda), piso_cemento = sum(piso_cemento),
            piso_tierra = sum(piso_tierra), total = sum(total)) %>%
  ungroup() %>% mutate(piso_alfombra = (piso_alfombra/total)*100, piso_madera = (piso_madera/total)*100,
                       piso_marmol = (piso_marmol/total)*100, piso_baldosa = (piso_baldosa/total)*100,
                       piso_madera_burda = (piso_madera_burda/total)*100, piso_cemento = (piso_cemento/total)*100,
                       piso_tierra = (piso_tierra/total)*100)

table(esc_zon$zona, esc_zon$year)

# Organizar base datos
esc_zona <- esc_zon %>% pivot_longer(cols = starts_with("piso_"), names_to = "material", values_to = "value") %>%
  mutate(id_data = 2, variable = "material_piso", value_label = "Material del piso",  zona = as.character(gsub(" ", "_", zona)),
         id_nivel = "zona_tipo", id_time = 1, time = year, nivel_value = glue("{zona}_{material}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

# Exportar base
write_csv(esc_zona, glue("{datos}/base_material_piso_zonas_2010-2020.csv"))

#Dividir por categoria

par <- esc_zon %>% select(zona, year, piso_alfombra) %>%
  mutate(id_data = 2, variable = "material_piso_1", value_label = "Material del piso - alfombra",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_alfombra) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_1_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, piso_madera) %>%
  mutate(id_data = 2, variable = "material_piso_2", value_label = "Material del piso - madera",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_madera) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_2_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, piso_marmol) %>%
  mutate(id_data = 2, variable = "material_piso_3", value_label = "Material del piso - marmol",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_marmol) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_3_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, piso_baldosa) %>%
  mutate(id_data = 2, variable = "material_piso_4", value_label = "Material del piso - baldosa",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_baldosa) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_4_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, piso_madera_burda) %>%
  mutate(id_data = 2, variable = "material_piso_5", value_label = "Material del piso - madera burda",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_madera_burda) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_5_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, piso_cemento) %>%
  mutate(id_data = 2, variable = "material_piso_6", value_label = "Material del piso - cemento",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_cemento) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_6_zona_2010-2020.csv"))

par <- esc_zon %>% select(zona, year, piso_tierra) %>%
  mutate(id_data = 2, variable = "material_piso_7", value_label = "Material del piso - tierra",
         id_nivel = "zona", id_time = 1, time = year, nivel_value = zona, value = piso_tierra) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value) %>%
  drop_na(nivel_value) %>%
  arrange(time)

write_csv(par, glue("{datos}/base_material_piso_7_zona_2010-2020.csv"))

