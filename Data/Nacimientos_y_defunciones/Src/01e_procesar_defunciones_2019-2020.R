#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 11 octubre, 2021
# Procesamiento de datos de Defunciones 2019 - 2020
#-------------------------------------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
#.rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Proyecciones_poblacion/Input"
datos <- "Data/Proyecciones_poblacion/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Funciones y poblacion dpto----
#-------------------------------------------------------#

# fun_pob: Organiza informacion de proyecciones de poblacion
fun_pob <- function(data, edad = NULL, genero = NULL){
  
  # Limpiamos NA y organizamos nombres
  data <- data %>% drop_na(`...2`) %>% janitor::row_to_names(row_number = 1)
  names(data) <- tolower(names(data))
  
  if(!is.null(edad)){
    data <- data %>% dplyr::rename(cod_mpio = dpmp, year = año, area = `área geográfica`) 
  } 
  if(!is.null(genero)){
    data <- data %>% 
      dplyr::rename(cod_mpio = dpmp, hombres = `total hombres`, mujeres = `total mujeres`, year = año) %>% 
      dplyr::select(cod_mpio, year, hombres, mujeres) %>%
      pivot_longer(cols = c("hombres", "mujeres"), names_to = "genero", values_to = "poblacion")
  } 
  
  if(is.null(edad) & is.null(genero)){
    data <- data %>% dplyr::rename(cod_mpio = dpmp, poblacion = total, year = año) %>% 
      dplyr::select(cod_mpio, year, poblacion)
  }
  
  return(data)
}

# fun_edad: Organiza poblacion por edades, municipio y anio
fun_edad <- function(data, genero = NULL){
  
  # Reshape wide to long
  data <- data %>% dplyr::filter(area == "Total") %>%
    dplyr::select(cod_mpio, year, starts_with(c("hombres", "mujeres"))) %>%
    pivot_longer(cols = starts_with(c("hombres", "mujeres")), names_to = "edad", values_to = "poblacion")
  
  # Agrupamos poblacion por edades 
  if(!is.null(genero)){
    
    # Limpiamos edades y genero
    data <- data %>% mutate(genero = gsub("_.*", "", edad))
    data$edad <- gsub("hombres_", "", data$edad) 
    data$edad <- gsub("mujeres_", "", data$edad) 
    data$edad <- gsub("y más", "", data$edad)
    data$edad <- as.numeric(data$edad)
    
    data <- data %>%
      mutate(cod_mpio = as.numeric(cod_mpio), year = as.numeric(year), poblacion = as.numeric(poblacion)) 
    
  } else {
    
    # Limpiamos edades
    data$edad <- gsub("hombres_", "", data$edad) 
    data$edad <- gsub("mujeres_", "", data$edad) 
    data$edad <- gsub("y más", "", data$edad)
    data$edad <- as.numeric(data$edad)
    
    data <- data %>% 
      mutate_all(as.numeric) %>%
      group_by(cod_mpio, year, edad) %>%
      summarise(poblacion = sum(poblacion)) %>%
      ungroup()
  }
  
  return(data)
}

# Poblacion 2018-2026
pob2 <- readxl::read_xlsx(glue("{datos_ori}/anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")) 
pob2 <- fun_pob(pob2, edad = 1)
pob2 <- fun_edad(pob2)

# Codigos de departamento y municipio
cod_dpto <- read_csv("Data/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv") %>%
  select(cod_dpto, cod_mpio, nom_dpto) 
pob2 <- pob2 %>% left_join(cod_dpto, by = "cod_mpio")
pob2 <- pob2 %>% group_by(cod_dpto, year) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% left_join(cod_dpto, by = "cod_dpto")

# Poblacion 2005-2017
pob1 <- readxl::read_xlsx(glue("{datos_ori}/anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")) 
pob1 <- fun_pob(pob1, edad = 1)
pob1 <- fun_edad(pob1)

# Codigos de departamento y municipio
pob1 <- pob1 %>% left_join(cod_dpto, by = "cod_mpio")
pob1 <- pob1 %>% group_by(cod_dpto, year) %>%
  summarise(poblacion = sum(poblacion)) %>%
  ungroup() %>% left_join(cod_dpto, by = "cod_dpto")

tot <- rbind(pob1, pob2)

tot <- tot %>% select(cod_dpto, year, poblacion, nom_dpto)

tot <- distinct(tot) %>% arrange(cod_dpto)

tot$nom_dpto[tot$nom_dpto == "Bogotá, D.C."] <- "Bogotá D.C."

write_csv(tot, glue("{datos_ori}/poblacion_dpto_2005-2026.csv"))

rm(pob1, pob2, fun_edad, fun_pob, cod_dpto)

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Nacimientos_y_defunciones/Input"
datos <- "Data/Nacimientos_y_defunciones/Output"
options(scipen = 999)

#Organizar bases de datos

def19 <- read_excel(glue("{datos_ori}/2019/defunciones2019-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>% .[,c(1:6)] %>% 
  fill(na_4,.direction = "down") %>% fill(na_3,.direction = "down") %>%
  mutate(cod_enf = as.character(substr(na_5, 1, 3))) %>%
  rename(dpto = na_3, mpio = na_4) %>%
  mutate(time = 2019) %>% rename(value = na_6)

def19$cod_enf[def19$cod_enf == "000"] <- "700"

cod_en <- def19 %>% .[c(2:68),] %>% mutate(nivel_label = substring(na_5, first = 5)) %>%
  select(cod_enf, nivel_label)

def20 <- read_excel(glue("{datos_ori}/2020/def2020p-cuadro5.xlsx")) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names()%>% .[,c(1:6)] %>% 
  fill(na_4,.direction = "down") %>% fill(na_3,.direction = "down") %>%
  mutate(cod_enf = as.character(substr(na_5, 1, 3))) %>%
  rename(dpto = na_3, mpio = na_4) %>%
  mutate(time = 2020) %>% rename(value = na_6)

def20$cod_enf[def20$cod_enf == "000"] <- "700"

#------------#
# Nacional----
#------------#

nac <- def19 %>% filter(is.na(mpio)) %>% left_join(cod_en, by = "cod_enf") %>%
  mutate(id_data = 19, variable = "defunciones",
         value_label = "Número de defunciones por causas", id_nivel = "nacional_causa",
         id_time = 1, nivel_value = glue("1_{cod_enf}"), nivel_label = glue("nacional_{nivel_label}")) %>%
  filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

nac_ <- def20 %>% filter(is.na(mpio)) %>% left_join(cod_en, by = "cod_enf") %>%
  mutate(id_data = 19, variable = "defunciones",
         value_label = "Número de defunciones por causas", id_nivel = "nacional_causa",
         id_time = 1, nivel_value = glue("1_{cod_enf}"), nivel_label = glue("nacional_{nivel_label}")) %>% 
  filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

nac_col <- rbind(nac, nac_)

#-----------------#
# Departamental----
#-----------------#


dpto <- def19 %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% 
  left_join(cod_en, by = "cod_enf") %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "defunciones", value = (value/poblacion)*100000,
         value_label = "Número de defunciones por causas", id_nivel = "dpto_causa",
         id_time = 1, nivel_value = glue("{na}_{cod_enf}"), nivel_label = glue("{substring(dpto, first = 3)}_{nivel_label}")) %>%
  filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

dpto_ <- def20 %>% filter(mpio == "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  mutate(cod_dpto = as.numeric(na), year = time) %>% 
  left_join(cod_en, by = "cod_enf") %>% left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(id_data = 19, variable = "defunciones", value = (value/poblacion)*100000,
         value_label = "Número de defunciones por causas", id_nivel = "dpto_causa",
         id_time = 1, nivel_value = glue("{na}_{cod_enf}"), nivel_label = glue("{substring(dpto, first = 3)}_{nivel_label}")) %>% 
  filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

nac_dpto <- rbind(dpto, dpto_)

#--------------#
# Municipal ----
#--------------#

mpio <- def19 %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  left_join(cod_en, by = "cod_enf") %>% mutate(id_data = 19, variable = "defunciones",
         value_label = "Número de defunciones por causas", id_nivel = "mpio_causa",
         id_time = 1, nivel_value = glue("{na_2}_{cod_enf}"), nivel_label = glue("{substring(mpio, first = 6)}_{nivel_label}")) %>%
  filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

mpio_ <- def20 %>% filter(mpio != "Total Dpto" & dpto != "Extranjeros" & dpto != "Sin Información") %>% 
  left_join(cod_en, by = "cod_enf") %>% mutate(id_data = 19, variable = "defunciones",
         value_label = "Número de defunciones por causas", id_nivel = "mpio_causa",
         id_time = 1, nivel_value = glue("{na_2}_{cod_enf}"), nivel_label = glue("{substring(mpio, first = 6)}_{nivel_label}")) %>% 
  filter(na_5 != "TOTAL") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

nac_mpi <- rbind(mpio, mpio_)

#Extraer
rm(cod_en, def19, def20, dpto, dpto_, tot)
write_csv(nac_col, glue("{datos}/base_defunciones_causas_col_2019-2020.csv"))
write_csv(nac_dpto, glue("{datos}/base_defunciones_causas_dpto_2019-2020.csv"))
write_csv(nac_mpi, glue("{datos}/base_defunciones_causas_mpio_2019-2020.csv"))

#-------------------------------------------------------#
# Códigos de las enfermedades 2018 - 2020 ----
#-------------------------------------------------------#

#700 Signos, s?ntomas y afecciones mal definidas
#101 Enfermedades infecciosas intestinales
#102 Tuberculosis
#103 Ciertas enfermedades transmisibles por vectores y rabia
#104 Ciertas enfermedades inmunoprevenibles
#105 Meningitis
#106 Septicemia, excepto neonatal
#107 Enfermedad por el VIH (SIDA)
#108 Infecciones respiratorias agudas
#109 Resto de ciertas enfermedades infecciosas y parasitarias
#201 Tumor maligno del est?mago
#202 Tumor maligno del colon y de la uni?n rectosigmoidea
#203 Tumor maligno de los ?rganos digestivos y del peritoneo excepto est?mago y colon
#204 Tumor maligno de la tr?quea, los bronquios y el pulm?n
#205 Tumor maligno de los ?rganos respiratorios e intrator?cicos, excepto tr?quea, bronquios y pulm?n
#206 Tumor maligno de la mama de la mujer
#207 Tumor maligno del cuello del ?tero
#208 Tumor maligno del cuerpo del ?tero
#209 Tumor maligno del ?tero, parte no especificada
#210 Tumor maligno de la pr?stata
#211 Tumor maligno de otros ?rganos genitourinarios
#212 Leucemia
#213 Tumor maligno del tejido linf?tico, de otros ?rganos hematopoy?ticos y de tejidos afines
#214 Tumores malignos de otras localizaciones y de las no especificadas
#215 Tumores in situ, beningnos y los de comportamiento incierto o desconocido
#301 Fiebre reum?tica aguda y enfermedades reum?ticas cr?nicas
#302 Enfermedades hipertensivas
#303 Enfermedades isqu?micas del coraz?n
#304 Enfermedad cardiopulmonar, enfermedades de la circulaci?n pulmonar y otras formas de enfermedad del coraz?n
#306 Insuficiencia card?aca
#307 Enfermedades cerebrovasculares
#308 Aterosclerosis
#309 Las dem?s enfermedades del sistema circulatorio
#401 Feto y reci?n nacido afectados por ciertas afecciones maternas
#402 Feto y reci?n nacido afectados por complicaciones obst?tricas y traumatismo del nacimiento
#403 Retardo del crecimiento fetal, desnutrici?n fetal, gestaci?n corta y bajo peso al nacer
#404 Trastornos respiratorios espec?ficos del per?odo perinatal
#405 Sepsis bacteriana del reci?n nacido
#406 Resto de ciertas afecciones originadas en el per?odo perinatal
#501 Accidentes de transporte terrestre
#502 Los dem?s accidentes de transporte y los no especificados
#503 Ca?das
#504 Accidentes por disparo de arma de fuego
#505 Ahogamiento y sumersi?n accidentales
#506 Accidentes que obstruyen la respiraci?n
#507 Exposici?n a la corriente el?ctrica
#508 Exposici?n al humo, fuego y llamas
#509 Envenenamiento accidental por exposici?n a sustancias nocivas
#510 Otros accidentes, inclusive secuelas
#511 Lesiones autoinfligidas intencionalmente (suicidios)
#512 Agresiones (homicidios)
#513 Eventos de intenci?n no determinada
#514 Las dem?s causas externas
#601 Diabetes mellitus
#602 Deficiencias nutricionales y anemias nutricionales
#603 Trastornos mentales y del comportamiento
#604 Enfermedades del sistema nervioso, excepto meningitis
#605 Enfermedades cr?nicas de las v?as respiratorias inferiores
#606 Resto de enfermedades del sistema respiratorio
#607 Apendicitis, hernia de la cavidad abdominal y obstrucci?n intestinal
#608 Cirrosis y ciertas otras enfermedades cr?ncas del h?gado
#609 Resto de enfermedades del sistema digestivo
#610 Enfermedades del sistema urinario
#611 Hiperplasia de la pr?stata
#612 Embarazo, parto y puerperio
#613 Malformaciones cong?nitas, deformidades y anomal?as cromos?micas
#614 Resto de las enfermedades

