#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 11 octubre, 2021
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

#-------------------------------------------------------#
# 1. Nacional ----
#-------------------------------------------------------#

years <- c(2010:2018)

# Leer y organizar defunciones a nivel nacional
def_col <- lapply(years, function(x){
  
  if(x < 2014){
    lista <- list.files(path = glue("{datos_ori}/{x}/Mortalidad_no_fetal"), pattern = "cuadro5_.*")
  } 
  if(x >= 2014 & x < 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}/Mortalidad_no_fetal"), pattern = "CUADRO5-.*")
  }
  
  print(x)
  print(lista)
  
  data <- read_excel(glue("{datos_ori}/{x}/Mortalidad_no_fetal/{lista}"), sheet = "00") %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    select(na, na_2) %>% rename(value = na_2) %>%
    mutate(cod_enf = as.character(substr(na, 1, 3)))%>%
    mutate(time = x, variable = "defunciones",
           value_label = "Número de defunciones por causas", id_nivel = "nacional_causa",
           id_time = 1, nivel_value = glue("1_{cod_enf}"), id_data = 19) %>% 
    filter(na != "TOTAL NACIONAL")
  
  
  return(data)
  
}) %>% bind_rows()

cod_en <- def_col %>% .[c(1:71),] %>% mutate(nivel_label = substring(na, first = 5)) %>%
  select(cod_enf, nivel_label)

cod_en_ <- def_col %>% .[c(565:631),] %>% mutate(nivel_label = substring(na, first = 5)) %>%
  select(cod_enf, nivel_label)

def_col_18 <- def_col %>% filter(time == "2018") %>% left_join(cod_en_, by = "cod_enf") %>%
  mutate(nivel_label = glue("nacional_{nivel_label}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

def_col <- def_col %>% filter(time != "2018")

def_col <- def_col %>% left_join(cod_en, by = "cod_enf") %>%
  mutate(nivel_label = glue("nacional_{nivel_label}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

write_csv(def_col, glue("{datos}/base_defunciones_causas_col_2010-2017.csv"))

# Pegamos anio 2019 y 2020 a 2018
def_col19 <- read_csv(glue("{datos}/base_defunciones_causas_col_2019-2020.csv"))
nac_col <- rbind (def_col_18, def_col19)

write_csv(nac_col, glue("{datos}/base_defunciones_causas_col_2018-2020.csv"))

rm(def_col, def_col_18, def_col19, nac_col)


#-------------------------------------------------------#
# 2. Departamental ----
#-------------------------------------------------------#

# Codigos de departamento y municipio
cod_dpto <- read_csv("Data/Herramientas/Input/base_nombres_codigos_dpto_mpio.csv") %>%
  select(cod_dpto, cod_mpio) %>% rename(nivel_value = cod_mpio)

# Lista de codigos departamentos en el estilo de datos DANE
lista_dpto <- cod_dpto %>% 
  select(cod_dpto) %>% 
  distinct(cod_dpto) %>%
  mutate(nivel_value = ifelse(nchar(cod_dpto) == 1, glue("0{cod_dpto}"), cod_dpto)) %>%
  .[["nivel_value"]]

# Leer cada anio de defunciones y los departamentos
def_dpto <- lapply(years, function(y){
  
  # Abrir cada anio
  print(y)
  if(y < 2014){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "cuadro5_.*")
  } 
  if(y >= 2014 & y < 2019){
    lista <- list.files(path = glue("{datos_ori}/{y}/Mortalidad_no_fetal"), pattern = "CUADRO5-.*")
  }
  
  # Abrir cada departamento, en el anio elegido
  data_dpto <- lapply(lista_dpto, function(x){
    
    data <- read_excel(glue("{datos_ori}/{y}/Mortalidad_no_fetal/{lista}"), sheet = x) %>%
      drop_na(`...4`) %>% janitor::row_to_names(row_number = 2) %>% janitor::clean_names() %>%
      select(na, na_2, na_3) %>% rename(value = na_3) %>%
      rename(dpto = na) %>% fill(dpto,.direction = "down") %>%
      mutate(time = y, variable = "defunciones",
             value_label = "Número de defunciones por causas", id_time = 1, id_data = 19, id_d = x)  
    
    return(data)
    
  }) %>% bind_rows()
  
  return(data_dpto)
  
}) %>% bind_rows()

tot <- read_csv(glue("Data/Proyecciones_poblacion/Input/poblacion_dpto_2005-2026.csv"))

dpto <- def_dpto %>% filter(dpto == "TOTAL") %>% filter(na_2 != "TOTAL") %>% 
  mutate(cod_dpto = as.numeric(id_d), year = time) %>% 
  left_join(tot, by = c("cod_dpto", "year")) %>%
  mutate(cod_enf = as.character(substr(na_2, 1, 3)), nivel_value = glue("{id_d}_{cod_enf}"),
         id_nivel = "dpto_causa", value = (as.numeric(value)/poblacion)*100000) %>%
  left_join(cod_en, by = "cod_enf") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value, id_d, cod_enf)

dpto18 <- dpto %>% filter(time == "2018") %>% left_join(cod_en_, by = "cod_enf")%>%
  mutate(nivel_label = glue("{id_d}_{nivel_label}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

dpto <- dpto %>% filter(time != "2018") %>% left_join(cod_en, by = "cod_enf") %>%
  mutate(nivel_label = glue("{id_d}_{nivel_label}")) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label, value)

write_csv(dpto, glue("{datos}/base_defunciones_causas_dpto_2010-2017.csv"))

# Pegamos anio 2019 y 2020 a 2018
def_dpto19 <- read_csv(glue("{datos}/base_defunciones_causas_dpto_2019-2020.csv"))
nac_dpto <- rbind (dpto18, def_dpto19)

write_csv(nac_dpto, glue("{datos}/base_defunciones_causas_dpto_2018-2020.csv"))

rm(cod_dpto, def_dpto19, dpto,dpto18, nac_dpto)

#-------------------------------------------------------#
# 3. Municipal ----
#-------------------------------------------------------#

mnpio <- def_dpto %>% filter(dpto != "TOTAL") %>% filter(na_2 != "TOTAL") %>%
  mutate(nivel_value = glue("{cod_muni = as.character(substr(dpto, 1, 5))}_
                            {cod_enf = as.character(substr(na_2, 1, 3))}"),
         id_nivel = "mpio_causa") %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

mpio18 <- mnpio %>% filter(time == "2018")
mnpio <- mnpio %>% filter(time != "2018")

write_csv(mnpio, glue("{datos}/base_defunciones_causas_mpio_2010-2017.csv"))
rm(cod_dpto, def_dpto, mpio18)
# Pegamos anio 2019 y 2020 a 2018
mpio19 <- read_csv(glue("{datos}/base_defunciones_causas_mpio_2019-2020.csv"))
nac_mpio <- rbind(mpio18, mpio19)

write_csv(nac_mpio, glue("{datos}/base_defunciones_causas_mpio_2018-2020.csv"))


cod_en <- def_dpto %>% .[c(2:72),] %>% mutate(nivel_value = as.character(substr(na_2, 1, 3)),
                                              nivel_label = substring(na_2, first = 4)) %>%
  select(nivel_value, nivel_label, na_2)

#-------------------------------------------------------#
# 4. Códigos de las enfermedades 2010 - 2017 ----
#-------------------------------------------------------#

#101 ENFERMEDADES INFECCIOSAS INTESTINALES
#102 TUBERCULOSIS, INCLUSIVE SECUELAS
#103 CIERTAS ENF. TRANSMITIDAS POR VECTORES Y RABIA -
#104 CIERTAS ENF. PREVENIBLES POR VACUNA
#105 MENINGITIS -
#106 SEPTICEMIA 
#107 INFECC. CON MODO DE TRANSM. PREDOM./. SEXUAL - 
#108 ENFERMEDAD POR EL VIH/SIDA -
#109 INFECCIONES RESPIRATORIAS AGUDAS -
#110 OTRASENF. INFECCIOSAS Y PARASITARIAS 
#201 TUMOR MALIGNO DEL ESTOMAGO
#202 TUMOR MALIGNO DEL COLON
#203 TUMOR M. ORG.DIGESTIVOS Y PERITONEO, EXCL.ESTOM.Y COLON
#204 TUMOR MALIGNO HIGADO Y VIAS BILIARES
#205 TUMOR MALIGNO DEL PANCREAS
#206 T. M. DE TRAQUEA, BRONQUIOS Y PULMON
#207 TUMOR M.ORG. RESPIRAT. E INTRAT., EXCL.TRAQUEA, BRONQUIOS Y PULMON
#208 TUMOR MALIGNO DE LA MAMA
#209 T. MALIGNO DEL UTERO
#210 T. MALIGNO DE LA PROSTATA
#211 T. MALIGNO OTROS ORGANOS GENITOURINARIOS
#212 T. MALIGNO TEJIDO LINFATICO, ORG. HEMATOPOY. Y TEJIDOS AFINES
#213 RESIDUO DE TUMORES MALIGNOS
#214 CARCINOMA IN-SITU, T. BENIGNOS Y DE COMPORTAM.INCIERTO O DESCON.
#301 FIEBRE REUMATICA AGUDA Y ENF. CARDIACAS REUM. CRONICAS
#302 ENFERMEDADES HIPERTENSIVAS
#303 ENFERMEDADES ISQUEMICAS DEL CORAZON
#304 ENF. CARDIOPULMONAR, DE LA CIRC. PULM. Y OTRAS ENF. CORAZON
#306 INSUFICIENCIA CARDIACA
#307 ENFERMEDADES CEREBROVASCULARES
#308 ATEROSCLEROSIS
#309 RESTO ENF. DEL SISTEMA CIRCULATORIO
#401 FETO Y RECIEN NACIDO AFECTADOS POR CIERTAS AFECC. MATERNAS
#402 FETO Y RECIEN N. AFECTADOS POR COMPL. OBST. Y TRAUM. NACIMIENTO
#403 RETARDO CRECIM.FETAL, DESNUTR. FETAL., BAJO P./ NACER, GEST.CORTA
#404 TRAST. RESPIRATORIOS ESPECIFICOS DEL PERIODO PERINATAL
#405 SEPSIS BACTERIANA DEL RECIEN NACIDO
#406 ENF. HEMOLITICA DEL FETO Y DEL RECIEN N. Y KERNICTERUS
#407 OTRAS AFECC. ORIGINADAS EN PERIODO PERINATAL
#501 ACC. TRANSPORTE TERRESTRE, INCLUSIVE SECUELAS
#502 OTROS ACC. TRANSPORTE Y LOS NO ESP., INCLUSIVE SECUELAS
#503 CAIDAS
#504 ACC. CAUSADOS POR MAQUINAS Y POR INSTR. CORTANTES/PUNZANTES
#505 ACC. CAUSADOS POR DISPARO DE ARMAS DE FUEGO
#506 AHOGAMIENTO Y SUMERSION ACCIDENTALES
#507 EXPOSICION AL HUMO, FUEGO Y LLAMAS
#508 ENVENENAMIENTO. ACC. POR, Y EXPOSICION A SUSTANC.NOCIVAS
#509 COMPLICAC.DE LA ATENC. MEDICA Y QUIRURG., INCLUSIVE SECUELAS
#510 OTROS ACCIDENTES, INCLUSIVE SECUELAS
#511 LESIONES AUTOINFLIGIDAS INTENC.(SUICIDIOS), INCL. SECUELAS
#512 AGRESIONES (HOMICIDIOS), INCLUSIVE SECUELAS
#513 INTERVENCION LEGAL Y OPERAC. DE GUERRA, INCL. SECUELAS
#514 EVENTOS DE INTENCION NO DETERMINADA, INCL. SECUELAS
#601 DIABETES MELLITUS
#602 DEFICIENCIAS NUTRICIONALES Y ANEMIAS NUTRICIONALES
#603 TRASTORNOS MENTALES Y DEL COMPORTAMIENTO
#604 ENF. SISTEMA NERVIOSO, EXCEPTO MENINGITIS
#605 ENF. CRONICAS VIAS REPIRATORIAS INFERIORES
#606 OTRAS ENF. DE LAS VIAS RESPIRATORIAS SUPERIORES
#607 ENF. DEL PULMON DEBIDAS A AGENTES EXTERNOS
#608 OTRAS ENFERMEDADES RESPIRATORIAS
#609 APENDICITIS, HERNIA CAVIDAD ABDOMINAL Y OBSTR. INTESTINAL
#610 CIERTAS ENF. CRONICAS DEL HIGADO Y CIRROSIS
#611 OTRAS ENF. SISTEMA DIGESTIVO
#612 ENFERMEDADES SISTEMA URINARIO
#613 HIPERPLASIA DE LA PROSTATA
#614 EMBARAZO, PARTO Y PUERPERIO
#615 MALFORMACIONES CONGEN., DEFORMID.Y ANOMALIAS CROMOSOMICAS
#616 RESIDUO
#700 SINTOMAS, SIGNOS Y AFECCIONES MAL DEFINIDAS

#-------------------------------------------------------#
# 5. C?digos de las enfermedades 2018 - 2020 ----
#-------------------------------------------------------#

#700 Signos, s?ntomas y afecciones mal definidas
#101 Enfermedades infecciosas intestinales
#102 Tuberculosis
#103 Ciertas enfermedades transmisibles por vectores y rabia -
#104 Ciertas enfermedades inmunoprevenibles
#105 Meningitis -
#106 Septicemia, excepto neonatal
#107 Enfermedad por el VIH (SIDA) -
#108 Infecciones respiratorias agudas
#109 Resto de ciertas enfermedades infecciosas y parasitarias -
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
