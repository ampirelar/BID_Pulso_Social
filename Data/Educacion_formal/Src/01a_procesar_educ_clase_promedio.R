#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 28 sept, 2021
# Procesamiento de datos de Educación formal 2010 - 2020
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

path <- getwd()
path <- gsub("01_Datos", "02_Descriptivas", path)
options(scipen = 999)

# Etiquetas nombres departamentos y zonas

nom_dpto <- read_xlsx(glue("{path}/Herramientas/Input/base_nombres_departamentos.xlsx"))

datos_ori <- "Data/Educacion_formal/Input"
datos <- "Data/Educacion_formal/Output"
options(scipen = 999)

#Organizar bases de datos

matr20 <- read_excel(glue("{datos_ori}/2020/anexo-EDUC-2020-1-1-matricula-niveles-edu-por-sector-grados.xlsx"), sheet = "Básica primaria") %>% 
  drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>% 
  select(na, na_2, total) %>% filter(na_2 == "Total") %>% rename(dpto = na) %>%
  mutate(time = 2020)

doc20 <- read_excel(glue("{datos_ori}/2020/anexo-EDUC-2020-2-1-docentes-nivel-edu-sector-zona.xlsx"), sheet = "Docentes_sector") %>% 
  drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>% 
  select(na, na_2, total_3) %>% filter(na_2 == "Total") %>% rename(dpto = na) %>%
  mutate(time = 2020) %>% left_join(matr20, by = c("dpto", "time", "na_2")) %>%
  rename(docentes = "total_3", estudiantes = "total") %>% 
  mutate(docentes = as.numeric(docentes), estudiantes = as.numeric(estudiantes), value = estudiantes/docentes)
  
doc20$dpto[doc20$dpto == "Bogotá, D.C."] <- "Bogotá D.C."
doc20$dpto[doc20$dpto == "Archipiélago de San Andrés, Providencia y Santa Catalina"] <- "San Andrés"

doc20_dpto <- doc20 %>% filter(dpto %in% nom_dpto$nivel_label) %>%
  mutate(id_data = "7", variable = "tamano_clase_prim", id_nivel = "dpto", 
         value_label = "Tamaño de clase en primaria", id_time = "1") %>% 
  rename(nivel_label = "dpto") %>% left_join(nom_dpto, by = "nivel_label") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

years <- c(2014:2019)

# Leer y organizar matriculados y docentes a nivel nacional 2010 - 2019
matr_col <- lapply(years, function(x){
  
  if(x < 2015){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "EFormal_alumnos_grados.*")
  } 
  if(x > 2014 & x <= 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "1-1.*")
  }
  
  lista_nom <- c("Primaria", "primaria")
  sheets <- readxl::excel_sheets(glue("{datos_ori}/{x}/General/{lista}"))
  patron <- sheets[grepl(paste(lista_nom, collapse = "|"), sheets, ignore.case = FALSE)]
  data <- read_excel(glue("{datos_ori}/{x}/General/{lista}"), sheet = patron) %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    select(na, total) %>% filter(na != "") %>% mutate(total = as.numeric(total), time = x)
    
  return(data)
  
}) %>% bind_rows()

matr_col$na[matr_col$na == "Nacional"] <- "TOTAL NACIONAL"
matr_col$na[matr_col$na == "Total Nacional"] <- "TOTAL NACIONAL"

years <- c(2014)

doc1_col <- lapply(years, function(x){
  
  if(x < 2011){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "EFormal_docentes_sector_.*")
  } 
  if(x > 2010 & x <= 2014){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "EFormal_Docentes_sector.*")
  }
  
  lista_nom <- c("Sector","SECTOR")
  sheets <- readxl::excel_sheets(glue("{datos_ori}/{x}/General/{lista}"))
  patron <- sheets[grepl(paste(lista_nom, collapse = "|"), sheets, ignore.case = FALSE)]
  data <- read_excel(glue("{datos_ori}/{x}/General/{lista}"), sheet = patron) %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    filter(na != "") %>% mutate(total = as.numeric(total_2), time = x) %>%
    select(na, total, time)
  
  return(data)
  
}) %>% bind_rows()

years <- c(2015:2019)

doc2_col <- lapply(years, function(x){
  
  if(x < 2016){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "2-1.*")
  } 
  if(x > 2015 & x <= 2017){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "2-2.*")
  }
  if(x > 2017 & x <= 2019){
    lista <- list.files(path = glue("{datos_ori}/{x}/General"), pattern = "2-1.*")
  }
  
  lista_nom <- c("Sector", "sector")
  sheets <- readxl::excel_sheets(glue("{datos_ori}/{x}/General/{lista}"))
  patron <- sheets[grepl(paste(lista_nom, collapse = "|"), sheets, ignore.case = FALSE)]
  data <- read_excel(glue("{datos_ori}/{x}/General/{lista}"), sheet = patron) %>%
    drop_na(`...4`) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    select(na, total_2, total_3) %>% filter(na != "") %>% mutate( time = x)
  
  return(data)
  
}) %>% bind_rows()

nom_dpto <- nom_dpto %>% mutate (nivel_label = chartr("ÁÉÍÓÚ", "AEIOU", toupper(nom_dpto$nivel_label)))

doc2_col1 <- doc2_col %>% filter(time == c("2016"))
doc2_col2 <- doc2_col %>% filter(time == c("2017"))

doc2_col3 <- rbind(doc2_col1, doc2_col2) %>% rename(total = "total_2") %>% select(na, total, time)

doc2_col <- doc2_col %>% filter(time != "2017" & time != "2016") %>% rename(total = "total_3") %>%
  select(na, total, time)

doc_col <- rbind(doc1_col, doc2_col, doc2_col3) 

doc_col$na[doc_col$na == "Total Nacional"] <- "TOTAL NACIONAL"
doc_col$na[doc_col$na == "Nacional"] <- "TOTAL NACIONAL"

doc_col <- doc_col %>% left_join(matr_col, by = c("time", "na")) %>%
  mutate(total.y = as.numeric(total.y), total.x = as.numeric(total.x),
         value = total.y/total.x, nivel_label = chartr("ÁÉÍÓÚ", "AEIOU", toupper(doc_col$na)),
         id_data = "7", variable = "tamano_clase_prim", id_nivel = "dpto", 
         value_label = "Tamaño de clase en primaria", id_time = "1", nivel = chartr("ÁÉÍÓÚ", "AEIOU", nivel_label))

doc_col$nivel_label[doc_col$nivel_label == "BOGOTA, D.C."] <- "BOGOTA D.C."
doc_col$nivel_label[doc_col$nivel_label == "BOGOTA, D.C"] <- "BOGOTA D.C."
doc_col$nivel_label[doc_col$nivel_label == "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA"] <- "SAN ANDRES"
doc_col$nivel_label[doc_col$nivel_label == "VALLE"] <- "VALLE DEL CAUCA"
doc_col$nivel_label[doc_col$nivel_label == "ARCHIPIELAGO DE SAN ANDRES,"] <- "SAN ANDRES"
doc_col$nivel_label[doc_col$nivel_label == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
doc_col$nivel_label[doc_col$nivel_label == "NACIONAL"] <- "TOTAL NACIONAL"

doc_dpto <- doc_col %>% left_join(nom_dpto, by = "nivel_label") %>%
  filter(nivel_label %in% nom_dpto$nivel_label) %>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

doc_dpto <- rbind(doc_dpto, doc20_dpto)  %>% arrange(time, nivel_value)

# Extraer

write_csv(doc_dpto, glue("{datos}/base_tamano_clase_prim_dpto_2014-2020.csv"))

#-------------#
# Nacional ----
#-------------#

doc_nal20 <- doc20 %>% filter(dpto == "Nacional") %>%
  mutate(id_data = "7", variable = "tamano_clase_prim", id_nivel = "nacional", 
         value_label = "Tamaño de clase en primaria", id_time = "1",
         nivel_value = "1") %>% rename(nivel_label = "dpto") %>% 
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

doc_nal <- doc_col %>% filter(nivel_label == "TOTAL NACIONAL" & value != "") %>%
  mutate(nivel_value = "1", nivel_label = "nacional", id_nivel = "nacional")%>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

matr_nal16 <- matr_col %>% filter(time == "2016") 

doc_nal16 <- doc_col %>% filter(time == "2016" & nivel_label != "TOTAL NACIONAL") 

doc_ <- colSums(select(matr_nal16, contains("total")))
doc_2 <- colSums(select(doc_nal16, contains("total.x")))

doc16 <- data.frame(doc_, doc_2)

doc16 <- doc16 %>% mutate(nivel_value = "1", nivel_label = "nacional", value = doc_/doc_2, id_data = "7",
         variable = "tamano_clase_prim", id_nivel = "nacional", time = "2016",
         value_label = "Tamaño de clase en primaria", id_time = "1")%>%
  select(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, value)

doc_nal <- rbind(doc_nal, doc16, doc_nal20) %>% arrange(time)

# Extraer

write_csv(doc_nal, glue("{datos}/base_tamano_clase_prim_col_2014-2020.csv"))
