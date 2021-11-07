#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# --- JOBPOSTS COMPUTRABAJO ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Project:            LABOR DISCRIMINATION
#   Modification date:  Aug 04 2020
#   This script gets main information from computrabajo offers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())
packageList<-c("tidyverse","textclean","scales","stringr","rvest","plyr")
suppressPackageStartupMessages(lapply(packageList,require,character.only=TRUE))

path_data <- "data/raw/output_computrabajo/"
path_new_data <- "data/raw/output_cleaned_computrabajo/"
path <- paste0(getwd(), "/")

#~~~~~~~~~~~~~~~~~~~~~~#
# --- LOAD DATASET ----
#~~~~~~~~~~~~~~~~~~~~~~~#

trim <- function (x){textclean::replace_non_ascii(gsub("^\\s+|\\s+$", "", x))}

#Remove urls from past offers ----
new_urls <- read.csv(paste0(path_data, "computrabajo_feb7.csv"), header=FALSE) %>%
  mutate_all(trim) %>%  mutate_all(tolower) %>%
  `colnames<-`(c("url","title","department", "location","company","date"))

old_urls <- readRDS(paste0(path,"data/cleaned/computrabajo.rds")) %>%
  select(url) %>% mutate_all(tolower) %>% mutate_all(trim)

duplicated_url <- inner_join(new_urls,old_urls) %>%
  select(url)

computrabajo <- anti_join(new_urls, duplicated_url)
rm(new_urls,old_urls,duplicated_url)

# corregir un caso atipico (26 agosto)
# atipico <- computrabajo %>% filter(date == "14 diciembre") %>%
#   mutate(date = gsub("(.*),.*", "\\1", date)) %>%
#   mutate(date = gsub("ayer", "19 septiembre", date)) %>%
#   mutate(date = gsub("hoy", "20 septiembre", date)) %>%
#   tidyr::separate(date, into = c("day", "month"), sep = "\\s") %>%
#   mutate(month = gsub("diciembre", 12, month)) %>%
#   mutate(year = 2019) %>%
#   mutate(date = paste(year,month,day, sep = "-"))%>%
#   mutate(date = readr::parse_date(as.character(date), "%Y-%m-%d", locale = locale("es"))) %>%
#   mutate(year = scales::date_format(format = "%Y")(date),
#          month = scales::date_format(format = "%b")(date),
#          dow = scales::date_format(format = "%A")(date),
#          week_number = strftime(date, format = "%V"),
#          month_year = scales::date_format(format = "%Y%b")(date)) %>%
#   mutate(dow = recode(dow, lunes = "Monday", martes = "Tuesday", miércoles = "Wednesday", jueves = "Thursday", viernes = "Friday", sábado = "Saturday", domingo = "Sunday"),
#          month = recode(month, dic. = "Dec"),
#          dow = ordered(dow), month = ordered(month)) %>%
#   dplyr::distinct() %>%
#   mutate(month_year = recode(month_year, "2019dic." = "2019Dec"),
#          month_year = ordered(month_year))

#organize dataset ----
computrabajo <- computrabajo %>%
  mutate(date = gsub("(.*),.*", "\\1", date)) %>%
  mutate(date = gsub("ayer", "6 febrero", date)) %>%
  mutate(date = gsub("hoy", "7 febrero", date)) %>%
  tidyr::separate(date, into = c("day", "month"), sep = "\\s") %>%
  mutate(month = gsub("enero", 1, month)) %>%
  mutate(month = gsub("febrero", 2, month)) %>%
  # mutate(month = gsub("marzo", 3, month)) %>%
  # mutate(month = gsub("abril", 4, month)) %>%
  # mutate(month = gsub("mayo", 5, month)) %>%
  # mutate(month = gsub("junio", 6, month)) %>%
  # mutate(month = gsub("julio", 7, month)) %>%
  # mutate(month = gsub("agosto", 8, month)) %>%
  # mutate(month = gsub("septiembre", 9, month)) %>%
  # mutate(month = gsub("octubre", 10, month)) %>%
  mutate(month = gsub("noviembre", 11, month)) %>%
  mutate(month = gsub("diciembre", 12, month)) %>%
  mutate(year = ifelse(month == "12" | month == "11", 2020, 2021)) %>%
  mutate(date = paste(year,month,day, sep = "-"))%>%
  mutate(date = readr::parse_date(as.character(date), "%Y-%m-%d", locale = locale("es"))) %>%
  mutate(year = scales::date_format(format = "%Y")(date),
         month = scales::date_format(format = "%b")(date),
         dow = scales::date_format(format = "%A")(date),
         week_number = strftime(date, format = "%V"),
         month_year = scales::date_format(format = "%Y%b")(date)) %>%
  mutate(dow = recode(dow, lunes = "Monday", martes = "Tuesday", miércoles = "Wednesday", jueves = "Thursday", viernes = "Friday", sábado = "Saturday", domingo = "Sunday"),
         month = recode(month, ene. = "Jan", feb. = "Feb", mar. = "Mar", abr. = "Apr", may. = "May",
                        jun. = "Jun", jul. = "Jul", ago. = "Aug", sep. = "Sep", oct. = "Oct", nov. = "Nov", dic. = "Dec"),
         dow = ordered(dow), month = ordered(month)) %>%
  dplyr::distinct() %>%
  mutate(month_year = recode(month_year, "2020nov" = "2020Nov","2020dic" = "2020Dec", "2021ene." = "2021Jan", "2021feb." = "2021Feb"),
         month_year = ordered(month_year))

# # eliminar caso atipico de base (26 agosto)
# computrabajo <- computrabajo %>% filter(url != "/ofertas-de-trabajo/oferta-de-trabajo-de-cajera-turno-vesperitno-1pm-a-9pm-en-guadalajara-ae7f78ce31ea83bc61373e686dcf3405")
# computrabajo <- bind_rows(computrabajo, atipico)
# rm(atipico)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ---- GET INFORMATION ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
info <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("direccion","wage","description","requirements","contract","working_hours"))

fun_information <- function(x){
  direccion <- computrabajo$url[x]
  print(x)

  info_pagina <- tryCatch(
    {
      pagina_actual <- read_html(paste0("https://www.computrabajo.com.co",direccion))
      
      wage <- pagina_actual %>%
        html_nodes(xpath = "//*[text()='Salario']/following-sibling::p") %>%
        html_text() %>% gsub("^\\s+|\\s+$", "", .)
      
      description <- pagina_actual %>%
        html_nodes(xpath = "//*[text()='Descripción']/following-sibling::li") %>%
        html_text() %>% gsub("^\\s+|\\s+$", "", .) %>% paste(., collapse = " ")
      
      requirements <-  pagina_actual %>%
        html_nodes(xpath = "//*[text()='Requerimientos']/following-sibling::li") %>%
        html_text() %>% paste(., collapse = " -new_req: ")
      
      contract <- pagina_actual %>%
        html_nodes(xpath = "//*[text()='Tipo de contrato']/following-sibling::p/text()") %>%
        html_text() %>% gsub("^\\s+|\\s+$", "", .)
      
      working_hours <- pagina_actual %>%
        html_nodes(xpath =  "//*[text()='Jornada']/following-sibling::p/text()") %>%
        html_text() %>% gsub("^\\s+|\\s+$", "", .)
      
      #Does it exist?
      error <- pagina_actual %>%
        html_nodes(xpath =  "//*[@id='main-message']/h1/span") %>%
        html_text() 
      
      #Rbind and check if missing webpage
      info_pagina <- as.data.frame(cbind(direccion, wage, description,requirements,contract,working_hours))
    },
    error = function(e){
      message(e)
    }
  )
}

test <- lapply(1:dim(computrabajo)[1], fun_information) %>% ldply()
# test <- lapply(1:5, fun_information) %>% ldply()
#~~~~~~~~~~~~~~~#
# ---- SAVE ----
#~~~~~~~~~~~~~~~#

cleaned_posts <- test %>% mutate(url = direccion) %>% select(-direccion)
test2 <- merge(computrabajo, cleaned_posts, by = "url")

saveRDS(test2, file = paste0(path_new_data,"computrabajo_complete_posts_feb7.rds"))

