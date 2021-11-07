#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 12 octubre, 2021
# Limpiar dataset de El empleo
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, textclean, scales, plyr, readxl)

#--------------------------#
# paths ----
#--------------------------#

path_data <- paste0(path, "/Data/Ofertas_laborales/Input/output/")
path_new_data <- paste0(path, "/Data/Ofertas_laborales/Output/elempleo/cleaned/")
options(scipen = 999)

trim <- function (x){replace_non_ascii(gsub("^\\s+|\\s+$", "", x))}

#-------------------------------------------------------#
# 1. Limpiar datos ----
#-------------------------------------------------------#

## Open Dataset 
files_all <- list.files(pattern="*.csv", full.names=TRUE, paste0(path_data,"output_cleaned_elempleo"))
elempleo <-  ldply(files_all, read_csv) 
#elempleo <- read_csv("data/raw/output_cleaned_elempleo/all_post_0.csv") 
elempleo_v0 <- elempleo

#Add year, month, week number and day of week
elempleo <- elempleo %>%
  mutate(date_string = date, 
         date =  parse_date(as.character(date), "%Y-%m-%d", locale = locale("es")),
         year = date_format(format = "%Y")(date),
         month = date_format(format = "%b")(date),
         dow = date_format(format = "%A")(date),
         week_number = date_format(format = "%U")(date),
         month_year = date_format(format = "%Y%b")(date),
         profession = ifelse(profession=="otros","otra",profession),
         #Modify order of wages, weekdays, months and education so that they appear well in graphs
         wage =  ordered(wage, levels = c("salario a convenir", "menos de $1 millon", "$1 a $1,5 millones",
                                          "$1,5 a $2 millones","$2 a $2,5 millones","$2,5 a $3 millones",
                                          "$3 a $3,5 millones","$3,5 a $4 millones","$4 a $4,5 millones",
                                          "$4,5 a $5,5 millones", "$5,5 a $6 millones", "$6 a $8 millones",
                                          "$8 a $10 millones","$10 a $12,5 millones", "$12,5 a $15 millones",
                                          "$15 a $18 millones","$18 a $21 millones","mas de $21 millones")))  %>%
  mutate(wage_groupped =  fct_collapse(wage,
                                       "salario a convenir" = c("salario a convenir"),
                                       "menos de $1 millon" = "menos de $1 millon",
                                       "$1 a $2 millones" = c("$1 a $1,5 millones", "$1,5 a $2 millones"),
                                       "$2 a $3 millones" = c("$2 a $2,5 millones", "$2,5 a $3 millones"),
                                       "mas de $3 millones" = c("$3 a $3,5 millones", "$3,5 a $4 millones",
                                                                "$4 a $4,5 millones","$4,5 a $5,5 millones",
                                                                "$5,5 a $6 millones", "$6 a $8 millones",
                                                                "$8 a $10 millones","$10 a $12,5 millones", "$12,5 a $15 millones",
                                                                "$15 a $18 millones","$18 a $21 millones",
                                                                "mas de $21 millones")
  )) %>%
  mutate(year_week = paste(year, week_number),
         day_of_year =date_format(format = "%j")(date)) %>%
  mutate(dow = ordered(dow, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")),
         month = ordered(month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
         education_level = ifelse(education_level =="formacion tec profesional" | education_level == "tecnico laboral","tecnologica", education_level)) %>%
  mutate(contract = gsub("contrato","",contract))%>%
  mutate(contract = tolower(trim(contract)),
         company_sector = tolower(trim(company_sector)),
         job_sector = tolower(trim(job_sector))) %>%
  mutate(contract = ifelse(contract == "definid", "definido", contract)) %>%
  mutate(contract = ifelse(contract == "contra" | contract == "contrat" | contract == "contr", "otro",contract)) %>%
  mutate(title = tolower(trim(title)),
         requirements = tolower(trim(requirements)),
         description = tolower(trim(description))) %>%
  #Do they mention teletrabajo?
  mutate(teletrabajo = ifelse(grepl("teletrabajo|trabajo a distancia|home office|oficina en el hogar", title), 1,
                              ifelse(grepl("teletrabajo|trabajo a distancia|home office|oficina en el hogar", requirements),1,
                                     ifelse(grepl("teletrabajo|trabajo a distancia|home office|oficina en el hogar", description),1,0))),
         covid = ifelse(date >= parse_date(as.character("2020-03-06"), "%Y-%m-%d", locale = locale("es")), 1,0))  %>% 
  distinct(id, .keep_all = TRUE)  %>% 
  distinct(description, month_year, .keep_all = TRUE)

#Order factors
elempleo$education_level = ordered(elempleo$education_level, levels=c("preescolar","basica primaria (1 - 5)", "basica secundaria (6 - 9)",
                                                                  "media (10 - 13)", "tecnologica", "universitaria","especializacion",
                                                                  "maestria","doctorado"))
elempleo$ingles <- factor(elempleo$ingles, levels=c(0,1), labels=c('not specified','required'))
elempleo$formal <- factor(elempleo$formal, levels=c(0,1), labels=c('Not specified','Formal'))
elempleo$venezuela <- factor(elempleo$venezuela, levels=c(0,1), labels=c('not specified','Venezuelan'))
elempleo$gender <- factor(elempleo$gender, levels=c(0,1,2), labels=c('not specified','woman','man'))  

# Group job sectors according to ciiu
elempleo <- left_join(elempleo, 
                      read_excel("documents/classification_sectors.xlsx", sheet = "company_sector"),
                      by = "company_sector")

elempleo <- left_join(elempleo, 
                      read_excel("documents/classification_sectors.xlsx", sheet = "job_sector"),
                      by = "job_sector")

max(elempleo$date, na.remove = TRUE)

saveRDS(elempleo, paste0(path_new_data,"elempleo.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ---- ELEMPLEO COSTA RICA ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Open Dataset 
files_all <- list.files(pattern="*.csv", full.names=TRUE, paste0(path_data,"output_cleaned_elempleo_cr"))
elempleo_cr <-  ldply(files_all, read_csv) 
#elempleo_cr <- read_csv("data/raw/output_cleaned_elempleo_cr/all_post_0.csv") 
elempleo_cr_v0 <- elempleo_cr

#Add year, month, week number and day of week
elempleo_cr <- elempleo_cr %>%
  mutate(date_string = date, 
         date =  parse_date(as.character(date), "%Y-%m-%d", locale = locale("es")),
         year = date_format(format = "%Y")(date),
         month = date_format(format = "%b")(date),
         dow = date_format(format = "%A")(date),
         week_number = date_format(format = "%U")(date),
         month_year = date_format(format = "%Y%b")(date),
         profession = ifelse(profession=="otros","otra",profession),
         #Modify order of wages, weekdays, months and education so that they appear well in graphs
         wage =  ordered(wage, levels = c("sin definir", "salario confidencial", "menos de cent 250 mil",
                                          "cent 250 a 350 mil","cent 350 a 450 mil","cent 450 a 550 mil",
                                          "cent 550 a 650 mil","cent 650 a 750 mil","cent 750 mil a 1 millon",
                                          "cent 1 a 1,5 millones", "cent 1,5 a 2 millones", "cent 2 a 2 ,5 millones",
                                          "cent 2,5 a 3, millones","cent 3 a 3,5 millones", "mas de cent 3,5 millones")))  %>%
  mutate(wage_groupped =  fct_collapse(wage,
                                       "sin definir" = c("sin definir", "salario confidencial"),
                                       "menos de cent 250 mil" = "menos de cent 250 mil",
                                       "cent 250 a 550 mil" = c("cent 250 a 350 mil","cent 350 a 450 mil","cent 450 a 550 mil"),
                                       "cent 550 a 1 millon" = c("cent 550 a 650 mil","cent 650 a 750 mil","cent 750 mil a 1 millon"),
                                       "mas de 1 millon" = c("cent 1 a 1,5 millones", "cent 1,5 a 2 millones", "cent 2 a 2 ,5 millones",
                                                             "cent 2,5 a 3, millones","cent 3 a 3,5 millones", "mas de cent 3,5 millones"))) %>%
  mutate(year_week = paste(year, week_number),
         day_of_year =date_format(format = "%j")(date)) %>%
  mutate(dow = ordered(dow, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")),
         month = ordered(month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
         education_level = ifelse(education_level =="tecnico medio" | education_level == "tecnico universitario","tecnico", education_level)) %>%
  mutate(contract = gsub("contrato","",contract))%>%
  mutate(contract = tolower(trim(contract)),
         company_sector = tolower(trim(company_sector)),
         job_sector = tolower(trim(job_sector))) %>%
  mutate(contract = ifelse(contract == "definid", "definido", contract)) %>%
  mutate(contract = ifelse(contract == "contra" | contract == "contrat" | contract == "contr", "otro",contract)) %>%
  mutate(title = tolower(trim(title)),
         requirements = tolower(trim(requirements)),
         description = tolower(trim(description))) %>%
  #Do they mention teletrabajo?
  mutate(teletrabajo = ifelse(grepl("teletrabajo|trabajo a distancia|home office|oficina en el hogar", title), 1,
                              ifelse(grepl("teletrabajo|trabajo a distancia|home office|oficina en el hogar", requirements),1,
                                     ifelse(grepl("teletrabajo|trabajo a distancia|home office|oficina en el hogar", description),1,0))),
         covid = ifelse(date >= parse_date(as.character("2020-03-06"), "%Y-%m-%d", locale = locale("es")), 1,0))  %>% 
  distinct(id, .keep_all = TRUE)  %>% 
  distinct(description, month_year, .keep_all = TRUE)

#Order factors
elempleo_cr$ingles <- factor(elempleo_cr$ingles, levels=c(0,1), labels=c('not specified','required'))
elempleo_cr$formal <- factor(elempleo_cr$formal, levels=c(0,1), labels=c('Not specified','Formal'))
elempleo_cr$venezuela <- factor(elempleo_cr$venezuela, levels=c(0,1), labels=c('not specified','Venezuelan'))
elempleo_cr$gender <- factor(elempleo_cr$gender, levels=c(0,1,2), labels=c('not specified','woman','man'))  

# Group job sectors according to ciiu
elempleo_cr <- left_join(elempleo_cr, 
                         read_excel("documents/classification_sectors.xlsx", sheet = "company_sector"),
                         by = "company_sector")

elempleo_cr <- left_join(elempleo_cr, 
                         read_excel("documents/classification_sectors.xlsx", sheet = "job_sector"),
                         by = "job_sector")
max(elempleo_cr$date, na.remove = TRUE)

saveRDS(elempleo_cr, paste0(path_new_data,"elempleo_cr.rds"))

