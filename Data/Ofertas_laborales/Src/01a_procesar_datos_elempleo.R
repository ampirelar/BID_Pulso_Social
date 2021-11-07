#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 12 octubre, 2021
# Generar dataset de El Empleo.com: limpia strings y agrega ID municipios a datos elempleo
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())

# Opciones
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
v <- as.numeric(args[1])

packageList<-c("tidyverse","textclean","readr")
suppressPackageStartupMessages(lapply(packageList,require,character.only=TRUE))
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

path <- "C:/Users/anapi/OneDrive - Universidad EAFIT/2021_BID_PulsoSocial/01_Analisis_empirico/01_Datos"
path_data <- paste0(path, "/Data/Ofertas_laborales/Input/output/")
path_data_modified <- paste0(path, "/Data/Ofertas_laborales/Output/elempleo/")
setwd(path)
options(scipen = 999)

#-------------------------------------------------------#
# 0. Cargar datos ----
#-------------------------------------------------------#

# FUNCTION: Remove white spaces and accents
trim <- function (x){replace_non_ascii(gsub("^\\s+|\\s+$", "", x))}

# MUNICIPALITIES: 0 is Colombia
mpios <- read.csv("Data/Ofertas_laborales/Input/col_mpios.csv",
                  sep=",",header=TRUE,encoding="UTF-8", allowEscapes=TRUE,quote="\"") %>%
          ## Fix Strings
          mutate(municipio=trim(tolower(municipio)),
                 depto=trim(tolower(depto)),
                 municipio=ifelse(municipio=="bogota, d.c.", "bogota",municipio),
                 municipio=gsub("\\s*\\([^\\)]+\\)","",municipio)) %>%
    ## Add all colombia
    rbind(c(0,0000, "colombia", "colombia"), .) %>% 
    distinct(municipio, .keep_all = TRUE)

### ELEMPLEO
# Sequence: names in a 10.000 sequence from last recorded id
brk1= 10000*v
brk2= 10000*(v-1)

last <- 1884909183
start = last - brk1
end = last -brk2

fil <- paste0(path_data,"elempleo_",start,"_",end,".csv")

# Open Dataset and add column names
ds <- read.csv(fil,sep=",",header=FALSE,encoding="UTF-8",allowEscapes=TRUE,quote="\"") %>%
    `colnames<-`(c("complete_title","title","location","date", "wage","job_sector","vacancies","profession",
                   "description","requirements","education_level","experience","contract","company_sector",
                   "company_sector_2","company","id"))

#-------------------------------------------------------#
# 1 Corregir strings y crear dummies ----
#-------------------------------------------------------#

### STRINGS
# Remove empty results and duplicated rows (according to descr, title and date)
ds <- ds %>% filter(description!="") %>% 
    distinct(description, .keep_all = TRUE)

# Normalizing all strings: lower cases, remove accents and trim white spaces
ds <- ds %>% mutate_all(tolower) %>%
    mutate_all(trim) %>%
    # Remove: " /" in profession and " vacante" in vacancies
    mutate(profession = gsub(" /", "", profession),
           vacancies = gsub(" vacantes", "", vacancies),
           vacancies = gsub(" vacante", "", vacancies)) %>%
    # Municipalities common typos
    mutate(location = gsub("toda colombia", "colombia", location),
           location = ifelse(location == "buga", "guadalajara de buga", location),
           location = ifelse(location == "tumaco", "san andres de tumaco", location),
           location = ifelse(location == "barranca", "barrancabermeja", location),
           location = ifelse(location == "sierra nevada de santamarta-resguardo kogui malayo arhuaco", "santa marta", location),
           location = ifelse(location == "el espinal", "espinal", location),
           location = ifelse(location == "la soledad", "soledad", location),
           location = ifelse(location == "puerto leguizamo", "leguizamo", location),
           location = ifelse(location == "san sebastian de mariquita", "mariquita", location))

# Organizing municipalities
bogota <- c("barrio norte","bogota alrededores","barrios unidos","carvajal")
internacional <- c("phoenix","mexico d.f","milwaukee","tel-aviv","miami","san antonio del norte",
                   "distrito central","culiacan rosales","mangaia","fort lauderdale","potters village",
                   "montevideo","montreal","sao paulo","denver","valladolid","chalchuapa","chiang mai","lima")

ds$location <- ifelse(ds$location %in% bogota, "bogota", ds$location)
ds$location <- ifelse(ds$location %in% internacional, "internacional", ds$location)

### DUMMIES
#check if job is formal, requires a specific gender, requires a Venezuelan, bilingual 
venezuela <- c("venezolano", "venezolana", "venezuela", "venezol")
formal <- c("prestaciones de ley", "prestaciones", "seguridad social", "contrato","smlv")
ingles <- c("bilingue","ingles","english","trilingue","language center")

ds <- ds %>% 
    mutate(venezuela = ifelse(grepl(paste(venezuela, collapse = "|"), description),1,0),
           formal = ifelse(grepl(paste(formal, collapse = "|"), description),1,
                           ifelse(contract != "",1,0)),
           ingles = ifelse(grepl(paste(ingles, collapse = "|"), description),1,
                           ifelse(grepl(paste(ingles, collapse = "|"), complete_title),1,0)),
           gender = ifelse(grepl("mujeres y hombres|mujer y hombre", description),0,
                                 ifelse(grepl("mujer", description, fixed = T),1,
                                        ifelse(grepl("hombre", description, fixed = T),2,0))))

#Replacing company_sector for company_sector_2 in case is.na(company_sector)
ds <- ds %>% mutate(company_sector = ifelse(company_sector=="",company_sector_2,company_sector)) %>%
    select(-company_sector_2)

#-------------------------------------------------------#
# 2. Agregar ID mpio ----
#-------------------------------------------------------#
#### 01 - Fix Location

    ## With Information
    ds_1 <- ds %>% filter(nchar(location)>0) %>%
              left_join(.,mpios,by=c("location"="municipio"))
    ds_1[is.na(ds_1$codmpio),]$location
    ## Without Information
    ds_2 <- ds %>% filter(nchar(location)==0)

    idx2 <- sapply(mpios$municipio, grep, ds_2$complete_title)
    idx1 <- sapply(seq_along(idx2), function(i) rep(i, length(idx2[[i]])))
    ds_2 <- cbind(ds_2[unlist(idx2),,drop=F],mpios[unlist(idx1),,drop=F])
    ds_2 <- ds_2[!duplicated(ds_2$complete_title),] %>%
                    mutate(location=municipio) %>%
                    select(-municipio)

    ds_end <- rbind(ds_1,ds_2)

rm(ds_1,ds_2,idx1,idx2)
write_csv(ds_end,paste0(path_data_modified,"all_post_",v+1482,".csv"))

