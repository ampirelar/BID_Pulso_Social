#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 27 sept, 2021
# Transformar datos Encuesta de Demografia y Salud
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl, janitor, sf)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Encuesta_de_demografía_y_salud/Input"
datos <- "Data/Encuesta_de_demografía_y_salud/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Abrir datos ----
# Leemos los archivos shp y guardamos como csvs
#-------------------------------------------------------#

years <- c(1986, 1990, 1995, 2000, 2005, 2010, 2015)

# Sub-nacional (departamental/regional)
lapply(years, function(x){
  print(x)
  data <- st_read(glue("{datos_ori}/DHS_Subnacional/shps/sdr_subnational_data_dhs_{x}.shp"))
  names(data) <- tolower(names(data))
  st_geometry(data) <- NULL
  write_csv(data, glue("{datos_ori}/DHS_Subnacional/csv/sdr_subnational_data_dhs_{x}.csv"))
})

# Paises
data <- st_read(glue("{datos_ori}/DHS_Paises/shps/sdr_national_data.shp"))
names(data) <- tolower(names(data))
st_geometry(data) <- NULL
write_csv(data, glue("{datos_ori}/DHS_Paises/csv/sdr_national_data.csv"))

  