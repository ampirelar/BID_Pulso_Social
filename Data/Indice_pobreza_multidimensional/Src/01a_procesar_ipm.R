#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 15 sept, 2021
# Procesamiento de datos de Indice de Pobreza Multidimensional
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readxl)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Indice_pobreza_multidimensional/Input"
datos <- "Data/Indice_pobreza_multidimensional/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Departamental ----
#-------------------------------------------------------#

ipm <- read_excel(glue("{datos_ori}/anexo_dptal_pobreza_multidimensional_20.xls"), sheet = "IPM_Departamentos") %>%
  drop_na(`...3`)

# Organizar datos
colnames(ipm)[1] <- "departamento"
ipm <- ipm %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(departamento = na)


#-------------------------------------------------------#
# 2. Nacional ----
#-------------------------------------------------------#

