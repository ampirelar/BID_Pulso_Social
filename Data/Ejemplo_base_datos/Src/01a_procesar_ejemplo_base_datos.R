#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 24 agosto, 2021
# Descripcion corta del script
#-------------------------------------------------------#
# aaaa
#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Ejemplo_base_datos/Input"
datos <- "Data/Ejemplo_base_datos/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#