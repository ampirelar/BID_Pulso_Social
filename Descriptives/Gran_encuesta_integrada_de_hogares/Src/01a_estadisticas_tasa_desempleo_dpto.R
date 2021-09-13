#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 6 sept, 2021
# Estadisticas descriptivas GEIH
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr)
# .rs.restartR()

# Abrir funciones de graficas
source("Descriptives/Herramientas/Src/01a_funciones_graficas.R")

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Data/Gran_encuesta_integrada_de_hogares/Input"
datos <- "Data/Gran_encuesta_integrada_de_hogares/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Departamental ----
#-------------------------------------------------------#

data_geih <- read_csv(glue("{datos}/base_tasa_desempleo_dpto_2001-2020.csv"))

# Preparar datos
data <- fun_data(data_geih)

# Barras
grafica1 <- fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
                      style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")

grafica2 <- fun_graph(df = data, type = "promedio_dpto", palette = bar_colors2[1], statistic = "mean", 
                      style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo",
                      g_theme = custom_theme)

# Implementar para escoger tasa desempleo de 1 year

# Ejemplo de graficas multiples
Rmisc::multiplot(grafica1, grafica2, cols = 2)

# Lineas
grafica <- fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
          style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")

grafica <- fun_graph(df = data, type = "promedio_dpto", palette = countries_colors1, statistic = "mean",
          style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")

# Mapas
data_map <- fun_map(df = data, time_map = 2020, classes = 5)
fun_graph(df = data_map, palette = map_cont2, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")

#-------------------------------------------------------#
# 2. Genero-Departamental ----
#-------------------------------------------------------#

data_geih_gen <- read_csv(glue("{datos}/base_tasa_desempleo_dpto_gen_2001-2020.csv"))

# Preparar datos
data <- fun_data(data_geih_gen)

# Barras
grafica1 <- fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
                      style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")

grafica2 <- fun_graph(df = data, type = "promedio_anual_gen", palette = bar_colors1, statistic = "mean",
          style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")

# Lineas
grafica3 <- fun_graph(df = data, type = "promedio_anual_gen", palette = bar_colors1, statistic = "mean",
          style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")

# Ejemplo para promediar variable de categorias poblacionales
data_map <- fun_map(df = data, time_map = 2020, classes = 5, statistic = "mean")
fun_graph(df = data_map, palette = map_cont1, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")

# Ejemplo para elegir categoria de desagregacion poblacional
# Implementar paleta uniforme para comparar mapas por categoria poblacional
data_map <- fun_map(df = data, time_map = 2020, classes = 5, pob_cat = "Mujer")
mujer <- fun_graph(df = data_map, palette = map_cont2, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")
data_map <- fun_map(df = data, time_map = 2020, classes = 5, pob_cat = "Hombre")
hombre <- fun_graph(df = data_map, palette = map_cont2, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")

Rmisc::multiplot(mujer, hombre, cols = 2)
