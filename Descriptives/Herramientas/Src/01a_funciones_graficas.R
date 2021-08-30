#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 30 agosto, 2021
# Funciones para creacion de graficas
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "Descriptives/Herramientas/Input"
datos <- "Descriptives/Herramientas/Output"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#

#--------------------------#
# A. fun_data ----
# Organiza los datos para construccion de graficas
#--------------------------#

fun_data <- function(df){
  
  # Etiquetas nombres departamentos
  nom_dpto <- readxl::read_xlsx(glue("{datos_ori}/base_nombres_departamentos.xlsx")) 
  
  # Identificar si existen desagregaciones en los datos (genero, jovenes, minorias)
  # Sin desagregacion
  if(unique(df$id_nivel) == "dpto"){
    data <- df %>% left_join(nom_dpto, by = "nivel_value")
  } 
  
  # Desagregacion de genero
  if(grepl("gen", unique(df$id_nivel))){
    data <- df %>%
      dplyr::rename(nivel_value_ori = nivel_value) %>%
      mutate(nivel_value = as.numeric(gsub("\\_.*", "", nivel_value_ori)),
             genero = gsub(".*\\_", "", nivel_value_ori),
             genero = ifelse(genero == 1, "Mujer", "Hombre")) %>% 
      left_join(nom_dpto, by = "nivel_value")
  }
  
  return(data)
}

#--------------------------#
# B. fun_graph ----
# Crea graficos segun el estilo indicado
#--------------------------#

fun_graph <- function(df, type, palette, statistic, path, style){
  
  # Tema
  theme <- list(
    theme_classic(base_size = text*1.5),
    theme(axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())
  )
  
  # df <- data
  # type <-  "promedio_anual"
  # style <- "bar"
  # statistic <- "mean"
  # palette <- bar_colors1
  
  # Crear etiquetas ejes X y Y 
  xlab_time <- ifelse(unique(df$id_time) == 1, "Año",
                      ifelse(unique(df$id_time == 2, "Mes", 
                                    ifelse(unique(df$time) == 3, "Día", NULL))))
  xlab_terr <- ifelse(grepl("dpto", unique(df$id_nivel)), "Departamento",
                      ifelse(grepl("mpio", unique(df$id_nivel)), "Municipio", NULL))
  ylab <- unique(df$value_label)
  
  # Etiquetas de grafica de promedios temporales (anios, meses, dias)
  options_time <- list(
    xlab(glue("\n{xlab_time}")),
    ylab(glue("{ylab}\n"))
  )
  
  # Etiquetas de grafica de promedios territoriales (departamentos, municipios)
  options_terr <- list(
    xlab(glue("\n{xlab_terr}")), 
    ylab(glue("{ylab}\n"))
  )
  
  print(glue("Graficando {ylab}, estadistica: {statistic}, tipo: {type}"))
  
  # A. Barras ----
  if(type == "promedio_anual" & style == "bar"){
    graph <- ggplot(data = df, aes(x = time, y = value)) +
      geom_bar(stat = "summary", fun = statistic, fill = palette[1]) +
      options_time + theme
  }
  
  if(type == "promedio_dpto" & style == "bar"){
    graph <- ggplot(data = df, aes(x = nivel_label, y = value, fill = nivel_label)) +
      geom_bar(stat = "summary", fun = statistic) +
      options_terr + theme + 
      scale_fill_manual(values =  rep(palette, 5))
  }
  
  if(type == "promedio_anual_gen" & style == "bar"){
    graph <- ggplot(data = data, aes(x = time, y = value, fill = genero)) +
      geom_bar(stat = "summary", fun = statistic, position = position_dodge()) +
      options_time + theme + 
      scale_fill_manual(values =  rep(palette, 5))
  }
  
  # B. Lineas ----
  if(type == "promedio_anual" & style == "lines"){
    graph <- ggplot(data = df, aes(x = time, y = value)) +
      geom_line(stat = "summary", fun = statistic, color = palette[1], size = s, alpha = a_line) +
      geom_point(stat = "summary", fun = statistic, color = palette[1], size = s*2, alpha = a_dot) +
      options_time + theme +
      scale_x_continuous(breaks = years_break)
  }
  
  if(type == "promedio_anual_gen" & style == "lines"){
    graph <- ggplot(data = df, aes(x = time, y = value, group = genero)) +
      geom_line(stat = "summary", fun = statistic, aes(color = genero), size = s, alpha = a_line) +
      geom_point(stat = "summary", fun = statistic, aes(color = genero), size = s*2, alpha = a_dot) +
      options_time + theme +
      scale_color_manual(values = rep(palette, 5)) +
      scale_x_continuous(breaks = years_break)
  }
  
  if(type == "promedio_dpto" & style == "lines"){
    graph <- ggplot(data = df, aes(x = time, y = value, group = nivel_label)) +
      geom_line(stat = "summary", fun = statistic, aes(color = nivel_label), size = s, alpha = a_line) +
      geom_point(stat = "summary", fun = statistic, aes(color = nivel_label), size = s*2, alpha = a_line) +
      options_time + theme + 
      scale_color_manual(values = rep(palette, 5)) +
      scale_x_continuous(breaks = years_break)
  }
  
  # Grafica
  plot(graph)
  
  # Guardar grafica
  var <- unique(df$variable)
  period <- glue("{min(df$time)}-{max(df$time)}")
  ggsave(glue("{path}/output/grafica_{var}_{type}_{style}_{period}.jpeg"), height = h, width = w, dpi = d)
  
}

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Abrimos datos de ejemplo
geih <- readxl::read_xlsx("Data/Ejemplo_base_datos/Output/geih_datos_ejemplo.xlsx", sheet = "departamental") %>%
  mutate(value_label = "Tasa de desempleo (%)")

# geih_gen <- readxl::read_xlsx("Data/Ejemplo_base_datos/Output/geih_datos_ejemplo.xlsx", sheet = "departamental_genero") %>%
#   mutate(value_label = "Tasa de desempleo (%)")

# Preparar datos
data <- fun_data(geih)
# data <- fun_data(geih_gen)

#-------------------------------------------------------#
# 2. Opciones ----
#-------------------------------------------------------#

# Tamanos graficas (ancho, alto, texto, resolucion)
w <- 4.5*2.5
h <- 3.2*2.5
text <- 14
d <- 900
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

# Colores
# Fuentes: 
# 1. https://publications.iadb.org/publications/spanish/document/Pulso-social-en-Am%C3%A9rica-Latina-y-el-Caribe-2017-Legado-familiar-%C2%BFrompemos-el-molde-o-repetimos-patrones.pdf
# 2. https://publications.iadb.org/publications/spanish/document/Pulso-social-de-Am%C3%A9rica-Latina-y-el-Caribe-2016-Realidades-y-perspectivas.pdf

bar_colors1 <- c("#3d5574", "#74ac54", "#b8544c", "#c0ac84", "#c8dccc", "#b0c4e4", "#d4742c")
lines_colors1 <- c("#3d5574", "#74ac54")
countries_colors1 <- c("#759cd4", "#c43424", "#ecbc24", "#385676", "#d4742d","#6dad55", "#a4a4a4", "#643d94")

bar_colors2 <- c("#4c9ccc", "#e4546c", "#fcac24", "#0c0c0c", "#b4b4b4")
lines_colors2 <- c("#4c9ccc", "#e4546c", "#fcac24", "#0c0c0c")

# Cortes de anios (etiquetas eje X)
years_break <- c(2001, 2005, 2010, 2015, 2019)

#-------------------------------------------------------#
# 3. Graficas ----
#-------------------------------------------------------#

# Barras
fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
          style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")

fun_graph(df = data, type = "promedio_dpto", palette = countries_colors1, statistic = "mean", 
          style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")

fun_graph(df = data, type = "promedio_anual_gen", palette = bar_colors1, statistic = "mean", 
          style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")


# Lineas
fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
          style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")

fun_graph(df = data, type = "promedio_dpto", palette = countries_colors1, statistic = "mean", 
          style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")

fun_graph(df = data, type = "promedio_anual_gen", palette = bar_colors1, statistic = "mean", 
          style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")

#-------------------------------------------------------#
# 4. Mapas ----
#-------------------------------------------------------#





