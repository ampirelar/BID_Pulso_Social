#-------------------------------------------------------#
# Pulso Social BID ----
# Ultima fecha de modificacion: 6 sept, 2021
# Funciones para creacion de graficas
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

pacman::p_load(tidyverse, glue, sf)
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
  nom_dpto <- readxl::read_xlsx("Descriptives/Herramientas/Input/base_nombres_departamentos.xlsx")
  
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
             nivel_pob = gsub(".*\\_", "", nivel_value_ori),
             nivel_pob = ifelse(nivel_pob == 2, "Mujer", "Hombre")) %>% 
      left_join(nom_dpto, by = "nivel_value")
  }
  
  return(data)
}

#--------------------------#
# B. fun_map ----
# Prepara informacion para mapas
#--------------------------#

fun_map <- function(df, time_map, classes, pob_cat, statistic){
  
  # Abrir mapa departamentos
  map_dptos <- st_read(glue("Data/Mapas/Output/mapa_departamentos_colombia.shp")) %>% 
    rename(nivel_label = nvl_lbl, id_nivel = id_nivl, nivel_value = nivl_vl) %>% 
    dplyr::select(id_nivel, nivel_label, nivel_value, X, Y)
  
  # Desagregacion de nivel poblacional
  if(missing(statistic)){
    statistic <- "none"
  }
  
  if(grepl("gen", unique(df$id_nivel)) & statistic == "mean"){
    df <- df %>%
      group_by(id_data, variable, id_nivel, nivel_value, id_time, time, value_label, nivel_label) %>%
      summarise(value = mean(value, na.rm = T)) %>%
      ungroup()
  }
  
  # Filtro nivel poblacional
  if(grepl("gen", unique(df$id_nivel)) & !(missing(pob_cat))){
    df <- df %>% dplyr::filter(nivel_pob == pob_cat)
  }
  
  # Unir datos con mapa y elegimos periodo a mapear
  df <- map_dptos %>% 
    left_join(df %>% subset(time == time_map), by = "nivel_value") %>%
    dplyr::select(-c(id_nivel.y, nivel_label.y)) %>% 
    rename(id_nivel = id_nivel.x, nivel_label = nivel_label.x)
  
  # Separar variable en cuantiles
  q1 <- quantile(df$value, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
  df$q_value <- cut(df$value, breaks = q1, include.lowest = T, dig.lab = 2)
  
  # Solo permitimos etiquetas a poligonos con informacion
  df$nivel_label[is.na(df$value)] <- NA
  
  return(df)
}

#--------------------------#
# C. fun_graph ----
# Crea graficos segun el estilo indicado
#--------------------------#

fun_graph <- function(df, type, palette, statistic, path, style, g_theme){
  
  # Temas por default
  if(missing(g_theme)){
    theme <- list(
      theme_classic(base_size = text*1.5),
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank()))
    
    theme_map <- list(
      theme_minimal(base_size = text),
      theme(legend.title = element_blank(),
            legend.direction = "horizontal", 
            legend.position = "bottom",
            legend.box = "horizontal",
            panel.grid = element_blank(), 
            panel.border = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank()))
  } else {
    theme <- g_theme
    theme_map <- g_theme
  }
  
  # Crear etiquetas ejes X y Y 
  xlab_time <- ifelse(unique(na.omit(df$id_time)) == 1, "Año",
                      ifelse(unique(na.omit(df$id_time) == 2, "Mes", 
                                    ifelse(unique(na.omit(df$time)) == 3, "Día", NULL))))
  xlab_terr <- ifelse(grepl("dpto", unique(na.omit(df$id_nivel))), "Departamento",
                      ifelse(grepl("mpio", unique(na.omit(df$id_nivel))), "Municipio", NULL))
  ylab <- unique(na.omit(df$value_label))
  
  # Etiquetas de grafica de promedios temporales (anios, meses, dias)
  options_time <- list(
    xlab(glue("\n{xlab_time}")),
    ylab(glue("{ylab}\n")))
  
  # Etiquetas de grafica de promedios territoriales (departamentos, municipios)
  options_terr <- list(
    xlab(glue("\n{xlab_terr}")), 
    ylab(glue("{ylab}\n")))
  
  # Etiquetas de mapas
  options_map <- list(
    xlab(glue("{ylab}\n")))
  
  if(missing(type) & missing(statistic)){
    type <- style
    statistic <- "no aplica"
  } 
  
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
      scale_fill_manual(values =  rep(palette, 40))
  }
  
  if(type == "promedio_anual_gen" & style == "bar"){
    graph <- ggplot(data = data, aes(x = time, y = value, fill = nivel_pob)) +
      geom_bar(stat = "summary", fun = statistic, position = position_dodge()) +
      options_time + theme + 
      scale_fill_manual(values =  rep(palette, 40))
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
    graph <- ggplot(data = df, aes(x = time, y = value, group = nivel_pob)) +
      geom_line(stat = "summary", fun = statistic, aes(color = nivel_pob), size = s, alpha = a_line) +
      geom_point(stat = "summary", fun = statistic, aes(color = nivel_pob), size = s*2, alpha = a_dot) +
      options_time + theme +
      scale_color_manual(values = rep(palette, 40)) +
      scale_x_continuous(breaks = years_break)
  }
  
  if(type == "promedio_dpto" & style == "lines"){
    graph <- ggplot(data = df, aes(x = time, y = value, group = nivel_label)) +
      geom_line(stat = "summary", fun = statistic, aes(color = nivel_label), size = s, alpha = a_line) +
      geom_point(stat = "summary", fun = statistic, aes(color = nivel_label), size = s*2, alpha = a_line) +
      options_time + theme + 
      scale_color_manual(values = rep(palette, 40)) +
      scale_x_continuous(breaks = years_break)
  }
  
  # C. Mapas ----
  if(style == "map"){
    graph <- ggplot(data = df) +
      geom_sf(aes(fill = q_value), color = "#2b2b2b", size = 0.1, alpha = a_dot) + 
      geom_text(aes(X, Y, label = nivel_label), vjust = 1.5, color = "black", 
                position = position_dodge(0.9), size = text/7) +
      scale_fill_manual(values =  rep(palette, 40), na.value = "#ededed") +
      guides(fill = guide_legend(ncol = 6)) +
      options_map + theme_map
  }
  
  # Grafica
  plot(graph)
  return(graph)
  
  # Guardar grafica
  # var <- unique(na.omit(df$variable))
  # period <- glue("{min(df$time)}-{max(df$time)}")
  # if(style != "map"){
  #   ggsave(glue("{path}/output/grafica_{var}_{type}_{style}_{period}.jpeg"), height = h, width = w, dpi = d)
  # } else {
  #   ggsave(glue("{path}/output/mapa_{var}_{type}_{style}_{unique(na.omit(df$time))}.jpeg"), height = h, width = w*0.8, dpi = d)
  # }
}

#-------------------------------------------------------#
# 1. Opciones ----
#-------------------------------------------------------#

# Tamanos graficas (ancho, alto, texto, resolucion)
w <- 4.5*2.5
h <- 3.2*2.5
text <- 14
d <- 900
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

# Opciones tema
legend_pos <- "none"
label_ang.x <- 45
label_h.x <- 1
label_v.x <- 1
label_ang.y <- 0
label_h.y <- 1
label_v.y <- 1
legend_title <- element_blank()
plot_title <- element_blank()
theme_style <- theme_classic(base_size = text*1.5)
text_color <- "black"

# Tema modificable
custom_theme <- list(
  theme_style,
  theme(legend.position = legend_pos, legend.title = legend_title,
        plot.title = plot_title,
        axis.title.x = element_text(colour = text_color),
        axis.title.y = element_text(colour = text_color),
        axis.text.x = element_text(angle = label_ang.x, vjust = label_h.x, hjust = label_v.x, colour = text_color),
        axis.text.y = element_text(angle = label_ang.y, vjust = label_h.y, hjust = label_v.y, colour = text_color))
)

# Colores
# Fuentes: 
# 1. https://publications.iadb.org/publications/spanish/document/Pulso-social-en-Am%C3%A9rica-Latina-y-el-Caribe-2017-Legado-familiar-%C2%BFrompemos-el-molde-o-repetimos-patrones.pdf
# 2. https://publications.iadb.org/publications/spanish/document/Pulso-social-de-Am%C3%A9rica-Latina-y-el-Caribe-2016-Realidades-y-perspectivas.pdf
# 3. https://mycolor.space/

bar_colors1 <- c("#3d5574", "#74ac54", "#b8544c", "#c0ac84", "#c8dccc", "#b0c4e4", "#d4742c")
lines_colors1 <- c("#3d5574", "#74ac54")
countries_colors1 <- c("#759cd4", "#c43424", "#ecbc24", "#385676", "#d4742d","#6dad55", "#a4a4a4", "#643d94")
bar_colors2 <- c("#4c9ccc", "#e4546c", "#fcac24", "#0c0c0c", "#b4b4b4")
lines_colors2 <- c("#4c9ccc", "#e4546c", "#fcac24", "#0c0c0c")
map_cont1 <- c("#C5DCFF", "#A1B8DC", "#7E96B8", "#5D7495", "#3D5574")
map_cont2 <- c("#74AC54", "#3AA367", "#00977B", "#00898B", "#007A94", "#006993")

# Cortes de anios (etiquetas eje X)
years_break <- c(2001, 2005, 2010, 2015, 2020)

#-------------------------------------------------------#
# 2. Graficas ----
#-------------------------------------------------------#

#--------------------------#
# A. General ----
#--------------------------#

# # Abrimos datos de ejemplo
# geih <- readxl::read_xlsx("Data/Ejemplo_base_datos/Output/geih_datos_ejemplo.xlsx", sheet = "departamental") %>%
#   mutate(value_label = "Tasa de desempleo (%)")
# 
# # Preparar datos
# data <- fun_data(geih)
# 
# # Barras
# grafica1 <- fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
#                 style = "bar", g_theme = custom_theme,
#                 path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# grafica2 <- fun_graph(df = data, type = "promedio_dpto", palette = countries_colors1, statistic = "mean", 
#           style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# # Ejemplo de graficas multiples
# Rmisc::multiplot(grafica1, grafica2, cols = 2)
# 
# # Lineas
# grafica <- fun_graph(df = data, type = "promedio_anual", palette = lines_colors2, statistic = "mean",
#           style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# grafica <- fun_graph(df = data, type = "promedio_dpto", palette = countries_colors1, statistic = "mean", 
#           style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# # Mapas
# data_map <- fun_map(df = data, time_map = 2019, classes = 5)
# fun_graph(df = data_map, palette = map_cont1, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# #--------------------------#
# # B. Genero ----
# #--------------------------#
# 
# geih_gen <- readxl::read_xlsx("Data/Ejemplo_base_datos/Output/geih_datos_ejemplo.xlsx", sheet = "departamental_genero") %>%
#   mutate(value_label = "Tasa de desempleo (%)")
# data <- fun_data(geih_gen)
# 
# # Barras y lineas por genero
# grafica <- fun_graph(df = data, type = "promedio_anual_gen", palette = bar_colors1, statistic = "mean", 
#           style = "bar", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# grafica <- fun_graph(df = data, type = "promedio_anual_gen", palette = bar_colors1, statistic = "mean", 
#           style = "lines", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# # Ejemplo para promediar variable de categorias poblacionales
# data_map <- fun_map(df = data, time_map = 2019, classes = 5, statistic = "mean")
# fun_graph(df = data_map, palette = map_cont1, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")
# 
# # Ejemplo para elegir categoria de desagregacion poblacional
# data_map <- fun_map(df = data, time_map = 2019, classes = 5, pob_cat = "Mujer")
# fun_graph(df = data_map, palette = map_cont1, style = "map", path = "Descriptives/Ejemplo_tasa_desempleo")


# # Abrir mapa municipios
# map_mpios <- st_read(glue("Data/Mapas/Output/mapa_municipios_colombia.shp")) %>% 
#   rename(nivel_label = nvl_lbl, id_nivel = id_nivl, nivel_value = nivl_vl) %>% 
#   dplyr::select(id_nivel, nivel_label, nivel_value)s



