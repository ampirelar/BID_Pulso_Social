#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# --- ANALYZE NEW JOB OFFERS  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Project:            LABOR DISCRIMINATION
#   Modification date:  Feb 12 2021
#   This script graphs timeseries from elempleo and computrabajo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())
packageList<-c("tidyverse","plyr","ggplot2","ggpubr","RColorBrewer","scales","readxl")
suppressPackageStartupMessages(lapply(packageList,require,character.only=TRUE))

# path <- "~/Dropbox/Labor Discrimination_EAFIT/02_Elempleo_covid/"
# path_graphs_overleaf <- "~/Dropbox/Applications/Overleaf/retos_mercado_laboral_covid_19/graphs/"
# setwd(path)

path <- "C:/Users/anapi/OneDrive - Universidad EAFIT/Investigacion/Labor_discrimination/"
path_data <- paste0(path,"data/raw/")
path_new_data <- paste0(path,"data/cleaned/")
path_graphs <- paste0(path,"graphs/")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# --- LOAD DATA AND COLORS  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fecha del ultimo sabado antes de descargar los datos: ejm, se descargaron dom 2 aug, se pone sab 1 aug
computrabajo <- readRDS(paste0(path_new_data,"computrabajo.rds")) %>%
  filter(date <= as.Date("2021-02-06"))

elempleo <- readRDS(paste0(path_new_data,"elempleo.rds"))  %>%
  filter(year != 2012) %>%
  filter(date <= as.Date("2021-02-26"))

# fechas covid
fechas_covid <- read_excel(paste0(path_data,"fechas_covid.xlsx"), sheet = "colombia") %>%
  mutate(start = as.Date(start), end = as.Date(end),fecha_anuncio = as.Date(fecha_anuncio),
         etapa = as.character(etapa), anuncios = as.character(anuncios)) %>%
  mutate(etapa = reorder(etapa, start))

# replace max quarantine date with last date of 2020, and remove 2021 dates that
# overlap 2020 dates
max_date <- as.Date("2020-12-31")
fechas_covid$end[fechas_covid$start == as.Date("2020-12-01") & fechas_covid$end == as.Date("2021-01-16")] <- max_date

fechas_covid <- fechas_covid %>% 
  dplyr::filter((start != as.Date("2021-01-14") & fecha_anuncio != as.Date("2021-01-14")) | 
                  (start != as.Date("2021-01-16") & end != as.Date("2021-03-01")))

# daily dates
fechas_covid_d <- fechas_covid

#Colors
uno <- rgb(2,7,48,max=255)
tres <- rgb(2,7,48,max=255)
cinco <- rgb(61,110,131, max = 255)
seis  <-rgb(61,110,131, max = 255)
siete <- rgb(255,254,217, max = 255)
ocho <- "#95a964"
nueve <- "#95a964"
doce <- "#2b5829"

# Define the number of colors you want
nb.cols <- 12
greys <- colorRampPalette(brewer.pal(8, "Greys"))(nb.cols)

# Graph style
source(paste0(path,"scripts/style_graphs.R"))

#~~~~~~~~~~~~~~~~~~~#
# --- TIMELINES ----
#~~~~~~~~~~~~~~~~~~~# 

graph_time <- function(x){
  
  # INPUTS ---
  engine <- lista[x, 1]
  var_filtro <- lista[x, 2]
  filtro <- lista[x, 3]
  nom_variable <- lista[x, 4]
  graph_title <- lista[x, 5]
  date_format <- lista[x, 6]
  
  dataset <- elempleo 
  
  if(engine == "computrabajo"){
    dataset <- computrabajo  
  }
  
  # Filter if we want to count only for instance health sector or teletrabajo
  if(filtro != ""){
    dataset <- dataset %>% filter(eval(as.name(var_filtro)) == filtro)
  }
  
  # STATISTICS ----
  
  # Number of posts 2019-2020
  statistics_2019_2020 <- dataset %>% 
    dplyr::filter(year >= 2020 | year == 2019) %>%
    mutate(week_number = as.numeric(week_number), dow2 = weekdays(date)) %>%
    dplyr::group_by(day_of_year, year) %>%
    dplyr::summarise(n_post = n(), date = min(date), dow = first(dow2))
  
  # All posible dates 2019-2020 (to take 0s into account)
  fechas <- data.frame(date = min(dataset$date)) %>%
    tidyr::complete(date = seq.Date(min(elempleo$date), max(elempleo$date), by = "day")) %>%
    filter(date >= as.Date("2019-01-01")) %>%
    mutate(year = date_format(format = "%Y")(date),
           day_of_year = date_format(format = "%j")(date)) %>%
    select(day_of_year, year)
  
  # Making sure all days are taken into account and trick 2019 as 2020 date
  # Left join all dates with vacancies count
  statistics_2019_2020 <- left_join(fechas, statistics_2019_2020) %>%
    mutate(n_post = ifelse(is.na(n_post), 0, n_post))
  
  statistics_2019_2020$origin <- as.Date(paste0("2020", "-01-01")) - lubridate::days(1)
  statistics_2019_2020$day2 <- as.numeric(statistics_2019_2020$day_of_year)
  statistics_2019_2020$date2 <- as.Date(statistics_2019_2020$day2, origin = statistics_2019_2020$origin) 
  
  statistics_2019_2020 <- statistics_2019_2020 %>%
    select(-c("origin", "day2", "date")) %>%
    dplyr::rename(date = date2) %>% 
    ungroup() %>%
    filter(!is.na(date)) 
  
  # making sure dates analyzed are the same (maximum date 2019 limited by 2020's max)
  statistics_2019_2020 <- statistics_2019_2020 %>%
    filter(date <= max(statistics_2019_2020[statistics_2019_2020$year == 2020, ]$date))
  
  # If week, then collapse
  if(nom_variable == "week_number"){
    
    statistics_2019_2020 <- statistics_2019_2020 %>%
      mutate(week_number = date_format(format = "%U")(date)) %>%
      group_by(year, week_number) %>%
      dplyr::summarise(n_post = sum(n_post), date = min(date, na.rm = T)) %>%
      ungroup() 
    
    # Make dates weekly
    fechas_covid <- fechas_covid %>%
      mutate(week_start = format(start, "%U"), week_end = format(end, "%U"),
             week_anuncio = format(fecha_anuncio,"%U")) %>%
      select(-c(start,end, fecha_anuncio)) %>%
      left_join(., statistics_2019_2020 %>% select(week_number, date) %>%
                  dplyr::rename(week_start = week_number)) %>% 
      dplyr::rename(start = date) %>%
      left_join(., statistics_2019_2020 %>% select(week_number, date) %>%
                  dplyr::rename(week_end = week_number)) %>% 
      dplyr::rename(end = date) %>% 
      unique() %>%
      left_join(., statistics_2019_2020 %>% select(week_number, date) %>%
                  dplyr::rename(week_anuncio = week_number)) %>% 
      dplyr::rename(fecha_anuncio = date) %>% 
      unique()
    
  }
  
  # GRAPHS ----
  
  gg_2020 <- ggplot(statistics_2019_2020 %>% filter(year == 2020)) +
    theme_blog() + 
    labs(title = "", y = "", x = "") + 
    geom_rect(aes(xmin = start, xmax = end, fill = etapa), 
              ymin = -Inf, ymax = Inf, alpha = 0.3, 
              data = fechas_covid %>% filter(!is.na(etapa))) + 
    geom_vline(aes(xintercept = as.numeric(start)),
               data = fechas_covid %>% filter(!is.na(etapa)),
               colour = "grey50", alpha = 0.5)  + 
    geom_vline(aes(xintercept = as.numeric(fecha_anuncio), linetype = 'Anuncio etapa'), 
               data = fechas_covid %>% filter(!is.na(anuncios)),
               colour = "blue", alpha = 0.5) +
    geom_line(aes(x = date, y = n_post, group = year, color = year), size = 2) +
    scale_fill_manual("Etapa", values = greys) +
    scale_color_manual("Año",values = c(tres, nueve)) + 
    scale_linetype('')
  # 
  # if(nom_variable == "week_number"){
  #   
  #   date_min <- statistics_2019_2020 %>% dplyr::filter(year == 2020 & n_post > 0)
  #   date_min <- min(date_min$date, na.rm = TRUE)
  #   gg_2020 <- gg_2020 + coord_cartesian(xlim = c(date_min, max_date))
  #   
  # }
  
  # Save
  ggexport(gg_2020, filename = paste0(path_graphs,engine,"_timeline_2020_",var_filtro,nom_variable,".png"),res = 100, width = 2700, height = 1400)
  
  # Only Elempleo datasets counts with 2019 data, for comparison
  if(engine == "elempleo"){
    gg_2019_2020 <- ggplot(statistics_2019_2020) +
      theme_blog() + 
      labs(title = "", y = "", x = "") + 
      geom_rect(aes(xmin = start, xmax = end, fill = etapa), 
                ymin = -Inf, ymax = Inf, alpha = 0.3, 
                data = fechas_covid %>% filter(!is.na(etapa))) + 
      geom_vline(aes(xintercept = as.numeric(start)), 
                 data = fechas_covid %>% filter(!is.na(etapa)),
                 colour = "grey50", alpha = 0.5)  + 
      geom_vline(aes(xintercept = as.numeric(fecha_anuncio), linetype = 'Anuncio etapa'), 
                 data = fechas_covid %>% filter(!is.na(anuncios)),
                 colour = "blue", alpha = 0.5) +
      geom_line(aes(x = date, y = n_post, group = year, color = year), size = 2) +
      scale_fill_manual("Etapa", values = greys) +
      scale_color_manual("Año",values = c(tres, nueve, doce)) + 
      scale_linetype('')
    
    # Save
    ggexport(gg_2019_2020, filename = paste0(path_graphs,engine,"_timeline_2019_2020_",var_filtro,nom_variable,".png"),res = 100, width = 2700, height = 1400)
    
  }
}

lista <- rbind(c("elempleo", "", "", "day_of_year","Ofertas laborales diarias Elempleo","%j"),
               c("elempleo", "job_sector", "salud", "day_of_year","Ofertas laborales diarias sector salud Elempleo","%j"),
               c("elempleo", "teletrabajo", 1, "day_of_year","Ofertas laborales diarias teletrabajo Elempleo","%j"),
               c("elempleo", "location", "bogota", "day_of_year","Ofertas laborales diarias Bogotá Elempleo","%j"),
               c("elempleo", "job_sector", "construccion y obra", "day_of_year","Ofertas laborales diarias sector construccion Elempleo","%j"),
               c("elempleo", "", "", "week_number","Ofertas laborales semanales Elempleo","%Y-%U-%u"),
               c("elempleo", "job_sector", "salud", "week_number","Ofertas laborales semanales sector salud Elempleo","%Y-%U-%u"),
               c("elempleo", "teletrabajo", 1, "week_number","Ofertas laborales semanales teletrabajo Elempleo","%Y-%U-%u"),
               c("elempleo", "location", "bogota", "week_number","Ofertas laborales semanales Bogotá Elempleo","%Y-%U-%u"),
               c("computrabajo", "", "", "day_of_year","Ofertas laborales diarias Computrabajo","%j"),
               c("computrabajo", "location", "bogota", "day_of_year","Ofertas laborales diarias Bogotá Computrabajo","%j"),
               c("computrabajo", "", "", "week_number","Ofertas laborales semanales Bogotá Computrabajo","%j"))

lapply(1:dim(lista)[1], graph_time)

#~~~~~~~~~~~~~~~~~~~~~~~#
# --- GRAPH BY GROUP ----
#~~~~~~~~~~~~~~~~~~~~~~~#

graph_group <- function(x){
  
  # Inputs
  engine <- lista_group[x, 1]
  nom_variable <- lista_group[x, 2]
  graph_title <- lista_group[x, 3]
  
  dataset <- elempleo
  
  if(engine == "computrabajo"){
    dataset <- computrabajo  
  }
  
  # highest ten groups 
  top_ten <- dataset %>% 
    filter(year == 2020) %>%
    filter(eval(as.name(nom_variable)) != "colombia") %>%
    group_by(eval(as.name(nom_variable))) %>%
    dplyr::summarise(total_posts=n()) %>%
    `colnames<-`(c(nom_variable,"total_posts")) %>%
    arrange(-total_posts) %>% 
    slice(1:5)
  
  # Total posts by var and date
  statistics <- dataset %>% 
    filter(year == 2020) %>%
    group_by(date, eval(as.name(nom_variable))) %>%
    dplyr::summarise(n_post = n()) %>%
    `colnames<-`(c("date", nom_variable,"n_post")) %>%
    filter(!is.na(eval(as.name(nom_variable)))) 
  
  # Keep only top 10 categories
  statistics <- merge(statistics, top_ten, all = F)
  
  # Graph by group
  gg_group <- ggplot(statistics, aes(x = date, y = n_post, group = eval(as.name(nom_variable)), color = eval(as.name(nom_variable)))) +
    geom_line(size = 1) + 
    theme_blog() + 
    labs(title = graph_title, y = "", x = "") +
    ggplot2::annotate("rect", xmin = as.Date("2020-03-06"),
             xmax = max_date, ymin = -Inf, ymax = Inf, alpha = .5) +
    ggplot2::annotate(geom = "text", x = as.Date("2020-03-06"), y = max(statistics$n_post) - sd(statistics$n_post), 
             label = "1er reporte Covid-19 \n en Colombia") +
    scale_color_manual(values = c(uno, cinco, ocho, doce,"gray")) +
    theme(legend.title = element_blank(),
          text = element_text(size = 18))
  
  # if(engine == "computrabajo"){
  #   
  #   date_min <- min(statistics$date, na.rm = TRUE)
  #   gg_group <- gg_group + coord_cartesian(xlim = c(date_min, max_date))
  #   
  # }
  
  #Save
  ggexport(gg_group, filename = paste0(path_graphs,engine,"_timeline_by_",nom_variable,".png"), res = 100, width = 2700, height = 1400)
}

lista_group <- rbind(c("elempleo", "contract", "Número ofertas laborales según tipo de contrato - Elempleo"),
                     c("elempleo", "wage", "Número ofertas laborales según salario - Elempleo"),
                     c("elempleo", "location", "Número ofertas laborales según municipio - Elempleo"),
                     c("elempleo", "wage_groupped", "Número ofertas laborales según salario (agrupado) - Elempleo"),
                     c("elempleo", "company_sector_grouped", "Número ofertas laborales según sectores económicos (agrupado) - Elempleo"),
                     c("elempleo", "job_sector", "Número ofertas laborales según sectores económicos (agrupado) - Elempleo"),
                     c("elempleo", "company_sector", "Número ofertas laborales según sectores económicos (agrupado) - Elempleo"),
                     c("computrabajo", "working_day", "Número ofertas laborales según día laboral - Computrabajo "))

lapply(1:dim(lista_group)[1], graph_group)

