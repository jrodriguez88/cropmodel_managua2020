# Script Practice 4 - Pronósticos agro-climáticos estacionales.
# Author: Rodriguez-Espinoza J. / Esquivel A.
# Repository: https://github.com/jrodriguez88/cropmodel_managua2020
# 2020

### Objetivo: 
### Generar pronosticos de rendimiento a partir de una prediccion climatica probabilistica.


### Load packages
library(tidyverse)
library(data.table)
library(lubridate)
source("https://raw.githubusercontent.com/jrodriguez88/ciat_tools/master/remuestreo_mod.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")

### 2 Definir directorio de trabajo y resultados, y zona de estudio
localidad <- "jalapa1"
latitud <- 13.9
altitud <- 677

directorio <- paste0(getwd(), "/practica_4/") 
directorio_datos <- paste0(directorio, "/data/")
directorio_resultados <- paste0(directorio, "/resultados/")
aquacrop_files <- paste0(directorio, "/aquacrop_files/")
plugin_path <- paste0(directorio, "/plugin/")


## Read Data
data_historic <- read_csv(paste0(directorio_datos, "/datos_clima.csv")) %>% 
  mutate(day = day(date), 
         month = month(date),
         year = year(date)) %>%
  rename(prec = rain) %>%
  dplyr::select(day:year, prec, tmax, tmin)

pronostico <- read_csv(paste0(directorio_datos, "/pronostico_probabilistico.csv"))


## Explore observed data and seasonal probabilistic forecast

plot_prob(pronostico)
plot_clima_hist(data_historic, localidad)

## Make statistical resampling over historic data
data_resampling <- resampling(data_historic, pronostico, 2020)


## Save escenaries
dir.create(directorio_resultados)
function_to_save(localidad, data_resampling, directorio_resultados)


## Plot Seasonal forecast escenaries
plot_resampling(data_resampling, data_historic, localidad)




## Convert to Aquacrop

a <- data_resampling$data[[1]] %>% slice(1:3) %>%
  mutate(data = map(data, ~.x %>% 
                      rename(rain = prec) %>% 
                      mutate(date=make_date(year, month, day))%>%
                      select(-c(year, month, day))))
  


#dir.create(aquacrop_files)
map2(paste0(localidad, a$id), a$data, ~make_weather_aquacrop(aquacrop_files, .x, .y, latitud, altitud))

