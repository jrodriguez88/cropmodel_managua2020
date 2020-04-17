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
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_project_by_date.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/read_outputs_aquacrop.R", encoding = "UTF-8")


### 2 Definir directorio de trabajo y resultados, y zona de estudio
localidad <- "jalapa"
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
#dir.create(directorio_resultados)
#function_to_save(localidad, data_resampling, directorio_resultados)


## Plot Seasonal forecast escenaries
plot_resampling(data_resampling, data_historic, localidad)


## Convert to Aquacrop
to_aquacrop <- data_resampling$data[[1]] %>%
  mutate(data = map(data, ~.x %>% 
                      rename(rain = prec) %>% 
                      mutate(year = 2020, 
                             date=make_date(year, month, day))%>%
                      select(-c(year, month, day))))
  
#dir.create(aquacrop_files)
map2(paste0(localidad, to_aquacrop$id), to_aquacrop$data, 
     ~make_weather_aquacrop(aquacrop_files, .x, .y, latitud, altitud))


star_sow <- c(4,1)   #c(month, day)
end_sow <- c(5,3)  
unlink(paste0(plugin_path, "/OUTP/*"))
#unlink(paste0(plugin_path, "/LIST/*"))

to_aquacrop %>% sample_n(.,  size = 25) %>%
  mutate(clim_data = map(data, ~.x %>% 
                      mutate(HUH = map2_dbl(tmax, tmin, HUH_cal)))) %>%
  mutate(sowing_dates = map(clim_data, ~sow_date_cal(star_sow, end_sow, .x, by = 3)),
         crop = "INTAL9",
         id2 = "id2", 
         id_name = paste0(localidad, id)) %>% select(-data, -id) %>%
  #data_to_project %>%# slice(1:3) %>% +
  mutate(to_project = pmap(list(x = id_name, y = sowing_dates, z = clim_data, k = crop, m = id2, n = plugin_path),
                           function(x,y,z,k,m, n) list(id_name = x, 
                                                       sowing_dates = y, 
                                                       clim_data = z,
                                                       cultivar = k,
                                                       plugin_path = n,
                                                       id2 = m))) %>% pull(to_project) %>%
  walk(~make_project_by_date(.x$id_name, .x$sowing_dates, .x$cultivar, 130, .x$clim_data, aquacrop_files, .x$plugin_path, .x$id2))
  
system("practica_4/plugin/ACsaV60.exe")



### Lectura de datos 
path_op <- paste0(plugin_path, "/OUTP/")
season_files <- list.files(path_op, pattern = "season") 

filename_var <- c("Escenario", "cultivar", "id_s", "crop_sys")
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, path_op)) %>%
  bind_rows() %>% 
  mutate(File = str_replace(File, ".PRM", "")) %>%
  separate(File, filename_var, sep = "_")


### plot resultados
season_data %>% 
  mutate(date = make_date(Year1, Month1, Day1)) %>%
  ggplot(aes(factor(date), Yield, fill = crop_sys)) +
#  geom_col() + 
#  stat_summary(fun.y = mean, geom = "bar") +
#  stat_summary(fun.data = mean_sdl, geom = "errorbar") +
  geom_boxplot() + 
#  facet_grid(crop_sys ~.)
  theme_bw() +
  labs(x= "Fecha",
       y= "Rendimiento (kg/ha)",
       title = "Simulacion Agroclimatica")



  
