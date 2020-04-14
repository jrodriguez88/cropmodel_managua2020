# Script Practice 3 - Visualizacion de simulaciones en R
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/cropmodel_managua2020
# 2020

### Objetivo: 
### Generar ambientes de cultivo en modelos de cultivo mediante la creación de archivos de clima y suelo.


### Load packages
library(tidyverse)
library(data.table)
library(lubridate)

source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/read_outputs_aquacrop.R", encoding = "UTF-8")

### 2 Definir directorio de trabajo y resultados, y zona de estudio
directorio <- paste0(getwd(), "/practica_3/") 
directorio_resultados <- paste0(directorio, "/data/")

# season files
season_files <- list.files(directorio_resultados, pattern = "season")

# daily files
daily_files <- list.files(directorio_resultados, pattern = "day")


### Leer y graficar resultados de AquaCrop plug-in
####read_aquacrop_season(file, path)
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, directorio_resultados)) %>%
  bind_rows() %>% 
  mutate(File = str_replace(File, ".PRM", "")) 
###
daily_data <- map(.x = daily_files, ~read_aquacrop_day(.x, directorio_resultados)) %>%
  set_names(daily_files) %>%
  bind_rows(.id = "File") %>% 
  mutate(File = str_replace(File, "PRMday.OUT", "")) 
###
##### Season plots
###
season_data %>% 
  dplyr::select(Year1, Yield, BioMass, Cycle, Rain, File) %>% 
  gather("var", "value", -c(File)) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 10, color="grey") + 
  facet_wrap(var ~., scales = "free") + 
  theme_classic()
###
season_data %>% mutate(date = make_date(Year1, Month1, Day1)) %>%
  dplyr::select(File, Yield, BioMass, Cycle, ETo) %>% 
  gather("var", "value", -c(File)) %>% 
  ggplot(aes(File, value)) +
  geom_boxplot(aes(fill=File)) + 
  facet_wrap(var ~., scales = "free") + 
  theme_classic()

###
season_data %>% mutate(date = make_date(Year1, Month1, Day1)) %>%
  select(date, Yield, BioMass, Cycle, Rain, File) %>% 
  gather("var", "value", -c(date, File)) %>% 
  ggplot(aes(year(date), value, color = File)) + geom_line() +
  #    stat_summary(aes(color = File), fun.data = mean_sdl, position=position_jitter()) + 
  facet_wrap(var ~ ., scales = "free") + 
  theme_bw()
###
season_data %>% mutate(date = make_date(Year1, Month1, Day1)) %>%
  dplyr::select(date, File, Yield, BioMass, Tr,ExpStr, StoStr,TempStr) %>% 
  gather("var", "value", -c(date, File)) %>% 
  ggplot(aes(File, value)) +
  geom_boxplot(aes(fill = File)) + 
  facet_wrap(var ~ ., scales = "free") + 
  theme_classic()




