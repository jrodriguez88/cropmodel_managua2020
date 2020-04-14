# Script Practice 2 - Parametrizacion y Calibracion de AquaCrop
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/cropmodel_lima2019
# 2019


### Load packages
library(tidyverse)
library(data.table)
library(lubridate)

### 2 Definir directorio de trabajo y resultados, y zona de estudio
directorio <- paste0(getwd(), "/practica_3/") 
directorio_resultados <- paste0(directorio, "/data/")
crop_file <- list.files(directorio_resultados, pattern = ".CRO", full.names = T)
crop_duration <- 120         # duracion promedio segun criterio del investigador
sowing_month <- c(12, 1,2,3,4)


#### Leer datos de clima

data <- read_csv(paste0(directorio_resultados, "datos_clima.csv"))

## function to calculate HUH (growing thermal units) _ tbase, topt,and thigh depends of crop
HUH_cal <- function(tmax, tmin, tbase = 8, topt = 30, thigh = 42.5) {
  
  tav <- (tmin + tmax)/2
  
  h <- 1:24
  
  Td <- tav + (tmax - tmin)*cos(0.2618*(h - 14))/2 
  
  huh <- Td %>% enframe(name = NULL, "td") %>%
    mutate(HUH = case_when(td <= tbase | td >= thigh ~ 0,
                           td > tbase | td <= topt ~ (td - tbase)/24,
                           td > topt | td < thigh ~ (topt-(td - topt)*(topt - tbase)/(thigh - topt))/24))
  
  sum(huh$HUH)   
  
} 

## Function to calculate GDD
get_param_gdd <- function(data , cropfile, sowing_dates) {
  
  gdd_data <- data %>% 
    mutate(GDD = pmap(list(tmax=.$tmax, tmin = .$tmin), HUH_cal) %>% 
             flatten_dbl())
  
  
  sow_dates <- gdd_data %>% dplyr::filter(month(date) %in% sowing_dates)
  
  #summary(sow_dates)
  
  gdd_by <- median(sow_dates$GDD)
  
  read_lines(cropfile) %>% str_subset("Calendar Days|(days)") %>% .[-c(1,2,10)] %>%
    str_split_fixed(":", 2) %>% str_trim() %>% matrix(ncol = 2) %>% 
    as_tibble() %>%
    mutate(V = as.numeric(V1)*gdd_by) %>% setNames(c("CYCLE", "grow_param", "GDD")) %>%
    select(grow_param, CYCLE, GDD)
  
  
  
}


get_param_gdd(data, crop_file, sowing_dates)




