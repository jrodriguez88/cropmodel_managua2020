# Script Practice 1 -  Creacion de Ambientes de cultivo
# Source of Weather data: NASA Prediction Of Worldwide Energy Resources https://power.larc.nasa.gov/
# Source of soil data: https://www.soilgrids.org/  
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/cropmodel_managua2020
# 2020

### Objetivo: 
### Generar ambientes de cultivo en modelos de cultivo mediante la creación de archivos de clima y suelo.


#################################################
### 1 Cargar Paquetes
#################################################

library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
library(naniar)
library(jsonlite)
library(sirad)
library(soiltexture)

## 1.1 Algunas funciones en desarrollo
source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_nasapower.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_soilgrids.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_soil_aquacrop.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")


#################################################
### 2 Definir directorios de trabajo y localidad
#################################################

#Periodo (Año-mes-dia yyyymmdd)
fecha_inicial <- 19880101
fecha_final <- 20191231

#Localidad
localidad <- "jalapa"
latitud <- 13.9
longitud <- -86.0
altitud <- 677

#Directorios de trabajo
directorio <- paste0(getwd(), "/practica_1/")
directorio_resultados <- paste0(directorio, "/data/")
dir.create(directorio_resultados)

#https://power.larc.nasa.gov/docs/v1/
#Precipitacion, Radiacion Solar, Humedad Relativa, Temp. Max, Temp. Min, Velocidad viento.
variables_clima <- c("PRECTOT", "ALLSKY_SFC_SW_DWN","RH2M", "T2M_MAX", "T2M_MIN","WS2M")

#https://www.isric.org/explore/soilgrids/faq-soilgrids
#Densidad Aparente, %Arcillas, %Arenas, %Grava, Carbono Organico, (Contenido agua a marchitez, Capacidad de campo, Saturacion) 
variables_suelo <- c("BLDFIE","CLYPPT","SNDPPT","CRFVOL","ORCDRC","WWP","AWCh1","AWCtS")
profundidades <- c("sl1", "sl2", "sl3", "sl4", "sl5")  # 60cm

  
#################################################
### 3. Descargar datos de clima y suelo
#################################################

datos_clima_crudos <- get_data_nasapower(variables_clima, fecha_inicial, fecha_final, latitud, longitud)
datos_suelo_crudos <- get_data_soilgrids(variables_suelo, latitud, longitud, profundidades)

#################################################
### 4. Explorar y Organizar data
#################################################

#Analisis exploratorio con skimr
skim(datos_clima_crudos)
skim(datos_suelo_crudos)

#Cambiar identificador NA
datos_clima_crudos <- datos_clima_crudos %>% replace_with_na_all(condition = ~.x == -99)

#Cambiemos esos  nombres extraños
names(datos_clima_crudos)
names(datos_clima_crudos) <- c("date", "srad", "rain", "rhum", "tmax", "tmin", "wvel")
skim(datos_clima_crudos)


#organizando datos finales para exportar a AquaCrop
datos_clima <- datos_clima_crudos %>%
  mutate(extraT = extrat(lubridate::yday(date), radians(latitud))$ExtraTerrestrialSolarRadiationDaily,
         srad = if_else(is.na(srad), 0.175*sqrt(tmax - tmin)*extraT, srad)) %>%
  dplyr::select(-extraT) %>% basic_qc_nasa
  

datos_suelo <- from_soilgrids_to_aquacrop(localidad, datos_suelo_crudos)

#Graficar clima
datos_clima %>%  
  group_by(year = year(date), month = month(date)) %>%
  summarise(rain = sum(rain), 
            tmin = mean(tmin), 
            tmax = mean(tmax), 
            srad = mean(srad), 
            rhum = mean(rhum),
            wvel = mean(wvel)) %>% #write.csv("climate_data_monthly.csv")
  ungroup() %>% gather(var, value, -c(year, month)) %>%
  ggplot(aes(factor(month), value)) + 
  geom_boxplot() + 
  facet_wrap(~var, scales = "free") + 
  labs(x = "mes", title = paste("promedios mesuales de", localidad)) +
  theme_bw()

#Graficar datos de suelo



#################################################
### 5. Convertir a formato Aquacrop
#################################################

make_weather_aquacrop(directorio_resultados, localidad, datos_clima, latitud, altitud)
make_soil_aquacrop(directorio_resultados, localidad, datos_suelo$data, datos_suelo$CN, datos_suelo$REW)


#################################################
### 6. Guardar datos clima y suelo en formato *.csv
#################################################

write_csv(datos_clima, paste0(directorio_resultados, localidad, "_clima.csv"))
write_csv(datos_suelo$data, paste0(directorio_resultados, localidad, "_suelo.csv"))





