# Script app - Pronósticos agro-climáticos estacionales.
# Author: Rodriguez-Espinoza J. / Esquivel A.
# Repository: https://github.com/jrodriguez88/COF_2020
# 2020

### Objetivo: 
### Generar pronosticos de rendimiento a partir de una prediccion climatica probabilistica.


### 1. Cargar requerimientos

source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/agroclim_forecaster.R", encoding = "UTF-8")
load_agroclim_requeriments()
inpack(c("tidyverse", "data.table", "lubridate", "sirad", "naniar", "jsonlite" ,"soiltexture"))
crear_directorios_COF()


### 2. Definir zona de estudio
localidad <- "SitioA"
latitud <- 13.9
altitud <- 677

### 3. Leer datos de entrada
datos_historicos <- read_csv(paste0(directorio_datos, "/datos_clima.csv"))

pronostico <- read_csv(paste0(directorio_datos, "/pronostico_probabilistico.csv"))


### 4. Grafique los datos observados y las probabilidades

plot_prob_forecast(pronostico)
plot_weather_series(datos_historicos, localidad)

### 5. Realice el remuestreo estadistico sobre la serie historica
data_resampling <- resampling(datos_historicos, pronostico, 2020)


### Opcional : Guarde los escenarios
#dir.create(directorio_resultados)
#function_to_save(localidad, data_resampling, directorio_resultados)


### 6. Grafique los escenarios de remuestreo
plot_resampling(data_resampling, datos_historicos, localidad, stat = "median")


### 7. Configurar datos para formatos AquaCrop

cultivar <- list.files(aquacrop_files, pattern = ".CRO") %>% str_remove(".CRO")
suelos <- list.files(aquacrop_files, pattern = ".SOL")
star_sow <- c(min(data_resampling$data[[1]]$data[[1]]$month),1)   #c(month, day)
end_sow <- c((star_sow[1]+1),3)

to_aquacrop1 <- map(cultivar, ~from_resampling_to_aquacrop(data_resampling, .x, "FrancoArcilloso")) %>% bind_rows()
to_aquacrop2 <- map(cultivar, ~from_resampling_to_aquacrop(data_resampling, .x, "FrancoArenoso")) %>% bind_rows()
to_aquacrop <- bind_rows(to_aquacrop1, to_aquacrop2)



### 8. Exportar datos a formato AquaCrop\

unlink(paste0(plugin_path, "/OUTP/*"))
unlink(paste0(plugin_path, "/LIST/*"))


walk2(paste0(localidad, to_aquacrop$id), to_aquacrop$data,
      ~make_weather_aquacrop(aquacrop_files, .x, .y, latitud, altitud))

to_aquacrop %>% pull(to_project) %>% 
  walk(~make_project_by_date(.x$id_name, .x$sowing_dates, .x$cultivar, 130, .x$clim_data, aquacrop_files, .x$plugin_path, .x$id2))


### 9. Ejecutar las simulaciones de AquaCrop
system("agroclim_COF/plugin/ACsaV60.exe")



### 10. Lectura de resultados
path_op <- paste0(plugin_path, "/OUTP/")
season_files <- list.files(path_op, pattern = "season") 

file_str <- c("clima", "cultivar", "soil", "crop_sys")
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, path_op)) %>%
  bind_rows() 


### 11. Graficar resultados finales
plot_agroclim_forecast(season_data, localidad, file_str, "t/ha")
plot_agroclim_hidric(season_data, localidad, file_str)


## Felicitaciones, ha terminado su primer pronostico agroclimatico
