
##############################################
# Clase de visualización de datos con ggplot2
##############################################

#' El símbolo de michi o numeral te permite comentar una linea, es decir
#' todo lo que esta a la derecha de # r no lo lee

#' El simbolo pipe %>% se puede generar con la combinacion de teclas shift+ctrl+M


# Tipo de datos -----

## Numeric
edad <- 15
class(edad)

## character
nombre <- 'Juan'
class(nombre)

# Estructuras de datos ----

## vector
nombres <- c('Juan','Pedro','Luis')
notas <- c(20, 18, 15)

# Librerias -----

## 1ero se instala (una sola vez)
install.packages('lubridate')
install.packages('aweek')
install.packages('tidyverse')

## 2do se carga (cada vez que lo usamos en un script)
library(lubridate)
library(aweek)
library(tidyverse)

# Manipulacion de datos -----

  ## Lectura de la base de datos ----
#' Data descargada directamente de la web (base muy pesada)
#base_covid <- read.csv2('https://cloud.minsa.gob.pe/s/AC2adyLkHCKjmfm/download', encoding = 'UTF-8')
#' Data reducida predescargada (base no pesada)
base_covid <- read.csv2('data/positivos_covid.csv', encoding = "UTF-8")

#' observamos el resumen del contenido de la base 
str(base_covid)

  ## Limpieza de la base de datos -----

base_covid_clean <- base_covid %>% 
  select(-id_persona  , -FECHA_CORTE) %>% 
  filter(EDAD >= 0 & EDAD <= 110)

  ## P1) Cálculo de metricas de la edad para Lima ----

df_p1 <- base_covid_clean %>% 
  filter(DEPARTAMENTO == 'LIMA') %>% 
    summarise(mean(EDAD), max(EDAD), min(EDAD), sd(EDAD))
  
  
  ## P1) Cálculo de metricas de la edad para cada departamento ----

df_p2 <- base_covid_clean %>% 
  group_by(DEPARTAMENTO) %>% 
  summarise(EDAD_MEDIA = mean(EDAD), 
            EDAD_MAX = max(EDAD),
            EDAD_MIN = min(EDAD),
            EDAD_SD = sd(EDAD)) %>% 
  ungroup()

  ## P3) Número de casos por día ----

df_p3 <- base_covid_clean %>% 
  count(FECHA_RESULTADO) %>% 
  mutate(FECHA_RESULTADO = as.character(FECHA_RESULTADO),
         FECHA_RESULTADO = as.Date(FECHA_RESULTADO, format= '%Y%m%d'))
  
  
  
  
  
  


