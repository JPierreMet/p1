
##############################################
# Clase de visualización de datos con ggplot2
##############################################

#' El símbolo de michi o numeral te permite comentar una linea, es decir
#' todo lo que esta a la derecha de # r no lo lee

# Librerias --------------------------------------------------------------------
library(tidyverse)  # Asi cargamos una libreria que ya esta instalada
library(janitor)

# Base de datos ----------------------------------------------------------------
data_fallecidos <- read.csv2('https://cloud.minsa.gob.pe/s/xJ2LQ3QyRW38Pe5/download')    # El argumento de encoding es para que reconozca las tildes y ñ

str(data_fallecidos)      # una manera de ver el contenido de tu tabla
summary(data_fallecidos)  # Una manera de resumir las variables de tu tabla

# Limpieza ---------------------------------------------------------------------
data_fallecidos_clean <- data_fallecidos %>%                    # En la primera linea
  clean_names() %>%                                             # homogenizamos los nombres de las variables
  select(-fecha_corte, -id_persona) %>%                         # quitamos algunas variables
  filter(edad_declarada >= 0 & edad_declarada <= 110,           # filtramos edad y sexo
         sexo == 'FEMENINO' | sexo == 'MASCULINO') %>% 
  mutate(fecha_fallecimiento = as.Date(as.character(fecha_fallecimiento), format = '%Y%m%d')) %>% # convertimos de formato numero a fecha
  rename(fecha = fecha_fallecimiento, edad = edad_declarada)    # renombramos algunas variables

# Paso 1 -----------------------------------------------------------------------

##' Gráfico de barras de fallecidos por fecha
## Modo básico

fallecidos_fecha <- data_fallecidos_clean %>% # Preparamos la data a usar
  count(fecha, name = 'fallecidos')

ggplot(data=fallecidos_fecha)+
  geom_col(mapping = aes(x= fecha, y= fallecidos))

# Paso 2 -----------------------------------------------------------------------

#' Gráfico de barras de fallecidos por fecha
## Modo básico + capas de complemento

fallecidos_fecha <- data_fallecidos_clean %>% 
  count(fecha, name = 'fallecidos')

ggplot(data=fallecidos_fecha)+
  geom_col(mapping = aes(x= fecha, y= fallecidos))+
  labs(x = 'Fecha (días)',
       y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por COVID-19',
       caption = 'Fuente: SUSALUD')+
  theme_bw()

# Paso 3 -----------------------------------------------------------------------

#' Gráfico de diversos tipos de fallecidos
## Modo básico lineas + capas de complemento

fallecidos_fecha <- data_fallecidos_clean %>% 
  count(fecha, name = 'fallecidos')

ggplot(data=fallecidos_fecha)+
  geom_line(mapping = aes(x= fecha, y=fallecidos))+
  labs(x = 'Fecha (día)',
       y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por COVID-19',
       caption = 'Fuente: SUSALUD')

## Modo básico puntos + capas de complemento
ggplot(data=fallecidos_fecha)+
  geom_point(mapping = aes(x= fecha, y=fallecidos))+
  geom_line(mapping = aes(x= fecha, y=fallecidos))+
  labs(x = 'Fecha (día)',
       y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por COVID-19',
       caption = 'Fuente: SUSALUD')

## Modo básico boxplot + capas de complemento
ggplot(data=data_fallecidos_clean)+
  geom_boxplot(mapping = aes(x= clasificacion_def, y=fecha))+
  labs(x = 'Criterio',
       y = 'Edad',
       title = 'Boxplot de edades por criterio de defunción',
       caption = 'Fuente: SUSALUD')+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# Paso 4 -----------------------------------------------------------------------

#' Grafico de defunciones por criterio y por sexo
falle_fecha_sexo <- data_fallecidos_clean %>% 
  count(fecha, sexo, name = 'fallecidos')

########## Barras (fill)

ggplot(data = falle_fecha_sexo) +
  geom_col(mapping = aes(x= fecha,
                         y = fallecidos,
                         fill = sexo))+
  labs(x = 'Criterio', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por sexo',
       caption = 'Fuente: SUSALUD',
       fill = 'Sexo')+
  theme_classic()

########## Lineas (color)

ggplot(data = falle_fecha_sexo) +
  geom_line(mapping = aes(x= fecha,
                         y = fallecidos,
                         color = sexo))+
  labs(x = 'Criterio', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por sexo',
       caption = 'Fuente: SUSALUD',
       color = 'Sexo')+
  theme_classic()

########## Lineas (size + color)

ggplot(data = falle_fecha_sexo) +
  geom_line(mapping = aes(x= fecha,
                          y = fallecidos,
                          color = sexo,
                          size = sexo))+
  labs(x = 'Criterio', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por sexo',
       caption = 'Fuente: SUSALUD')+
  theme_classic()

########## Point (shape)

ggplot(data = falle_fecha_sexo) +
  geom_point(mapping = aes(x= fecha,
                          y = fallecidos,
                          color = sexo,
                          shape = sexo))+
  labs(x = 'Criterio', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por sexo',
       caption = 'Fuente: SUSALUD',
       color = 'Sexo', shape = 'Sexo'
       )+
  theme_classic()

# Paso 5 -----------------------------------------------------------------------

falle_fecha_sexo <- data_fallecidos_clean %>% 
  count(fecha, sexo, name = 'fallecidos')

########## Barras (fill)

ggplot(data = falle_fecha_sexo) +
  geom_col(mapping = aes(x= fecha,
                         y = fallecidos,
                         fill = sexo))+
  labs(x = 'Criterio', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por sexo',
       caption = 'Fuente: SUSALUD',
       fill = 'Sexo')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  scale_x_date(date_breaks = '2 month', date_labels = '%y/%m')
  
########### Lineas (color)

ggplot(data = falle_fecha_sexo) +
  geom_line(mapping = aes(x= fecha,
                          y = fallecidos,
                          color = sexo,
                          size = sexo))+
  labs(x = 'Criterio', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por sexo',
       caption = 'Fuente: SUSALUD')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  scale_x_date(date_breaks = '2 month', date_labels = '%y/%m')+
  scale_size_manual(values = c(1, 1))+
  scale_color_manual(values = c('red','blue'))
  
#############

data_fallecidos_clean %>% 
  count(fecha, clasificacion_def, name = 'fallecidos') %>% 
  ggplot() +
  geom_line(mapping = aes(x= fecha,
                          y = fallecidos,
                          color = clasificacion_def,
                          size = clasificacion_def))+
  labs(x = 'Fecha', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por criterio de defuncion',
       caption = 'Fuente: SUSALUD',
       color = 'Criterio de defunción')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  scale_x_date(date_breaks = '2 month', date_labels = '%y/%m')+
  scale_size_manual(values = c(1, 1, 1, 1, 1, 1, 1.5))+
  scale_color_viridis_d()


data_fallecidos_clean %>% 
  count(fecha, clasificacion_def, name = 'fallecidos') %>% 
  ggplot() +
  geom_col(mapping = aes(x= fecha,
                          y = fallecidos,
                          fill = clasificacion_def))+
  labs(x = 'Fecha', y = 'N° de fallecidos',
       title = 'Tendencia de fallecidos por criterio de defuncion',
       caption = 'Fuente: SUSALUD',
       fill = 'Criterio de defunción')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  scale_x_date(date_breaks = '2 month', date_labels = '%y/%m')+
  scale_fill_viridis_d()

  
data_fallecidos_clean %>% 
  count(fecha, clasificacion_def, name = 'fallecidos') %>% 
  ggplot() +
  geom_tile(mapping = aes(x= fecha,
                         y = clasificacion_def,
                         fill = fallecidos))+
  labs(x = 'Fecha', y = 'Criterio de defuncion',
       title = 'Mapa de calor de fallecidos por criterio',
       caption = 'Fuente: SUSALUD')+
  theme_classic()+
  scale_x_date(date_breaks = '2 month', date_labels = '%y/%m')+
  scale_fill_viridis_c()

