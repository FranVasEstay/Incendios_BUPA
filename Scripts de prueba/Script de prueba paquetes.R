#______________________________________________________________________________#
#____________________SCRIPT DUMMY PARA PROBAR LAS FUNCIONES ___________________#
#______________________________________________________________________________#

#____________ Paquetes de R para el Proyecto de Análisis de Incendios__________#

##Paquetes Clasificados por Función
required_packages <-c(
### 1. Extracción y Limpieza de Datos
"readxl",
"dplyr",
"tidyverse",

### 2. Análisis Exploratorio (EDA",
"skimr",  # skim(datos", → Resumen estadístico
"DataExplorer",  # plot_missing(datos",
"psych",  # describe(datos", → Estadísticas robustas

###  3. Series de tiempo
"tsibble",  # as_tsibble(", -> Estructurar datos temporales
"fable",  # ARIMA(",,ETS(", -> Modelos predictivos
"CausalImpact",  # CausalImpact(", -> Análisis pre/post incendio
"tsibble",

### 4. Análisis espacial
"sf",       # st_read(", → Cargar shapefiles
"leaflet",  # addTiles(", + addMarkers(", → Mapas interactivos
"ggmap",    # get_stamenmap(", → Mapas estáticos con ggplot2

### 5. Modelado estadístico
"mgcv", #`gam(consultas ~ s(pm2.5", + te(temperatura, humedad",",` → Modelos no lineales
"lme4", #`glmer(consultas ~ incendio + (1|comuna",",` → Efectos aleatorios
"broom", #`tidy(modelo",` → Resultados en formato tabla
"mgcv",
###  6. Visualización
"ggplot2",  # Gráficos base
"plotly",   # Gráficos interactivos ggplotly(",
"patchwork", # Combinar gráficos: p1 + p2

###  7. Reproducibilidad
"here", #`here("data", "raw.csv")` → Rutas portables
"renv", #`renv::init()` → Gestión de dependencias
"targets", #Flujo automatizado de análisis

### 8. Documentación
"rmarkdown",  # Informes dinámicos
"bookdown",   # Informes técnicos largos
"kableExtra" # Tablas formateadas (HTML/PDF)
)

### Función para instalar solo los faltantes
instalar_faltantes <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

### Instalar solo los que faltan
invisible(sapply(required_packages, instalar_faltantes))

### Instalación especial para CausalImpact si no está
if (!requireNamespace("CausalImpact", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("google/CausalImpact")
}

### Cargar todos los paquetes
invisible(lapply(required_packages, library, character.only = TRUE))
library(CausalImpact)  # Cargar por separado por si fue instalado desde GitHub

#______________________________________________________________________________#

renv::init() #Inicia el proyecto

## Gestion de datos
### Data sintética para practicar
library(tidyverse)
set.seed(123)

datos_simulados <- tibble(
  fecha = seq.Date(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
  pm25 = round(20 + 10 * sin(seq(0, 2*pi, length.out = 365) + rnorm(365, sd = 5))),  # PM2.5 con estacionalidad
  consultas = round(50 + pm25 * 0.8 + rnorm(365, sd = 10)),  # Relación PM-consultas
  sexo = sample(c("F", "M"), 365, replace = TRUE),
  temperatura = round(15 + 10 * sin(seq(0, 2*pi, length.out = 365)) + rnorm(365, sd = 3), 1),
  humedad = round(50 + 30 * sin(seq(0, 2*pi, length.out = 365)) + rnorm(365, sd = 5)),
  region = sample(c("5","13"),365,replace = T))%>% 
  left_join(coordenadas_regiones, by = "region") %>%  # Une las coordenadas
  mutate(
    # Añade variación espacial aleatoria (±0.5 grados)
    latitud = latitud + rnorm(n(), sd = 0.1),
    longitud = longitud + rnorm(n(), sd = 0.1)
  )
#Guardar datos simulados
save(datos_simulados, file = "Datos/datos_prueba.RData")

# Verificación
datos_simulados %>% 
  select(region, latitud, longitud) %>% 
  distinct()
                        
# Visualizar relación PM-consultas
 ggplot(datos_simulados, aes(pm25, consultas)) +
 geom_point() +
 geom_smooth(method = "lm")

##Analisis de tendencias temporales y espaciales
### Análisis de serie temporal
ts_data <- datos_simulados %>%
  as_tsibble(index = fecha, key = region)

ggplot(ts_data, aes(x = fecha, y = consultas, color = region)) +
  geom_line() +
  facet_wrap(~ region, ncol = 1) +
  theme_bw() #Aquí se ve un paralelo entre las dos regiones en cuanto a las consultas.

### Mapa interactivo de contaminación (no está funcionando)
leaflet(datos_simulados) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    color = ~colorNumeric("Reds", pm2.5)(pm2.5),
    popup = ~paste("PM2.5:", pm2.5)
  )

## Modelado de serie interrumpida de tiempo
### Serie interrumpida de tiempo (ITS)
# Supongamos que el incendio comenzó el 05 de febrero de 2024
fecha_incendio <- as.Date("2024-02-05")
datos_simulados <- datos_simulados %>%
  mutate(
    incendio = ifelse(fecha >= fecha_incendio, 1, 0),  # 1 = post-incendio
    tiempo = as.numeric(fecha - min(fecha)),  # Tiempo desde inicio (días)
    dow = weekdays(fecha)  # Día de la semana (factor)
  )
modelo_its <- glm(consultas ~ incendio + tiempo + incendio:tiempo +
                    temperatura + humedad + dow,
                  data = datos_simulados,
                  family = quasipoisson())
modelo_its

### Causal Impact (antes/después)
library(zoo)  # Para trabajar con series temporales

# Crear un objeto zoo (serie temporal con fechas)
ts_consultas <- zoo(datos_simulados$consultas, order.by = datos_simulados$fecha)
pre_period <- c(as.Date("2023-01-01"), as.Date("2023-02-15"))
post_period <- c(as.Date("2023-02-16"), as.Date("2023-12-31"))

impact <- CausalImpact(ts_consultas, pre_period, post_period)
plot(impact)

## Interacción modelo humo
### Modelo con interacción
modelo_interaccion <- gam(
  consultas ~ te(pm25, temperatura) + s(humedad),
  data = datos_simulados,  # Especificar el dataframe aquí
  family = poisson()
)

### Visualización de interacción
vis.gam(
  modelo_interaccion,
  view = c("pm25", "temperatura"),
  plot.type = "contour",
  main = "Interacción PM2.5 y Temperatura"
)

#Gráfico de contorno con ggplot2
pred_data <- expand.grid(
  pm25 = seq(min(datos_simulados$pm25), max(datos_simulados$pm25), length.out = 50),
  temperatura = seq(min(datos_simulados$temperatura), max(datos_simulados$temperatura), length.out = 50),
  humedad = mean(datos_simulados$humedad)  # Mantener constante
)

pred_data$pred <- predict(modelo_interaccion, newdata = pred_data, type = "response")

ggplot(pred_data, aes(pm25, temperatura, z = pred)) +
  geom_contour_filled(bins = 10) +
  labs(title = "Efecto interactivo: PM2.5 y Temperatura en consultas médicas",
       fill = "Consultas\nprevistas") +
  theme_minimal()

## Figuras
p1 <- ggplot(datos_simulados, aes(fecha, consultas)) + 
  geom_line()

p2 <- ggplot(datos_simulados, aes(pm25, consultas)) + 
  geom_point() + 
  geom_smooth()

library(gridExtra)
p_combined <- grid.arrange(p1, p2, ncol = 1)  # Apilados verticalmente
ggsave("Figuras/figura_combinada_prueba.png", p_combined, width = 10, height = 8, dpi = 300)

#________________________________GUARDAR_______________________________________#
# Exportar todos los objetos (la imagen del entorno de trabajo)
save.image(file = "Figuras/Objetos_R.RData")
