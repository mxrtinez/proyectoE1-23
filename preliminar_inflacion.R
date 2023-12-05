library("readxl") # para leer archivos Excel
library("ggplot2") # para graficar
library("tidyverse") # para manejo de datos
library(dplyr) # para hacer joins
library(tidyr) # para reshape la tabla
library(RColorBrewer) # colorcitos de los plots
#install.packages("patchwork") # para poder combinar grafucis
library(patchwork)

# obteniendo la informacion de promedios de productos por cada año
folder <- "D:/UIS SEXTO SEMESTRE/ESTADISTICA/proyecto/"
nombre_archivo <- "datosRawprecios_all.xlsx"

file = paste(folder, '/', nombre_archivo, sep="")
sheets <- excel_sheets(file)
datos <- readxl::read_xlsx(file, sheet = sheets[1])
datos

# obtener los datos de inflacion
folder <- "D:/UIS SEXTO SEMESTRE/ESTADISTICA/proyecto/"
nombre_archivo <- "datosRaw/1.1.INF_Serie historica Meta de inflacion IQY.xlsx"
file = paste(folder, '/', nombre_archivo, sep="")
sheets <- excel_sheets(file)
datos_inflacion <- readxl::read_xlsx(file, skip=7,sheet = sheets[1])
names(datos_inflacion) <- c("Fecha", "inflacionTotal", "limiteSuperior", "MetadeInflacion","limiteInferior")

datos_inflacion <- datos_inflacion %>%
  mutate(Fecha = as.Date(sub("(\\d{4})(\\d{2})\\..*", "\\1-\\2-01", Fecha))) %>%
  select(-limiteSuperior) %>%
  select(-MetadeInflacion) %>%
  select(-limiteInferior)

datos_inflacion <-  datos_inflacion %>%
  filter(year(Fecha) >= 2013)

datos_inflacion

#Ver la tendencia global de los precios año a año, un poco ver la inflación
# Para hacer esto
#1. Escoger un año de referencia
#2. Identificar una canasta de productos que esten en todos los años.
#3. Filtrar los datos según esa canasta, obteniendo los precios de cada producto cada año. 
#Esto se usa para calcular el costo de la canasta de cada año. 
#4. Calcular el CPI para cada año (es una formulita)
#5. Calcular la taza de inflación (es una formulita con el CPI que se calculó en el paso anterior)

#año de referencia:
years <-  c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
year_referencia = "2013"


p1 <- datos %>%
  filter(Producto == "Aguacate papelillo") %>%
  filter(year(Fecha) == 2023) %>%
  ggplot(aes(x=month(Fecha), y=Precio, group=Ciudad, color=Ciudad))+
  geom_point() +
  geom_line() +
  labs(title = "Precio aguacate a nivel nacional 2023")

print(p1)

# COMPARANDO PRECIO DE AGUACATE
papelillo <- datos %>%
  filter(Producto == "Aguacate papelillo") %>%
  filter(Ciudad == "Bogotá") %>%
  ggplot(aes(x=month(Fecha), y=Precio, group=year(Fecha), color=year(Fecha)))+
  geom_point() +
  geom_line() +
  labs(title = "Precio aguacate común Bogota 2013 -2023")

  
hass <- datos %>%
  filter(Producto == "Aguacate Hass") %>%
  filter(Ciudad == "Bogotá") %>%
  ggplot(aes(x=month(Fecha), y=Precio, group=year(Fecha), color=year(Fecha)))+
  geom_point() +
  geom_line() +
  labs(title = "Precio aguacate Hass Bogota 2013 -2023")


combined <- papelillo + hass
print(combined)

# filtrar los datos segun esos productos
producto1 <- datos %>%
  filter(Producto == "Chocolate instantáneo")


producto2 <- datos %>%
  filter(Producto == "Harina precocida de maíz")

producto1df <- calcular_tasa_inflacion(producto1)

producto2df <- calcular_tasa_inflacion(producto2)


# Create the first plot
h <- ggplot() + 
  geom_line(data = datos_inflacion, aes(x = Fecha, y = inflacionTotal, color ='Inflacion nacional')) + 
  geom_line(data = producto1df, aes(x = Fecha, y = Inflacion, color = 'Inflacion harina producto1')) +
  labs(title = "Tasa inflación producto 1") +
  scale_color_manual(name = "Graph", values = c('Inflacion nacional' = 'black', 'Inflacion harina producto1'='blue'))
print(h)


p <- ggplot() + 
  geom_line(data = datos_inflacion, aes(x = Fecha, y = inflacionTotal, color ='Inflacion nacional')) + 
  geom_line(data = producto2df, aes(x = Fecha, y = Inflacion, color = 'Inflacion harina producto2')) +
  labs(title = "Tasa inflación producto 2") +
  scale_color_manual(name = "Graph", values = c('Inflacion nacional' = 'black', 'Inflacion harina producto2' = 'blue'))

print(p)

combined2 <- h + p
print(combined2)





# Calcular tasa de inflacion de unos datos en especifico
calcular_tasa_inflacion <- function(d){
  # Create a preallocated numeric vector
  fechas <- unique(d$Fecha)
  n = length(unique(d$Fecha))
  promedios <- numeric(n)
  # calcular promedios por mes
  for (i in 1:n) {
    a <- d %>%
      filter(Fecha == fechas[i])
      promedios[i] <- mean(a$Precio)
  }
  
  base <- promedios[1] # enero de 2013
  ipc_list <- promedios * (100/base)
  tasa_inflacion <- c(n)
  tasa_inflacion[1] = 0
  for(i in 2:n){
    tasa_inflacion[i] = (ipc_list[i] - ipc_list[i-1]) * (100/ipc_list[i-1])
  }
  
  promediosdf <- data.frame(Fecha = fechas,
                            Inflacion = tasa_inflacion,
                            Promedio= promedios,
                            IPC = ipc_list)

  promediosdf$Fecha = as.Date(promediosdf$Fecha)
  return(promediosdf)
}
