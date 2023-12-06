library("readxl") # para leer archivos Excel
library("ggplot2") # para graficar
library("tidyverse") # para manejo de datos
library(dplyr) # para hacer joins
library(tidyr) # para reshape la tabla
library(RColorBrewer) # colorcitos de los plots
#install.packages("patchwork") # para poder combinar grafucis
library(patchwork)
library(skimr)

##########################################################################
###################     Datos de productos      ##########################
##########################################################################

# Leer datos (convertidos de xlsx por hojas)
datos <- read.csv("datosRaw/newData.csv", header = TRUE)

# Convertir fecha de char a date
datos$Date <- as.Date(datos$Date)
datos$Date <- as.Date(datos$Date, format = "%Y-%m/%d")

# Mirar inicio y final de la tabla
head(datos) # para ver mas valores: head(aux, 10)
names(datos)

##########################################################################
###################     Datos de insumos      ##########################
##########################################################################

insumos <- readxl::read_xlsx("./datosRaw/series-historicas-insumos-2021-2023.xlsx", sheet = 5, range = "A9:I61985")

# Mirar inicio y final de la tabla
head(insumos) # para ver mas valores: head(aux, 10)
tail(insumos)
#View(datos)
names(insumos)
class(insumos$Mes)

# Convertir Columna mes a número de mes
months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
            "Septiembre", "Octubre", "Noviembre", "Diciembre")
insumos$Mes <- match(insumos$Mes, months)

# Crear columan con fecha
insumos$Date <- paste(insumos$Año, insumos$Mes, '1', sep = "-")
class(insumos$Mes)
insumos$Date <- as.Date(insumos$Date, format = "%Y-%m-%d")
class(insumos$Mes)


# Check for "missing values"
is.na(insumos)
sum(is.na(insumos))

# Summary Statistics
summary(insumos)
summary(insumos$'Precio promedio') # Para una columna específica

# Summary (SKIM) Statistics
skim(insumos)
skim(insumos$'Precio promedio')

# Using tidyverse
glimpse(skim(insumos))
dim(skim(insumos))

##########################################################################
###################     SOME PLOTS     ##########################
##########################################################################
names(insumos)
unique(insumos$`Nombre del producto`)

# Usar Nitrogeno = "Nitromag 21-0-0-7" en santander
nitrogeno <- filter(insumos, `Nombre del producto` == "Nitromag 21-0-0-7" & `Nombre departamento` == "Santander")

hist(nitrogeno$`Precio promedio`, main = "Histograma Precio promedio")

# Historico 
plot(nitrogeno$Date, nitrogeno$`Precio promedio`, type = "l", main = "Historico de Nitrogeno en Socorro", xlab = "Fecha", ylab = "Precio")

# Historico suavizado
ggplot(nitrogeno, aes(x = Date, y = `Precio promedio`)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title = "Historico de Nitrogeno Scorro (Suavizado)", x = "Fecha", y = "Precio")

# Seleccionar un producto
producto <- filter(datos, Ciudad == "Bucaramanga" & Mercado == "Bucaramanga, Centroabastos" & Producto == "Lulo")

plot(producto$Date, producto$Precio, type = "l", main = "Precio historico del producto", xlab = "Fecha", ylab = "Pesos")

ggplot(producto, aes(x = Date, y = Precio)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title = "Precio historico del producto (Suavizado)", x = "Fecha", y = "Total")

# Normalizar los valores para graficar 
productoNorm <- producto %>%
  mutate(precio_normalizado = scale(Precio))
insumosNorm <- nitrogeno %>%
  mutate(total_normalizado = scale(`Precio promedio`))

ggplot(insumosNorm, aes(x = Date, y = total_normalizado)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  geom_smooth(data = productoNorm, aes(x = Date, y = precio_normalizado), color = "red") +
  labs(title = "Nitrogeno Socorro [Azul] vs Precio de Producto (Suavizado) [Rojo]", x = "Fecha", y = "Y")


