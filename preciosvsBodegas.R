library("readxl") # para leer archivos Excel
library("ggplot2") # para graficar
library("tidyverse") # para manejo de datos
library(dplyr) # para hacer joins
library(tidyr) # para reshape la tabla
library(RColorBrewer) # colorcitos de los plots
#install.packages("patchwork") # para poder combinar grafucis
library(patchwork)
library(skimr)

# Leer datos (convertidos de xlsx por hojas)
datos <- read.csv("datosRaw/newData.csv", header = TRUE)

# Convertir fecha de char a date
datos$Fecha <- as.Date(datos$Date)


##########################################################################
###################     Datos de productos      ##########################
##########################################################################

# Mirar inicio y final de la tabla
head(datos) # para ver mas valores: head(aux, 10)
tail(datos)
#View(datos)
names(datos)
datos$Date <- as.Date(datos$Date, format = "%Y-%m-%d")
length(datos$Date)
unique(datos$Grupo)
unique(datos$Ciudad)

# Check for "missing values"
is.na(datos)
sum(is.na(datos))

# Summary Statistics
summary(datos)
summary(datos$Precio) # Para una columna específica

# Summary (SKIM) Statistics
skim(datos)

# Group data by and do skim
datos %>% 
  dplyr::group_by(Grupo) %>%
  skim()

# Using tidyverse
glimpse(datos)
dim(datos)

##########################################################################
###################     SOME PLOTS     ##########################
##########################################################################

# -----------------------------------------
# Histogramming non-numerical data
tabledata <- table(datos$Grupo)
barplot(tabledata, horiz = FALSE, las = 2)
#pie(tabledata)

# HIstograma Ciudad
tabledata2 <- table(datos$Ciudad)
barplot(tabledata2, horiz = FALSE, las = 2)
#pie(tabledata2, labels = paste0(round(tabledata2/sum(tabledata2)*100, 2), "%"))

# Histogramming Numerical data
hist(datos$Precio)

##########################################################################
###################     Datos de productos      ##########################
##########################################################################

abastecimiento <- readxl::read_xlsx("./datosRaw/Series-historicas-abastecimiento-2013-2023.xlsx", sheet = 3, range = "A10:AL139")

# Mirar inicio y final de la tabla
head(abastecimiento) # para ver mas valores: head(aux, 10)
tail(abastecimiento)
#View(datos)
names(abastecimiento)
class(abastecimiento$Fecha)
abastecimiento$Fecha <- as.Date(abastecimiento$Fecha, format = "%d/%m/%Y")
length(abastecimiento$Fecha)

# Check for "missing values"
is.na(abastecimiento)
sum(is.na(abastecimiento))
#abastecimiento <- replace(abastecimiento, is.na(abastecimiento), 0)

# Summary Statistics
summary(abastecimiento)
summary(abastecimiento$'TOTAL ABASTECIMIENTO') # Para una columna específica

# Summary (SKIM) Statistics
skim(abastecimiento)
skim(abastecimiento$`Bucaramanga, Centroabastos`)

# Using tidyverse
glimpse(skim(abastecimiento))
dim(skim(abastecimiento))

##########################################################################
###################     SOME PLOTS     ##########################
##########################################################################

# Histogramming abastecimiento total pais
hist(abastecimiento$`TOTAL ABASTECIMIENTO`, main = "Histograma de abastecimiento total")

# Abastecimiento 
plot(abastecimiento$Fecha, abastecimiento$`TOTAL ABASTECIMIENTO`, type = "l", main = "Historico de abastecimiento total", xlab = "Fecha", ylab = "Toneladas")

# Abastecimiento suavizado
ggplot(abastecimiento, aes(x = Fecha, y = `TOTAL ABASTECIMIENTO`)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title = "Historico de abastecimiento total (Suavizado)", x = "Fecha", y = "Total")

# Abastecimiento Bucaramanga

plot(abastecimiento$Fecha, abastecimiento$`Bucaramanga, Centroabastos`, type = "l", main = "Historico de abastecimiento [Bucaramanga, Centro abastos]", xlab = "Fecha", ylab = "Toneladas")

ggplot(abastecimiento, aes(x = Fecha, y = `Bucaramanga, Centroabastos`)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title = "Historico de abastecimiento [Bucaramanga, Centro abastos] (Suavizado)", x = "Fecha", y = "Total")

# Seleccionar un producto
producto <- filter(datos, Ciudad == "Bucaramanga" & Mercado == "Bucaramanga, Centroabastos" & Producto == "Lulo")

plot(producto$Fecha, producto$Precio, type = "l", main = "Precio historico del producto", xlab = "Fecha", ylab = "Pesos")

ggplot(producto, aes(x = Fecha, y = Precio)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title = "Precio historico del producto (Suavizado)", x = "Fecha", y = "Total")

# Normalizar los valores para graficar 
productoNorm <- producto %>%
  mutate(precio_normalizado = scale(Precio))
abastecimientoNorm <- abastecimiento %>%
  mutate(total_normalizado = scale(`Bucaramanga, Centroabastos`))

ggplot(abastecimientoNorm, aes(x = Fecha, y = total_normalizado)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  geom_smooth(data = productoNorm, aes(x = Fecha, y = precio_normalizado), color = "red") +
  labs(title = "Abastecimiento [Azul] vs Precio de Producto (Suavizado) [Rojo]", x = "Fecha", y = "Y")

