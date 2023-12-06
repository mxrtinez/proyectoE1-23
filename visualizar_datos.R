library("readxl") # para leer archivos Excel
library("ggplot2") # para graficar
library("tidyverse") # para manejo de datos
library(dplyr) # para hacer joins
library(tidyr) # para reshape la tabla
library(RColorBrewer) # colorcitos de los plots
#install.packages("patchwork") # para poder combinar grafucis
library(patchwork) # para plotear multiples graficos en una figura
#install.packages("lubridate")
library(lubridate)
#install.packages("cowplot")
library(cowplot) # tambien para hacer multiples graficos pero diferente
library(skimr) # ver los summary mas bonito
#install.packages("kableExtra")
library(kableExtra) # mostrar tablas bonitas
library(knitr)
library(DT)           # for printing nice HTML output tables
#install.packages("DT")


# algunas constantes
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)
abreviaciones_meses <- c(
  "ene", "feb", "mar", "abr", "may", "jun",
  "jul", "ago", "sep", "oct", "nov", "dic"
)


##########################################################################
###################     Datos productos     ##########################
##########################################################################

nombre_archivo <- "datosRaw/precios_historico_productos_2013-2023_modificado.xlsx"
# no se porque
nombre_archivo <- "D:/UIS SEXTO SEMESTRE/ESTADISTICA/proyectoE1-23/datosRaw/precios_historico_productos_2013-2023_modificado.xlsx"

#file = paste(folder, '/', nombre_archivo, sep="")
file = nombre_archivo
sheets <- excel_sheets(file)
datos <- readxl::read_xlsx(file, sheet = sheets[1])

#se nos paso el grupo de tuberculos y lacteos tiene un nombre diferente para 2021 
datos <- datos %>%
  mutate(Grupo = ifelse(Grupo == "TUBÉRCULOS, RAÍCES Y PLÁTANOS" , "TUBERCULOS, RAICES Y PLATANOS", Grupo))

datos <- datos %>%
  mutate(Grupo = ifelse(Grupo == "LACTEOS Y HUEVOS" , "LÁCTEOS Y HUEVOS", Grupo))


unique(datos$Grupo)

sum(is.na(datos))

# quitando todos los datos NA
datos <- na.omit(datos)

# visualizacion de los daticos :3
skim(datos)


# Porcentaje de informacion por cada grupo
grouped_data <- datos %>%
  group_by(Grupo) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

#Diagrama de pie
pie(grouped_data$Count, labels = paste(grouped_data$Grupo, 
                                       round(grouped_data$Percentage, 2), "%"))

ggplot(grouped_data, aes(x="", y=Percentage, fill=Grupo)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(x = 1.6,label = paste(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5))



agricola <- grouped_data %>%
  filter(Grupo %in% grupos_select)

sum(agricola$Percentage)
# Porcentaje de informacion por año
grouped_data <- datos %>%
  group_by(year = year(Fecha)) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Diagrama de pie
pie(grouped_data$Count, labels = paste(grouped_data$year, '-',
                                       round(grouped_data$Percentage, 2), "%"))




grouped_data <- datos %>%
  group_by(year = year(Fecha)) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

grouped_data$year <- as.factor(grouped_data$year)

ggplot(grouped_data, aes(x="", y=Percentage, fill=year, group=year)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") +
  geom_text(aes(x = 1.6, label = paste(year, '\n', round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title="Porcentaje de información por año")
  #scale_fill_manual(values = colors)
       
       
# Ver ciudades
# Una tabla con los productos de cada grupo

unique_values_table <- datos %>%
  group_by(Ciudad) %>%
  summarize(Mercados = toString(unique(Mercado)))

# Imprimir la tabla
unique_values_table %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)



#install.packages("gtsummary")
library(gtsummary)

# Assuming your DataFrame is named 'df'
# Create a summary table
t <- datos %>% select(Fecha, Grupo, Ciudad, Precio)
table_summary <- tbl_summary(t)

# View the table
print(table_summary)


# Ver productos
# Una tabla con los productos de cada grupo
unique_values_table <- datos %>%
  group_by(Grupo) %>%
  summarize(Productos = toString(unique(Producto)))

# Imprimir la tabla
unique_values_table %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)


# Ver cantidad de productos reportados en el dataset cada mes
# Vemos que es bastante consistente en cuanto a la cantidad de productos
# que se registran al mes
productos_por_mes <- datos %>%
  group_by(Fecha) %>%
  summarize(ProductosUnicos = n_distinct(Producto))

ggplot(productos_por_mes, aes(x = Fecha, y = ProductosUnicos)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Unique Products per Month",
       x = "Month",
       y = "Number of Unique Products") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ahora veamos la cantidad de filas que hay por cada producto
# Para ver cuanta informacion tenemos de cada producto
filas_por_producto <- datos %>%
  group_by(Producto) %>%
  summarize(Filas = n())

ggplot(filas_por_producto, aes(x = Producto, y = Filas)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Unique Products per Month",
       x = "Month",
       y = "Number of Unique Products") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Veamos en cuantas ciudades sale cada producto
ciudades_por_producto <- datos %>%
  group_by(Producto) %>%
  summarize(Filas = n_distinct(Ciudad))

ggplot(ciudades_por_producto, aes(x = Producto, y = Filas)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Unique Products per Month",
       x = "Month",
       y = "Number of Unique Products") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# como podemos ver son demasiados datos!
# productos de agricultura
grupos_select <- c("TUBERCULOS, RAICES Y PLATANOS", "VERDURAS Y HORTALIZAS","FRUTAS", "GRANOS Y CEREALES"   )

productos_agricultura = unique(filter(datos, Grupo %in% grupos_select)$Producto)
print(length(productos_agricultura))

##########################################################################
###########    Seleccionamos los datos de region andina    ###############
##########################################################################
ciudades_andinas <- c("Bucaramanga", "Cúcuta", "Ibagué", "Manizales", "Medellín", 
                      "Santa Bárbara (Antioquia)", "Tunja", "Bogotá", "La Ceja (Antioquia)", 
                      "Popayán", "Rionegro (Antioquia)", "Armenia", "Duitama (Boyacá)", 
                      "Marinilla (Antioquia)", "Neiva", "Peñol (Antioquia)", "Pereira", 
                      "San Vicente (Antioquia)", "Sogamoso (Boyacá)", "Sonsón (Antioquia)", 
                      "Chiquinquirá (Boyacá)", "La Virginia (Risaralda)", "Palmira (Valle del Cauca)", 
                      "El Santuario (Antioquia)", "El Carmen de Viboral (Antioquia)", 
                      "La Unión (Antioquia)", "Tibasosa (Boyacá)")


# productos que se pueden producir en la region andina colombiana
productos_list <- c(
  "Aguacate común", "Aguacate Hass", "Aguacate papelillo", "Curuba",
  "Curuba redonda", "Feijoa", "Fresa", "Granadilla", "Guayaba agria","Guanábana",
  "Maracuyá", "Papaya Maradol","Piña gold","Mango común",
  "Guayaba común", "Guayaba manzana", "Guayaba pera", "Higo",
  "Lulo", "Mora de Castilla", "Pera nacional", "Tomate de árbol",
  "Arracacha amarilla", "Arracacha blanca", "Papa capira",
  "Papa criolla limpia", "Papa criolla sucia", "Papa ICA-Huila",
  "Papa nevada", "Papa parda pastusa", "Papa Puracé", "Papa rubí",
  "Papa R-12 negra", "Papa R-12 roja", "Papa sabanera", "Papa San Félix",
  "Papa suprema", "Papa tocana", "Papa tocarreña", "Papa única",
  "Plátano comino", "Plátano dominico hartón maduro",
  "Plátano dominico hartón verde", "Plátano dominico verde",
  "Plátano guineo", "Plátano hartón maduro", "Plátano hartón verde",
  "Plátano hartón verde llanero", "Plátano hartón verde venezolano",
  "Ulluco", "Yuca chirosa", "Yuca criolla", "Yuca ICA",
  "Yuca llanera", "Maíz amarillo cáscara", "Maíz amarillo trillado", "Maíz blanco trillado",
  "Fríjol nima calima", "Fríjol radical", "Fríjol Uribe rosado", "Fríjol cargamento rojo", "Fríjol cargamento blanco"
)
print(length(productos_list))

# Mapeo de ciudades andinas con su respectivo departamento (gracias chatgpt4)
ciudad_a_departamento <- c("Bucaramanga" = "Santander",
                           "Cúcuta" = "Norte de Santander",
                           "Ibagué" = "Tolima",
                           "Manizales" = "Caldas",
                           "Medellín" = "Antioquia",
                           "Santa Bárbara (Antioquia)" = "Antioquia",
                           "Tunja" = "Boyacá",
                           "Bogotá" = "Cundinamarca",
                           "La Ceja (Antioquia)" = "Antioquia",
                           "Popayán" = "Cauca",
                           "Rionegro (Antioquia)" = "Antioquia",
                           "Armenia" = "Quindío",
                           "Duitama (Boyacá)" = "Boyacá",
                           "Marinilla (Antioquia)" = "Antioquia",
                           "Neiva" = "Huila",
                           "Peñol (Antioquia)" = "Antioquia",
                           "Pereira" = "Risaralda",
                           "San Vicente (Antioquia)" = "Antioquia",
                           "Sogamoso (Boyacá)" = "Boyacá",
                           "Sonsón (Antioquia)" = "Antioquia",
                           "Chiquinquirá (Boyacá)" = "Boyacá",
                           "La Virginia (Risaralda)" = "Risaralda",
                           "Palmira (Valle del Cauca)" = "Valle del Cauca",
                           "El Santuario (Antioquia)" = "Antioquia",
                           "El Carmen de Viboral (Antioquia)" = "Antioquia",
                           "La Unión (Antioquia)" = "Antioquia",
                           "Tibasosa (Boyacá)" = "Boyacá")


unique_values_table <- datos_agro %>%
  group_by(Departamento) %>%
  summarize(Ciudades = toString(unique(Ciudad)))

# Imprimir la tabla
unique_values_table %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)


# Filtrar datos agro
datos_agro <- datos %>%
  filter(Ciudad %in% ciudades_andinas & Producto %in% productos_list)

# agregando la columna de departamento
datos_agro <- datos_agro %>% 
  mutate(Departamento = ciudad_a_departamento[Ciudad])

grouped_data <- datos_agro %>%
  group_by(Departamento) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Pie Chart using Base R
pie(grouped_data$Count, labels = paste(grouped_data$Departamento, 
                                       round(grouped_data$Percentage, 2), "%"))




skim(datos_agro)

# visualizacion de datos agro
# No todos los productos que seleccionamos aparecen todos los meses!
productos_por_mes <- datos_agro %>%
  group_by(Fecha) %>%
  summarize(ProductosUnicos = n_distinct(Producto))

ggplot(productos_por_mes, aes(x = Fecha, y = ProductosUnicos)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Unique Products per Month",
       x = "Month",
       y = "Number of Unique Products") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(productos_por_mes, aes(x = ProductosUnicos, fill = Fecha)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "white") +
  labs(title = "Distribution of Unique Products per Month",
       x = "Number of Unique Products",
       y = "Frequency") +
  theme_minimal()

# Veamos los productos por ciudades
# algunas ciudades reportan tan solo algunos
ciudades_por_producto <- datos_agro %>%
  group_by(Ciudad) %>%
  summarize(Filas = n_distinct(Producto))

ggplot(ciudades_por_producto, aes(x = Ciudad, y = Filas)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Unique Products per Month",
       x = "Month",
       y = "Number of Unique Products") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(paste("Cantidad de productos: ",length(productos_list)))

print(paste("Cantidad de mercados ", length(unique(datos_agro$Mercado))))




##########################################################################
###################     Datos inflación     ##############################
##########################################################################


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


##########################################################################
###################     Datos de insumos      ##########################
##########################################################################

insumos <- readxl::read_xlsx(paste(folder, "datosRaw/series-historicas-insumos-2021-2023.xlsx", sep = ""), sheet = 5, range = "A9:I61985")

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
insumos$Fecha <- paste(insumos$Año, insumos$Mes, '1', sep = "-")
class(insumos$Mes)
insumos$Fecha <- as.Date(insumos$Fecha, format = "%Y-%m-%d")
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

unique(insumos$`Nombre del producto`)
unique(insumos$`Nombre departamento`)
unique(insumos$`Nombre municipio`)

departamentos_andinos <- c("Antioquia", "Boyacá", "Cundinamarca", "Norte de Santander", "Santander", 
                           "Tolima", "Huila", "Caldas", "Quindío", "Risaralda")

nrow(insumos)
# cambiando nombres de columnas a algo mas amigable
insumos$Departamento <- insumos$`Nombre departamento`
insumos <- insumos[, !names(insumos) %in% 'Nombre departamento']

insumos$Insumo <- insumos$`Nombre del producto`
insumos <- insumos[, !names(insumos) %in% 'Nombre del producto']


insumos_andino <- insumos %>%
  filter(Departamento %in% departamentos_andinos)


nrow(insumos_andino)
##########################################################################
###################     Selección y filtrado de datos     ################
##########################################################################
# Seleccionar los datos con los que vamos a trabajar
# Como son muchos datos nos enfocaremos en una region del pais
ciudades_andinas <- c("Bucaramanga", "Cúcuta", "Ibagué", "Manizales", "Medellín", 
                      "Santa Bárbara (Antioquia)", "Tunja", "Bogotá", "La Ceja (Antioquia)", 
                      "Popayán", "Rionegro (Antioquia)", "Armenia", "Duitama (Boyacá)", 
                      "Marinilla (Antioquia)", "Neiva", "Peñol (Antioquia)", "Pereira", 
                      "San Vicente (Antioquia)", "Sogamoso (Boyacá)", "Sonsón (Antioquia)", 
                      "Chiquinquirá (Boyacá)", "La Virginia (Risaralda)", "Palmira (Valle del Cauca)", 
                      "El Santuario (Antioquia)", "El Carmen de Viboral (Antioquia)", 
                      "La Unión (Antioquia)", "Tibasosa (Boyacá)")

# Mapeo de ciudades andinas con su respectivo departamento (gracias chatgpt4)
ciudad_a_departamento <- c("Bucaramanga" = "Santander",
                           "Cúcuta" = "Norte de Santander",
                           "Ibagué" = "Tolima",
                           "Manizales" = "Caldas",
                           "Medellín" = "Antioquia",
                           "Santa Bárbara (Antioquia)" = "Antioquia",
                           "Tunja" = "Boyacá",
                           "Bogotá" = "Cundinamarca",
                           "La Ceja (Antioquia)" = "Antioquia",
                           "Popayán" = "Cauca",
                           "Rionegro (Antioquia)" = "Antioquia",
                           "Armenia" = "Quindío",
                           "Duitama (Boyacá)" = "Boyacá",
                           "Marinilla (Antioquia)" = "Antioquia",
                           "Neiva" = "Huila",
                           "Peñol (Antioquia)" = "Antioquia",
                           "Pereira" = "Risaralda",
                           "San Vicente (Antioquia)" = "Antioquia",
                           "Sogamoso (Boyacá)" = "Boyacá",
                           "Sonsón (Antioquia)" = "Antioquia",
                           "Chiquinquirá (Boyacá)" = "Boyacá",
                           "La Virginia (Risaralda)" = "Risaralda",
                           "Palmira (Valle del Cauca)" = "Valle del Cauca",
                           "El Santuario (Antioquia)" = "Antioquia",
                           "El Carmen de Viboral (Antioquia)" = "Antioquia",
                           "La Unión (Antioquia)" = "Antioquia",
                           "Tibasosa (Boyacá)" = "Boyacá")

grupos_select <- c("TUBERCULOS, RAICES Y PLATANOS", "TUBÉRCULOS, RAÍCES Y PLÁTANOS", "VERDURAS Y HORTALIZAS","FRUTAS", "GRANOS Y CEREALES"   )

# productos que se pueden producir en la region andina colombiana
productos_list <- c(
  "Aguacate común", "Aguacate Hass", "Aguacate papelillo", "Curuba",
  "Curuba redonda", "Feijoa", "Fresa", "Granadilla", "Guayaba agria","Guanábana",
  "Maracuyá", "Papaya Maradol","Piña gold","Mango común",
  "Guayaba común", "Guayaba manzana", "Guayaba pera", "Higo",
  "Lulo", "Mora de Castilla", "Pera nacional", "Tomate de árbol",
  "Arracacha amarilla", "Arracacha blanca", "Papa capira",
  "Papa criolla limpia", "Papa criolla sucia", "Papa ICA-Huila",
  "Papa nevada", "Papa parda pastusa", "Papa Puracé", "Papa rubí",
  "Papa R-12 negra", "Papa R-12 roja", "Papa sabanera", "Papa San Félix",
  "Papa suprema", "Papa tocana", "Papa tocarreña", "Papa única",
  "Plátano comino", "Plátano dominico hartón maduro",
  "Plátano dominico hartón verde", "Plátano dominico verde",
  "Plátano guineo", "Plátano hartón maduro", "Plátano hartón verde",
  "Plátano hartón verde llanero", "Plátano hartón verde venezolano",
  "Ulluco", "Yuca chirosa", "Yuca criolla", "Yuca ICA",
  "Yuca llanera", "Maíz amarillo cáscara", "Maíz amarillo trillado", "Maíz blanco trillado",
  "Fríjol nima calima", "Fríjol radical", "Fríjol Uribe rosado", "Fríjol cargamento rojo", "Fríjol cargamento blanco"
)

# Quitar todo lo que sea importado
datos <- datos %>%
  filter(!grepl("importada|importado", Producto, ignore.case = TRUE))

d <- datos %>%
  filter(Grupo %in% c("TUBERCULOS, RAICES Y PLATANOS", "TUBÉRCULOS, RAÍCES Y PLÁTANOS", "VERDURAS Y HORTALIZAS") & Ciudad %in% ciudades_andinas )

unique(d$Producto)

# Filtrar datos agro
datos_agro <- datos %>%
  filter(Grupo %in% grupos_select & Ciudad %in% ciudades_andinas & Producto %in% productos_list)

datos_agro <- datos_agro %>% 
  mutate(Departamento = ciudad_a_departamento[Ciudad])

head(datos_agro)

unique(datos_agro$Producto)

summary(datos)
library(DT)           # for printing nice HTML output tables
#install.packages("DT")


datos2023 <- filter(datos, year(Fecha) == 2023)
datos2023[0:100,]

datatable(datos2023[0:1000,], caption = 'Table 1: Clean and tidy data.')


kable(head(datos), format = "markdown")

s = summary(datos)
unique_counts <- sapply(datos, function(col) length(unique(col)))
unique_counts

kable(s, format = "markdown")
kable(unique_counts, format = "markdown")




