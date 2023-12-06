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

# algunas constantes
folder <- "D:/UIS SEXTO SEMESTRE/ESTADISTICA/proyecto/"
abreviaciones_meses <- c(
  "ene", "feb", "mar", "abr", "may", "jun",
  "jul", "ago", "sep", "oct", "nov", "dic"
)

##########################################################################
###################     Datos productos     ##########################
##########################################################################

nombre_archivo <- "datosRawprecios_all.xlsx"

file = paste(folder, '/', nombre_archivo, sep="")
sheets <- excel_sheets(file)
datos <- readxl::read_xlsx(file, sheet = sheets[1])
datos

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

#TODO: falta juntar los datos de los insumos con los de 2013-2021

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


################################################################################################
###################     Busqueda de productos con mayor y menor variabilidad     ################
#################################################################################################

df <- data.frame()

# Normalizar daticos agro por producto 
for (i in unique(datos_agro$Producto)) {
  
  # Ver por año cuantas ciudades tienen ese producto en promedio
  numciudades <- promedioCiudades(i)
  
  # Promedio mensual del precio a nivel regional
  promedios_regional <- datos_agro %>%
    filter(Producto == i) %>%
    group_by(Fecha) %>%
    summarize(Promedio = mean(Precio))
  
  # Normalizar los precios para tener valores entre 0 y 1, mas faciles de comparar
    promedios_regional_normalizado <- promedios_regional %>%
      group_by(year = lubridate::year(Fecha)) %>%
      mutate(Promedio_normalizado = (Promedio / (max(Promedio, na.rm = TRUE))))
    
  # Calcular desviacion estandar de los precios normalizados
  r <- data.frame(
    Producto = i,
    Promedio_normalizado = mean(promedios_regional_normalizado$Promedio_normalizado, na.rm = TRUE),
    Sd = sd(promedios_regional_normalizado$Promedio_normalizado, na.rm = TRUE),
    Num_ciudades = numciudades
  )
  
  #Agregar el coeficiente de variacion
  r$CV = r$Sd / r$Promedio_normalizado
  # Agregar fila al dataframe de resultado
  df <- bind_rows(df, r)
}

# Hay varios productos que solo se registran en 3-5 ciudades
# Buscamos los que esten en por lo menos 10
df <- filter(df, Num_ciudades > 10 )


# Obtenemos n los datos que menos y mas varian
n = 5
menor_sd <- (arrange(df,Sd))
mayor_sd <- (arrange(df,desc(Sd)))
head(menor_sd, 5)
head(mayor_sd,5)

# TODO: hacer un diagrama de barras para representar esto mas bonito
# TODO: revisar que tal es el CV como medida ¿ es mejor que solo Sd?


selectos_min <- filter(datos_agro, Producto %in%  menor_sd[0:n,]$Producto)
selectos_max <- filter(datos_agro, Producto %in%  mayor_sd[0:n,]$Producto)


# Grafica la variacion del precio a lo largo del tiempo
plot_min <- graficar_tiempo_producto(selectos_min, "Menor desviacion")
plot_mas <- graficar_tiempo_producto(selectos_max, "Mayor desviacion")


##########################################################################
###################     Graficas para productos encontrados     ##########
##########################################################################
# para graficar en una sola figura
plot_list <- list(plot_min, plot_mas)
final_plot <- wrap_plots(plot_list, ncol = 2)
print(final_plot)

# Comparar con comportamiento de inflacion
comparar_inflacion(selectos_min, "selectos min")
comparar_inflacion(selectos_max, "selectos max")


# Graficar por cada producto
for(i in unique(selectos_min$Producto)) {
  plot1 <- producto_comportamiento_tiempo(i)
  #plot2 <- producto_comportamiento_ciudades(i, 2022)
  #plot3 <- comparar_inflacion(filter(datos_agro, Producto == i), i)
  
  print(plot1)
  # Etso es para pintar varios plots en una sola figura :)
  #combined_plot <- plot_grid(plot1, plot2, plot3, ncol = 1)
  # Display the combined plot for the current iteration
  #print(combined_plot)
}


##########################################################################
###################     Correlacion con insumos    #######################
##########################################################################

# Ver si existe una correlacion entre el precio de los productos y los insumos
# Paso 1: Merge de ambos datasets para solo tener los datos en comun
merged_data <- merge(selectos_min, insumos_andino, by = c("Departamento", "Fecha"))

# Paso 2: crear la matriz de correlacion 
correlation_matrix <- merged_data %>%
  group_by(Producto, Insumo) %>%
  summarise(correlation = cor(Precio, `Precio promedio`, use = "complete.obs"))

# Ordenar de mayor a menor
correlation_matrix <- correlation_matrix %>%
  arrange(Producto, desc(abs(correlation)))

m = 3
# Filtrar los m primeros insumos de cada producto
top_correlations <- correlation_matrix %>%
  group_by(Producto) %>%
  top_n(m, wt = abs(correlation))


head(top_correlations)

#TODO:  hacer unas grafiquitas que sea en un mismo plot el precio del producto y el de los 3-5 insumos en el tiempo




#TODO: de los productos que salgan investigarlos en internet, su presencia en la region andina. Tener mas info

##########################################################################
###################     Funciones     ####################################
##########################################################################


promedioCiudades <- function(nombre_producto){
  p1 <- datos_agro %>%
    filter(Producto == nombre_producto) %>%
    group_by(year = lubridate::year(Fecha)) %>%
    mutate(numCiudades = length(unique(Ciudad)))
  p1 <- p1 %>%
    group_by(year(Fecha)) %>%
    summarize(promedioCiudadAño = mean(numCiudades))

  return(  mean(p1$promedioCiudadAño))

}