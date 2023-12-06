library("readxl") # para leer archivos Excel
library("ggplot2") # para graficar
library("tidyverse") # para manejo de datos
library(dplyr) # para hacer joins
library(tidyr) # para reshape la tabla
library(RColorBrewer) # colorcitos de los plots

nombres_meses <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)

# obteniendo la informacion del megaarchivo
folder <- "D:/UIS SEXTO SEMESTRE/ESTADISTICA/proyecto/"
nombre_archivo <- "megaarchivo.xlsx"

file = paste(folder, '/', nombre_archivo, sep="")
sheets <- excel_sheets(file)
s <- "Sheet"
datos_list <- list()

# obtener todos los anos del dataset
for(s in sheets){
  datos <- readxl::read_xlsx(file, sheet = s)
  datos_list[[s]] <- datos
}

#mostrar el ano 2023
year_2023 = datos_list[['Sheet2023']]
kable(head(year_2023))


# Viendo que es lo que hay
productos <- split(year_2023$Producto, year_2023$Grupo) %>%
lapply(function(product) {
  unique(product)
})
names(productos)
head(productos)

# Voy a buscar los principales productos por cada grupo
# es decir los que mas se repiten en las filas
r <- year_2023 %>%
  group_by(Grupo,Producto) %>%
  summarize(Count = n()) %>%
  group_by(Grupo) %>%
  top_n(3, wt= Count)
print(r,n=24)


# veamos como se comporta el precio de un producto en ciertas ciudades
producto <- "Pimentón"
ciudades <- c("Medellín", "Cúcuta", "Bucaramanga", "Bogotá", "Cali")
a = "Sheet2023"
y <- datos_list[[a]] %>% 
  filter(Producto==producto)

p<-pivot_data(y) 
#%>%  filter(Ciudad %in% ciudades)

ggplot(data=p, aes(x=mes, y=precio, group=Ciudad, color=Ciudad))+
  geom_point() +
  geom_line() +
  labs(title = paste(producto, a)) +
  theme(legend.position = "none")


# Veamos como se comporta el precio del producto en una ciudad en diferentes anos
#primero voy a crear un mega dataframe con todos los anos
mega <- lapply(sheets, function(s){
  df <- datos_list[[s]]
  df$year <- as.numeric(regmatches(s, regexpr("\\d+", s)))
  return(df)
}) %>%
  bind_rows()

ciudad <- "Bucaramanga"
# las columnas de los meses
historial_bucaramanga <- pivot_data(mega,cols) %>%
  filter(Ciudad == ciudad & Producto == producto)

pl <- ggplot(data=historial_bucaramanga, 
       aes(x=mes, y=precio, group=year, color=year))+
  geom_point() +
  geom_line(linewidth=1) +
  labs(title = paste(producto, ciudad)) +
  scale_colour_distiller(
    palette = "Set1",
  ) +
  theme_minimal()
pl + guides(color = guide_legend(override.aes = list(shape = 16, keywidth = 1.5)))




# veamos que paso en 2022
ciudades <- c("Medellín", "Cúcuta", "Bucaramanga", "Bogotá", "Cali")
productos <- c("Carne de res, bola de pierna","Carne de res, sobrebarriga","Pechuga de pollo","Guayaba pera")
a = "Sheet2022"
y <- datos_list[[a]] %>% 
  filter(Grupo == "VERDURAS Y HORTALIZAS" & Ciudad=="Bucaramanga")

p<-pivot_data(y) 
#%>%  filter(Ciudad %in% ciudades)

ggplot(data=p, aes(x=mes, y=precio, group=Producto, color=Producto))+
  geom_point() +
  geom_line() +
  labs(title = paste(producto, a)) +
  theme(legend.position = "none")

# FUNCIONES
# para poder hacer graficas hay que transformar la data
pivot_data <- function(d){
  cols <- intersect(names(d),nombres_meses)
  # hay que cambiar el formato de la data para poder graficarla
  p = pivot_longer(d, cols = cols, names_to = 'mes', values_to = 'precio') 
  # para poder graficar con strings
  p$mes <- factor(p$mes, levels = cols)
  return(p)
}



         
