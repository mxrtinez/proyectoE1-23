## FUNCIONES ###

# Carcular variabilidad
normalizar_producto_tiempo <- function(nombre_producto){
  # Promedio mensual del precio a nivel regional
  promedios_regional <- datos_agro %>%
    filter(Producto == nombre_producto) %>%
    group_by(Fecha) %>%
    summarize(Promedio = mean(Precio))
  
  # Normalizar los precios para tener valores entre 0 y 1, mas faciles de comparar
  promedios_regional_normalizado <- promedios_regional %>%
    group_by(year = lubridate::year(Fecha)) %>%
    mutate(Promedio_normalizado = (Promedio / (max(Promedio, na.rm = TRUE))))
  
  return(promedios_regional_normalizado)
}





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


#Ver la tendencia global de los precios año a año, un poco ver la inflación
# Para hacer esto
#1. Escoger un año de referencia
#2. Identificar una canasta de productos que esten en todos los años.
#3. Filtrar los datos según esa canasta, obteniendo los precios de cada producto cada año. 
#Esto se usa para calcular el costo de la canasta de cada año. 
#4. Calcular el CPI para cada año (es una formulita)
#5. Calcular la taza de inflación (es una formulita con el CPI que se calculó en el paso anterior)
# Graficar la tasa de inflacion de un grupo de productos comparado con la inflacion nacional
comparar_inflacion <- function(productosdf,nombre_producto){
  
  producto1df <- calcular_tasa_inflacion(productosdf)
  # Create the first plot
  h <- ggplot() + 
    geom_line(data = datos_inflacion, aes(x = Fecha, y = inflacionTotal, color ='Inflacion nacional')) + 
    geom_line(data = producto1df, aes(x = Fecha, y = Inflacion, color = 'Inflacion producto')) +
    labs(title = paste("Tasa inflación", nombre_producto),
         y="Tasa inflación") +
    scale_color_manual(name = "Graph", values = c('Inflacion nacional' = 'black', 'Inflacion producto'='blue'))
  return(h)
}

# permite graficar varios productos y los agripa
graficar_tiempo_producto <- function(productosdf, nombre_producto){
  
  productosdf <- productosdf %>%
    group_by(Fecha, Producto) %>%
    summarise(Promedio_nacional = mean(Precio))
  
  h <- ggplot() + 
    geom_line(data = productosdf, aes(x = Fecha, y = Promedio_nacional, group=Producto, color=Producto), linewidth = 1) +
    labs(title = paste("Precio producto ", nombre_producto),
         y="Precio")
  return(h)
}

# Visualizar el comportamiento de un producto en cada año, para ver si se presentan ciclos
producto_comportamiento_tiempo <- function(nombre_producto){
  p1 <- datos_agro %>%
    filter(Producto == nombre_producto) %>%
    group_by(Fecha) %>%
    summarise(Promedio_nacional = mean(Precio))
  
  p1 <- p1 %>%
    group_by(year = lubridate::year(Fecha)) %>%
    mutate(normalizado = scale(Promedio_nacional, center = TRUE, scale = TRUE))
  
  p <- ggplot(p1, aes(x = month(Fecha, label = TRUE), y = normalizado, group = year(Fecha), color = factor(year(Fecha)))) +
    geom_point() +
    geom_line() +
    labs(title = paste("Tendencia Precio", nombre_producto, " región andina"),
         y="Precio normalizado",
         x = "Mes") +
    scale_x_discrete(labels = abreviaciones_meses) +  # Display month names
    scale_color_manual(values = rainbow(length(unique(p1$year))))  # Set colors for each year
  
  return(p)
}


producto_comportamiento_tiempo2 <- function(nombre_producto){
  p1 <- datos_agro %>%
    filter(Producto == nombre_producto) %>%
    group_by(Fecha) %>%
    summarise(Promedio_nacional = mean(Precio))
  
  p1 <- p1 %>%
    group_by(year = lubridate::year(Fecha)) %>%
    mutate(normalizado = scale(Promedio_nacional, center = TRUE, scale = TRUE))

    
  p <-  ggplot(p1, aes(x = month(Fecha, label = TRUE), y = normalizado, group = year(Fecha), color = factor(year(Fecha)))) +
    geom_smooth(method = "loess", formula = y ~ x) +
    labs(title = paste("Tendencia Precio", nombre_producto, " región andina"),
         y="Precio normalizado",
         x = "Mes") +
    scale_x_discrete(labels = abreviaciones_meses) +  # Display month names
    scale_color_manual(values = rainbow(length(unique(p1$year))))  # Set colors for each year
  
  return(p)
}



# visualizar el comportamiento de varios productos
productos_comportamiento_tiempo <- function(p1, nombre_producto){
  p1 <- p1 %>%
    group_by(Fecha) %>%
    summarise(Promedio_nacional = mean(Precio))
  
  p1 <- p1 %>%
    group_by(year = lubridate::year(Fecha)) %>%
    mutate(normalizado = scale(Promedio_nacional, center = TRUE, scale = TRUE))
  
  p <- ggplot(p1, aes(x = month(Fecha, label = TRUE), y = normalizado, group = year(Fecha), color = factor(year(Fecha)))) +
    geom_point() +
    geom_line() +
    labs(title = paste("Tendencia Precio", nombre_producto, " región andina"),
         y="Precio normalizado",
         x = "Mes") +
    scale_x_discrete(labels = abreviaciones_meses) +  # Display month names
    scale_color_manual(values = rainbow(length(unique(p1$year))))  # Set colors for each year
  return(p)
}

# Ver el comportamiento de un producto en todas las ciudades
producto_comportamiento_ciudades <- function(nombre_producto, año){
  p1 <- datos_agro %>%
    filter(Producto == nombre_producto) %>%
    filter(year(Fecha) >= año) %>%
    group_by(mes = month(Fecha), Ciudad) %>%
    summarise(Promedio_mes = mean(Precio))
  
  p2 <- p1 %>%
    group_by(Ciudad) %>%
    mutate(normalizado = scale(Promedio_mes, center = TRUE, scale = TRUE))
  
  
  p <- ggplot(p2, aes(x = mes, y = normalizado, group = Ciudad, color = Ciudad)) +
    geom_point() +
    geom_line() +
    labs(title = paste("Tendencia Precio", nombre_producto, " región andina ",año),
         y = "Precio normalizado",
         x = "mes") +
    scale_x_continuous(breaks = 1:12, labels = abreviaciones_meses) +  # Display month names
    scale_color_manual(values = rainbow(length(unique(p2$Ciudad))))  # Set colors for each year
  return(p)
}





