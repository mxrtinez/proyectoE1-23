library("readxl") # para leer archivos Excel
library("tidyverse") # para manejo de datos
library(dplyr) # para hacer joins
# Ahora exportar eso a un mega archivp
#install.packages("openxlsx")

library(openxlsx)
#CREANDO UN MEGAARCHIVO CON LOS DATOS

archivos_meses <- c(
  "series-historicas-precios-mayoristas-2019.xlsx",
  "series-historicas-precios-mayoristas-2020.xlsx",
  "series-historicas-precios-mayoristas-2021.xlsx",
  "series-historicas-precios-mayoristas-2022.xlsx",
  "anex-SIPSA-SerieHistoricaMayorista-2023.xlsx"
)

folder <- "D:/UIS SEXTO SEMESTRE/ESTADISTICA/proyecto/datosRaw"
data_list <- list()

# extraer las hojas por meses del archivo
for (nombre_archivo in archivos_meses){
  file = paste(folder, '/', nombre_archivo, sep="")
  sheets <- excel_sheets(file)
  sheets <- sheets[-1] # la primera hoja es de indice 
  # leer todos los meses y meterlos a un dataset
  dato_meses <- lapply(sheets, function(sheet) {
    datos <- readxl::read_xlsx(file, skip = 6, sheet = sheet) %>%
      select(-Fecha) # eliminando la columna de fecha
    
    colnames(datos) <- c("Grupo", "Producto", "Mercado","Precio") # cambiando los nombres de las columnas
    # algunos datos estaban en minuscula y daban error
    datos$Grupo <- toupper(datos$Grupo)
    return(datos)
  })
  year_match <- str_match(nombre_archivo, "\\b(\\d{4})\\b")[1]
  data_list[[year_match]] <- dato_meses
}

# data_list es un dataset con un archivo por mes, por cada año
for(i in names(data_list)){
  print(paste(i, "meses:", length(data_list[[i]])))
}


# Ahora creare un datset por cada año que contenga toda la infomacion de los meses
nombres_meses <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)

columns_to_match <- c("Grupo", "Producto", "Mercado")

# juntar los precios de los meses en un solo dataset
# al final se tiene un dataset por cada año con una columna por cada mes
new_list = list()
for (year in names(data_list)) {
  d = data_list[[year]]
  meses = length(d)
  #nueva columna de precio con el nombre del mes
  result <- d[[1]] %>%
    mutate(!!nombres_meses[1] := Precio) %>% 
    select(-Precio)
  
  for(i in 2:meses){
    result <- inner_join(result, d[[i]], by = columns_to_match) %>%
      mutate(!!nombres_meses[i] := Precio) %>%
      select(-Precio)
  }
  # calcular el promedio de un producto en el año
  #result$Promedio <- rowMeans(result[, 4:meses], na.rm = TRUE)
  #solo las filas que tengan informacion
  result <- result[complete.cases(result),]
  result <- na.omit(result)
  result <- result %>%
    mutate(Ciudad = str_extract(Mercado, "^[^,]+")) %>%
    select(Grupo, Producto, Ciudad, Mercado, everything())
  result$Promedio <- rowMeans(result[, 5:meses], na.rm = TRUE)
  new_list[[year]] <- result
  result <- NULL
}



# Exportar como excel
wb <- createWorkbook()
# Add each dataset as a sheet in the Excel workbook
years = names(new_list)
for (i in 1:length(new_list)) {
  addWorksheet(wb, sheetName = paste0("Sheet", years[i]))
  writeData(wb, sheet = i, x = new_list[[years[i]]], startCol = 1, startRow = 1)
}
# Save the Excel workbook to a file
saveWorkbook(wb, file = paste(folder, "daticos4.xlsx", sep=""), overwrite = TRUE)



# Los datos de 2013-2018 estan en un formato ligeramente diferente
nombre_archivo_promedios = "series-historicas-precios-mayoristas.xlsx"

data_list2 = list()
file = paste(folder, '/', nombre_archivo_promedios, sep="")
sheets <- excel_sheets(file)
sheets <- sheets[-1] # la primera hoja es de indice 

# leer todos los años y meterlos a un dataset
for(sheet in sheets) {
  datos <- readxl::read_xlsx(file, skip = 9, sheet = sheet)
  colnames(datos) <- c("Fecha","Grupo", "Producto", "Mercado","Precio") 
  # algunos datos estaban en minuscula y daban error
  datos$Grupo <- toupper(datos$Grupo)
  datos <- na.omit(datos)
  
  list_mes = split(datos, datos$Fecha)
  names(list_mes) <- nombres_meses
  list_mes <- lapply(list_mes, function(mes) {
    r <- mes %>%
      select(-Fecha)
  })
  
  #unir todos los meses en un solo dataset
  result <- list_mes[["enero"]] %>%
    mutate(!!nombres_meses[1] := Precio) %>% 
    select(-Precio)
  columns_to_match <- c("Grupo","Producto","Mercado")
  for(i in 2:12){
    mes <- nombres_meses[i]
    result <- inner_join(result, list_mes[[mes]], by = columns_to_match) %>%
      mutate(!!mes := Precio) %>%
      select(-Precio)
  }
  # calcular el promedio de un producto en el año
  #solo las filas que tengan informacion
  result <- result[complete.cases(result),]
  result <- na.omit(result)
  result <- result %>%
    mutate(Ciudad = str_extract(Mercado, "^[^,]+")) %>%
    select(Grupo, Producto, Ciudad, Mercado, everything())
  result$Promedio <- rowMeans(result[, 5:meses], na.rm = TRUE)
  data_list2[[sheet]] <- result
  result <- NULL
}


# junatmos los dos datasets:
promedios_anuales <- c(data_list2, new_list)

# Exportar como excel
wb <- createWorkbook()
# Add each dataset as a sheet in the Excel workbook
years = names(promedios_anuales)
for (i in 1:length(years)) {
  addWorksheet(wb, sheetName = paste0("Sheet", years[i]))
  writeData(wb, sheet = i, x = promedios_anuales[[years[i]]], startCol = 1, startRow = 1)
}
# Save the Excel workbook to a file
saveWorkbook(wb, file = paste(folder, "precios_por_mes_2013_2023.xlsx", sep=""), overwrite = TRUE)


# Ahora creare un dataset con solo los promedios
years = names(promedios_anuales)
year = years[1]
columnas = c("Grupo", "Producto", "Ciudad", "Mercado", "Promedio")
columns_to_match = c("Grupo", "Producto", "Ciudad", "Mercado")
#nueva columna de precio con el nombre del mes
all_years <- promedios_anuales[[year]][,columnas] %>%
  mutate(!!year := Promedio) %>% 
  select(-Promedio)

for(i in 2:length(years)){
  year = years[i]
  all_years <- inner_join(all_years, promedios_anuales[[year]][,columnas],
                       by = columns_to_match) %>%
    mutate(!!year := Promedio) %>%
    select(-Promedio)
}

wb <- createWorkbook()
addWorksheet(wb, sheetName = "todo")
writeData(wb, sheet = 1, x = all_years, startCol = 1, startRow = 1)
# Save the Excel workbook to a file
saveWorkbook(wb, file = paste(folder, "precios_promedio_2013_2023.xlsx", sep=""), overwrite = TRUE)



## Quiero guardarlo todo en uno para mostrarlo con el tiempo lo mas de chevere
# extraer las hojas por meses del archivo
data_list3 = list()
for (nombre_archivo in archivos_meses){
  file = paste(folder, '/', nombre_archivo, sep="")
  sheets <- excel_sheets(file)
  sheets <- sheets[-1] # la primera hoja es de indice 
  # leer todos los meses y meterlos a un dataset
  dato_meses <- lapply(sheets, function(sheet) {
    datos <- readxl::read_xlsx(file, skip = 6, sheet = sheet)
    
    colnames(datos) <- c("Fecha","Grupo", "Producto", "Mercado","Precio") # cambiando los nombres de las columnas
    # algunos datos estaban en minuscula y daban error
    datos$Grupo <- toupper(datos$Grupo)
    return(datos)
  })
  
  year_match <- str_match(nombre_archivo, "\\b(\\d{4})\\b")[1]

  combined_dataset <- do.call(rbind, dato_meses)
  data_list3[[year_match]] <- combined_dataset
}
data_list3
combined_dataset <- do.call(rbind, data_list2)



nombre_archivo_promedios = "series-historicas-precios-mayoristas.xlsx"
data_list4 = list()
file = paste(folder, '/', nombre_archivo_promedios, sep="")
sheets <- excel_sheets(file)
sheets <- sheets[-1] # la primera hoja es de indice 
# leer todos los años y meterlos a un dataset
for(sheet in sheets) {
  datos <- readxl::read_xlsx(file, skip = 9, sheet = sheet)
  colnames(datos) <- c("Fecha","Grupo", "Producto", "Mercado","Precio") 
  # algunos datos estaban en minuscula y daban error
  datos$Grupo <- toupper(datos$Grupo)
  datos <- na.omit(datos)
  data_list4[[sheet]] = datos
  
}
combined2 <- do.call(rbind, data_list4)
to_combine <- c(data_list4, data_list3)
combined_dataset <- do.call(rbind, to_combine)
combined_dataset$Fecha <- as.Date(combined_dataset$Fecha)


result <- combined_dataset %>%
  mutate(Ciudad = str_extract(Mercado, "^[^,]+")) %>%
  select(Fecha,Grupo, Producto, Ciudad, Mercado, everything())


result
wb <- createWorkbook()
addWorksheet(wb, sheetName = "todo")
writeData(wb, sheet = 1, x = result, startCol = 1, startRow = 1)
# Save the Excel workbook to a file
saveWorkbook(wb, file = paste(folder, "precios_all.xlsx", sep=""), overwrite = TRUE)

