library("readxl") # para leer archivos Excel
library("tidyverse") # para manejo de datos
library("lubridate") # Trabajar con fechas

newData <- data.frame()
yearD <- 2013

while(yearD <= 2023){
  sheet <- yearD - 2012
  print(yearD)
  dataset <- readxl::read_xlsx("./datosRaw/datosRawprecios_por_mes_2013_2023.xlsx", sheet = sheet)
  columnDate <- lubridate::ymd(paste(yearD, "-", 2, "-", "01", sep = ""))
  for (i in 1:nrow(dataset)) {
    for (m in 5:14) {
      newRow <- data.frame(
        Date <- columnDate,
        Grupo <- dataset$Grupo[i][[1]],
        Producto <- dataset$Producto[i][[1]],
        Ciudad <- dataset$Ciudad[i][[1]],
        Mercado <- dataset$Mercado[i][[1]],
        Precio <- dataset[i,m][[1]]
      )
      newData <- rbind(newData, newRow)
    }
  }
  yearD <- yearD + 1
}

head(newData)

names(newData)[names(newData) == "Precio....dataset.i..m...1.."] <- "Precio"
names(newData)[names(newData) == "Producto....dataset.Producto.i...1.."] <- "Producto"
names(newData)[names(newData) == "Grupo....dataset.Grupo.i...1.."] <- "Grupo"
names(newData)[names(newData) == "Ciudad....dataset.Ciudad.i...1.."] <- "Ciudad"
names(newData)[names(newData) == "Mercado....dataset.Mercado.i...1.."] <- "Mercado"
names(newData)[names(newData) == "Date....columnDate"] <- "Date"

write.csv(newData, "newData.csv", row.names=TRUE)
