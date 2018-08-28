#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("plyr","ggplot2","class","caret","e1071")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#----------------- Descarga de datos y extraccion de los mismos ---------------------#
#Ya que el dataset es de Kaggle y se necesita un sign-in para descargarlo, ya se incluye en el folder

#Se descomprime el archivo y se coloca en un folder llamado datos
unzip("black-friday.zip", exdir = "./datos")

#----------------- Lectura de datos ------------------#
#Se lee el archivo descomprimido para realizar un análisis preliminar
BF_raw <- read.csv("./datos/BlackFriday.csv", stringsAsFactors = F)
summary(BF_raw)

#----------------- Limpieza de datos -----------------#
#Primero, las variables con caracteres se convierten a numéricas para un mejor análisis
#Se crea una variable donde colocar los datos limpios
BF_Clean <- BF_raw

#Product_ID no se modifica ya que es un identificador del producto

#Género
#Si el género es F colocar 1, si no, es M y colocar 0
BF_Clean$Gender <- ifelse(BF_Clean$Gender == "F",1,0)

#Edad
#Se clasifican los grupos de edad de 1 a 7
BF_Clean$Age[BF_Clean$Age == "0-17"] <- 1
BF_Clean$Age[BF_Clean$Age == "18-25"] <- 2
BF_Clean$Age[BF_Clean$Age == "26-35"] <- 3
BF_Clean$Age[BF_Clean$Age == "36-45"] <- 4
BF_Clean$Age[BF_Clean$Age == "46-50"] <- 5
BF_Clean$Age[BF_Clean$Age == "51-55"] <- 6
BF_Clean$Age[BF_Clean$Age == "55+"] <- 7
#Ya que los valores aparecen como caracteres, se convierte toda la columna a numerica
BF_Clean$Age <- as.numeric(BF_Clean$Age)

