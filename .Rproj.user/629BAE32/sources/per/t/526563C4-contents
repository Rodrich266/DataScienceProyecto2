#----- Instalación e importación de librerias necesarias para correr el programa -----#
for (libreria in c("memisc","foreign","filesstrings","openxlsx","data.table","tidyr","dplyr")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#----------------- Descarga de datos y extraccion de los mismos ---------------------#
#Ya que el dataset es de Kaggle y se necesita un sign-in para descargarlo, ya se incluye en el folder

#Se descomprime el archivo y se coloca en un folder llamado datos
unzip("black-friday.zip", exdir = "./datos")

#Se lee el archivo descomprimido para realizar un análisis preliminar
BF_raw <- read.csv("./datos/BlackFriday.csv")
View(BF_raw)
