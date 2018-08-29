#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("rela","psych","FactoMineR","cluster","mclust","fpc","plyr")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#----------------- Descarga de datos y extraccion de los mismos ---------------------#
#Ya que el dataset es de Kaggle y se necesita un sign-in para descargarlo, ya se incluye en el folder

#Se descomprime el archivo y se coloca en un folder llamado datos
unzip("black-friday.zip", exdir = "./datos")

#----------------------------- Lectura de datos ------------------------------------#
#Se lee el archivo descomprimido para realizar un análisis preliminar
BF_raw <- read.csv("./datos/BlackFriday.csv", stringsAsFactors = F)
summary(BF_raw)

#----------------------------- Limpieza de datos -----------------------------------#
#Primero, las variables con caracteres se convierten a numéricas para un mejor análisis
#Y se crea una variable donde colocar los datos limpios
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

#Categoría de ciudad
#Se cambia A por 1, B por 2 y C por 3 en la categoría de ciudad
BF_Clean$City_Category[BF_Clean$City_Category == "A"] <- 1
BF_Clean$City_Category[BF_Clean$City_Category == "B"] <- 2
BF_Clean$City_Category[BF_Clean$City_Category == "C"] <- 3
#Ya que los valores aparecen como caracteres, se convierte toda la columna a numerica
BF_Clean$City_Category <- as.numeric(BF_Clean$City_Category)

#Estadía en ciudad actual
#En este caso los únicos datos diferentes son los "4+"
BF_Clean$Stay_In_Current_City_Years[BF_Clean$Stay_In_Current_City_Years== "4+"] <- 4
#Se convierten todos los valores ingresados a numéricos
BF_Clean$Stay_In_Current_City_Years <- as.numeric(BF_Clean$Stay_In_Current_City_Years)

#Las categorías de producto 2 y 3 contienen NAs y valores que pueden encontrarse en la categoría 1 por lo que se eliminan las columnas
BF_Clean <- subset(BF_Clean, select = -c(Product_Category_2,Product_Category_3))

#BF_Clean$Purchase_Avg <- merge(BF_Clean, ddply(BF_Clean, .(Product_ID), summarize, Purchase_Avg = mean(Purchase)), by = "Product_ID")

#-------------------------- Análisis de Correlación -------------------------------#
#Se realiza el analisis de correlacion de pearson para las variables numericas, sin los IDs
BF_Cor <- cor(BF_Clean[,c(3:10)],use = "pairwise.complete.obs")
View(BF_Cor)

#----------------------------- Análisis Gráfico -----------------------------------#
#Graficar frecuencias de variables categóricas en diagramas de barras y circulares
for (i in 3:9){
  barplot(table(BF_Clean[i]),main = paste("Diagrama de barras de", names(BF_Clean[i])))
  pie(table(BF_Clean[i]),main = paste("Diagrama circular de", names(BF_Clean[i])))
}

#Graficar frecuencias de variables cuantitativas en histogramas y caja y bigotes
for (i in 9:ncol(BF_Clean)){
  hist(BF_Clean[,i], freq = T, main = paste("Histograma de", names(BF_Clean[i])))
  boxplot(BF_Clean[,i], xlab =  names(BF_Clean[i]), main = paste("Caja y Bigotes de", names(BF_Clean[i])))
}

#---------------------------------- PCA -------------------------------------------#
#Se debe analizar si se puede usar el análisis factorial para formar las combinaciones lineales de las variables
BF_paf <- paf(as.matrix(BF_Clean[,3:10]))
BF_paf$KMO #0.51 La adecuación a la muestra es mala, pero aceptable
BF_paf$Bartlett #140362

#Nivel de significación de la prueba
cortest.bartlett(BF_Clean[,3:10])
#El valor p es de 0, lo que indica que el análisis factorial puede funcionar

#Normalización de datos
CP <- prcomp(BF_Clean[,3:10], scale = T)
CP

summary(CP)

CP_PCA <- PCA(BF_Clean[,3:10],ncp=ncol(BF_Clean[,3:10]), scale.unit = T)
summary(CP_PCA)

#------------------------------- Clustering --------------------------------------#

#Método de Ward para determinar el número correcto de clusteres con k-medias
wss <- (nrow(BF_Clean[,3:10])-1)*sum(apply(BF_Clean[,3:10],2,var))

for (i in 3:10) 
  wss[i] <- sum(kmeans(BF_Clean[,3:10], centers=i)$withinss)

plot(3:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# En base a la gráfica generada por el código anterior, se determina que se necesitan 3 clusters
