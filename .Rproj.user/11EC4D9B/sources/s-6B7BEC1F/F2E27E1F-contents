library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster

#k-medias
datos<-iris
irisCompleto<-iris[complete.cases(iris),]
km<-kmeans(iris[,1:4],3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Species))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Species))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Species))*100

plotcluster(iris[,1:4],km$cluster) #grafica la ubicación de los clusters

#Clustering jerárquico
hc<-hclust(dist(iris[,1:4])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
g3HC<-datos[datos$gruposHC==3,]

#Fuzzy C-Means
fcm<-cmeans(iris[,1:4],3)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

#Mixture of gaussians
mc<-Mclust(iris[,1:4],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]

#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(iris[,1:4]))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal

#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(iris[,1:4]))
mean(silch[,3]) #0.51, no es la mejor partición pero no está mal

#Método de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(iris[,1:4]))
mean(silfcm[,3]) #0.54, no es la mejor partición pero no está mal

#Método de la silueta para mixture of gaussians
silmg<-silhouette(mc$classification,dist(iris[,1:4]))
mean(silmg[,3]) #0.50, no es la mejor partición pero no está mal

#Método de Ward para determinar el número correcto de clusteres con k-medias
#Para saber cual es el mejor numero de clusters
wss <- (nrow(iris[,1:4])-1)*sum(apply(iris[,1:4],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(iris[,1:4], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

