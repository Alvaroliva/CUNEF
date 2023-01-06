##PRACTICA ANALISIS CLUSTER
library(readxl) 
library(dplyr)
library(stats)
library(NbClust)
eurostat2020 <- read_excel("C:/Users/admin/Downloads/eurostat_2020.xlsx", row.names(2))
eurostat2020[,-(2:7)]
#EJ1
#
mediterraneosynordicos<-eurostat2020 %>% 
  filter( Paises %in%  c("Spain","Portugal","France","Italy","Sweden", "Finland"))
dat <- scale(mediterraneosynordicos[,-1], center = TRUE, scale = TRUE)
rownames(dat) <- mediterraneosynordicos$Paises
matriz.dis.euclid<-dist(dat, method="euclidean",diag=TRUE)
print(matriz.dis.euclid)
#
dat2 <- mediterraneosynordicos[,-1]
matriz.dis.euclid_sin_esc<-dist(dat2,method="euclidean",diag=TRUE)
print(matriz.dis.euclid_sin_esc)

#EJ2
paises <- eurostat2020[,-1]
matriz.dis.euclid2<-dist(paises,method="euclidean",diag=TRUE)
hclust.cercano<-hclust(matriz.dis.euclid2,method="single")
data.frame(hclust.cercano[2:1])
#dendograma 
plot(hclust.cercano,labels=eurostat2020$Paises)
rect.hclust(hclust.cercano, k = 5, border = "red")

#EJ3
hclust.average<-hclust(matriz.dis.euclid2,method="average")
data.frame(hclust.average[2:1])
#dendograma 
plot(hclust.average,labels=eurostat2020$Paises)
rect.hclust(hclust.average, k = 6, border = "red")

#EJ4
hclust.ward<-hclust(matriz.dis.euclid2,method="ward.D")
data.frame(hclust.ward[2:1])
#dendograma 
plot(hclust.ward,labels=eurostat2020$Paises)
rect.hclust(hclust.ward, k = 4, border = "red")


#EJ5
res <- NbClust(paises, distance = "euclidean", min.nc = 2, max.nc = 4, 
               method = "ward.D2", index = "alllong")
kmeans <- kmeans(paises, 4)
medias <- aggregate(paises, by=list(kmeans$cluster), FUN = mean)
medias
datos5 <- data.frame(eurostat2020[,-(2:7)], kmeans$cluster)
colnames(datos5)<- c("Paises", "Cluster")
datos5 <- datos5[order(datos5$Cluster),]
print(datos5)
