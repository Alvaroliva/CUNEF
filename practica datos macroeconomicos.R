#PRACTICA FINAL
#1.IMPORTACION DE LIBRERIAS
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(psych) #test de barlet
library(FactoMineR) #PCA
library(foreign)
library(rpart)
library(rpart.plot)
library(rattle) # para representaciones
library(Epi)
library(janitor)

library(ggrepel)
library(factoextra)
library(FactoMineR)
library(pastecs)
library(paran)
library(nFactors)
library(Hmisc)


library(ggplot2)
library(ggrepel)
library(factoextra)
library(psych)
library(FactoMineR)
library(pastecs)
library(corrplot)
library(paran)
library(nFactors)
library(Hmisc)

#2. LEER EL EXCEL Y REALIZAR UN ANALISIS BASICO DE LAS VARIABLES
index <- read_excel(path = "D:/R CUNEF/3º R/PRACTICA FINAL/index2022_data.xlsx", na = "N/A")
index1 <- na.omit(index)

str(index1) #Vemos que algunas columnas numéricas son characters
#Con la funcion sapply y el codigo a continuacion transformamos estas variables
cols.num <- c("GDP (Billions, PPP)","GDP per Capita (PPP)", "5 Year GDP Growth Rate (%)", 
              "Unemployment (%)", "Public Debt (% of GDP)", "FDI Inflow (Millions)", 
              "Inflation (%)", "GDP Growth Rate (%)")
index1[cols.num] <- sapply(index1[cols.num],as.numeric)
str(index1) #Comprobamos que todas las variables ya son numericas
#Y realizamos un anásis exploratorio de los datos
head(index1)
index1 <- na.omit(index1)
summary(index1) #Y realizamos un analisis descriptivo de la informacion

#3.EJERCICIO 1 ----
#Realizamos una matriz de correlacion
cor.mat<-cor(index1[,-1], method="pearson")
cor.mat
corrplot(cor.mat, type="lower", order="original", 
         tl.col="black", tl.cex=0.7, tl.srt=45)
corrplot(cor.mat, type="full", order="hclust", addrect = 3,
         tl.col="black", tl.cex=0.7, tl.srt=45)

print(cortest.bartlett(cor.mat, n=10))

# Identificación de los componentes
fit<-PCA(index1[,-1],scale.unit=TRUE,ncp=9,graph =TRUE)
fviz_pca_var(PCA(index1[,-1],scale.unit=TRUE,ncp=9,graph =TRUE),
             col.var = "steelblue",labelsize = 2, repel = TRUE)

#varianza explicada
autoval= fit$eig
barplot(autoval[, 2], names.arg=1:nrow(autoval), 
        main = "Varianza explicada",
        xlab = "Componentes Principales",
        ylab = "Porcentaje explicado de la varianza",
        col ="steelblue",
        ylim=c(0,105))

paran(index1[,-1], iterations=5000,graph=TRUE,color=FALSE)

#Representación de las variables que más contribuyen a la explicación de las CP
fviz_contrib(fit, choice = "ind", axes = 1, top = 5)

fviz_contrib(fit, choice = "var", axes = 2, top = 3)

#4.EJERCICIO 2 ----
#Analisis jerarquico para ue27
ue <-index1 %>%
  filter(index1$`Country Name` %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                                      "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                                      "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", 
                                      "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", 
                                      "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"))

all <- scale(index1[,-1])
paises <- scale(ue[,-1]) #normalizamos los datos
matriz.dis.euclid2<-dist(paises,method="euclidean",diag=TRUE)
hclust.cercano<-hclust(matriz.dis.euclid2,method="single")
data.frame(hclust.cercano[2:1])
#dendograma 
plot(hclust.cercano,labels=ue$`Country Name`)
rect.hclust(hclust.cercano, k = 4, border = "red")
####vecino mas cercano
hclust.average<-hclust(matriz.dis.euclid2,method="average")
data.frame(hclust.average[2:1])
#dendograma 
plot(hclust.average,labels=ue$`Country Name`)
rect.hclust(hclust.average, k = 6, border = "red")
####metodo ward
hclust.ward<-hclust(matriz.dis.euclid2,method="ward.D")
data.frame(hclust.ward[2:1])
#dendograma 
plot(hclust.ward,labels=ue$`Country Name`)
rect.hclust(hclust.ward, k = 4, border = "red")

###no jerarquico
kmeans <- kmeans(all, 4)
medias <- aggregate(all, by=list(kmeans$cluster), FUN = mean)
medias
datos <- data.frame(index1[,(1)], kmeans$cluster)
colnames(datos)<- c("Paises", "Cluster")
datos <- datos[order(datos$Cluster),]
print(datos)

##EJERCICIO 3
idh <- read_excel('C:/Users/admin/Downloads/Human development index.xlsx')
idh1 <- idh %>%                               # Reemplazamos los valores de la columna 2018
  mutate(`2018` = replace(`2018`, `2018` >= 0.8, 1)) %>%
  mutate(`2018` = replace(`2018`, `2018` < 0.8, 0))
names(idh1)[1] <- "Country Name"
names(idh1)[2] <- "Human Index"
df_merge <- merge(idh1, index1, by = "Country Name",
                  all.x = TRUE)
df_merge <- na.omit(df_merge)
df_merge <- df_merge[,-1]
df_merge$`Human Index` <- factor(df_merge$`Human Index`) #factorizamos la variable a explicar
set.seed(1234)
train <- sample(nrow(df_merge), 0.8 * nrow(df_merge))
df_merge.train <- df_merge[train,]
df_merge.validate <- df_merge[-train,]
## ver ratio human index
prop.table(table(df_merge.train$`Human Index`))
prop.table(table(df_merge.validate$`Human Index`))
#Estimamos el arbol de train

arbol_train <- rpart(`Human Index` ~ ., 
                     data=df_merge.train, method="class")

fancyRpartPlot(arbol_train) # Visualizamos el arbol train
arbol_train$control

arbol_train <- rpart(`Human Index` ~ ., 
                       data=df_merge.train,
                       control=rpart.control(minsplit=5, minbucket=3),
                       method="class")

fancyRpartPlot(arbol_train) # Visualizamos el nuevo arbol train
#Predecimos el arbol y sacamos la matriz de confusion 
arbol.pred <- predict(arbol_train, df_merge.validate, type="class")

matriz <- table(df_merge.validate$`Human Index`, arbol.pred,
                dnn=c("Real", "Predicted"))
matriz
#Representacion de la curva ROC
ROC(data=df_merge, form= `Human Index` ~.)
