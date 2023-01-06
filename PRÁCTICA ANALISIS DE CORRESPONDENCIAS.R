setwd("C:/Users/admin/Downloads")

library(ca)
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(crosstable)
datos <- read_excel("datos_EILU_GRAD_2019.xlsx")
datos <- datos %>% 
  filter(TR_SUELDO != 9)
#a)
tabla <- table(datos$TITULACION, datos$TR_SUELDO)
tabla 
datos.tabla<-data.matrix(tabla)
fisher.test(datos.tabla, simulate.p.value = TRUE)
chisq.test(datos.tabla,simulate.p.value=T)
#Aunque con muchas variables es dificil extraer conclusiones, como se puede observar, el p-valor es cercano a 0. 
#Esto indica que al ser menor de 0.05 rechazamos la hipótesis nula y podemos decir que existen diferencias significativas entre ambas variables.
library(gmodels)
CrossTable(datos.tabla, prop.t=FALSE,prop.chisq=FALSE,format="SAS")
fit.ca<-ca(tabla)
summary(fit.ca, scree = TRUE, rows=TRUE, columns=TRUE)
# dim 3 (R3, tres dimensiones, y recoge el 94,9% de informacion)
print(fit.ca)
fviz_ca_biplot(fit.ca)+theme_grey()

fit.factominer<-CA(tabla,ncp=4,graph=TRUE)
summary(fit.factominer,graph=TRUE)

#b)
tabla <- table(datos$TR_SUELDO, datos$Rama)
datos.tabla<-data.matrix(tabla)
chisq.test(datos.tabla)
#Aunque con muchas variables es dificil extraer conclusiones, como se puede observar, el p-valor es cercano a 0. 
#Esto indica que al ser menor de 0.05 rechazamos la hipótesis nula y podemos decir que existen diferencias significativas entre ambas variables.
CrossTable(datos.tabla, prop.t=FALSE,prop.chisq=FALSE,format="SAS")
fit.ca<-ca(tabla)
summary(fit.ca, scree = TRUE, rows=TRUE, columns=TRUE)
# dim 3 (tres dimensiones, y recoge el 98,4% de informacion)
print(fit.ca)
fviz_ca_biplot(fit.ca)+theme_grey()
fit.factominer<-CA(tabla,ncp=3,graph=TRUE)
summary(fit.factominer,graph=TRUE)

#c)
#hombres
hombres <- filter(datos, SEXO == "HOMBRE")
tabla <- table(hombres$TR_SUELDO, hombres$Rama)
datos.tabla<-data.matrix(tabla)
chisq.test(datos.tabla)
CrossTable(datos.tabla, prop.t=FALSE,prop.chisq=FALSE,format="SAS")
fit.ca<-ca(tabla)
summary(fit.ca, scree = TRUE, rows=TRUE, columns=TRUE)
# dim 4 (cuatro dimensiones, y recoge el 98,5% de informacion)
print(fit.ca)
fviz_ca_biplot(fit.ca)+theme_grey()
fit.factominer<-CA(tabla,ncp=4,graph=TRUE)
summary(fit.factominer,graph=TRUE)



#mujeres
mujeres <- filter(datos, SEXO == "MUJER")
tabla <- table(mujeres$TR_SUELDO, mujeres$Rama)
datos.tabla<-data.matrix(tabla)
chisq.test(datos.tabla)
CrossTable(datos.tabla, prop.t=FALSE,prop.chisq=FALSE,format="SAS")
fit.ca<-ca(tabla)
summary(fit.ca, scree = TRUE, rows=TRUE, columns=TRUE)
# dim 3 (tres dimensiones, y recoge el 98,4% de informacion)
print(fit.ca)
fviz_ca_biplot(fit.ca)+theme_grey()
summary(fit.factominer,graph=TRUE)

