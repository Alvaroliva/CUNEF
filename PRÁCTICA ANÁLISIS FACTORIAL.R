USArrests <- USArrests

#Comprobamos la estructura de la tabla 
str(USArrests)
summary(epa20)
stat.desc(epa20) # las varianzas no son excesivamente grandes

#A partir de este análisis voy a intentar detectar si hay variables no observadas
#que explican por qué los indicadores están relacionados entre sí y con ello 
#poder agruparse


cor.mat<-cor(USArrests, method="pearson") # matriz de correlaciones
cor.mat

corrplot(cor.mat, type="full", order="hclust", addrect = 3,
         tl.col="black", tl.cex=0.7, tl.srt=45)#todas las variables están correlacionadas positivamente

print(cortest.bartlett(cor.mat, n=10))#el p valor es muy pequeño y por tanto podemos utilizar la técnica

fit<-PCA(USArrests,scale.unit=TRUE,ncp=9,graph =TRUE)
fviz_pca_var(fit, col.var = "steelblue",labelsize = 3, repel = TRUE)
#Por tanto con las dimensiones 1 y 2 se recoge el 86.7% de la información. Vemos como Murder y Assault están muy correlacionadas.
#Es la dimension 2 la que separa a Urban Pop de las otras tres. 

plot(fit, axes = c(1,2), choix=c("ind"))+
  geom_label_repel() #En este gráfico podemos ver la relación que hay entre los Estados. En los cuales se forman 4 subgrupos

