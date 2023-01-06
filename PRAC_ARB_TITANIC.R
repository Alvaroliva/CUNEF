##########  ARBOLES DE CLASIFICACION   #############

##############  EJEMPLO_1_TITANIC ##################



library(titanic)
library(tidyverse)
data("titanic_train")

#Paquetes necesarios

library(rpart) 

library(rattle) #GRAFICOS DE VISUALIZACION
library(tibble)
library(bitops)

library(rpart.plot)


# 1. MODELADO CON ÃRBOLES DE CLASIFICACIÃN

arbol=rpart(
  formula = Survived ~Sex + Age,
  data=titanic_train,
  method = 'class'
  
)

# Por ejemplo, para modelos de regresion, method= anova (nÂºinfectados)


# 2. GRAFICO DEL ARBOL

fancyRpartPlot(arbol)




# 3. METODO PREDICTIVO 

pred_arbol = predict(arbol, type = 'class')


## Creamos un dataframe con los datos originales y la prediccion
 
### Se agrega una nueva variable con el resultado de la prediccion

titanic_pred = cbind(titanic_train, pred_arbol)


# 4. PODEMOS REALIZAR PREGUNTAS:

## �Que pasaria con pasajero masculino de 4 anos de edad?

predict(obj =arbol,
        newdata=data.frame(Age = 4,
                           Sex = 'male'),
                           type = 'class')

#Lo clasifica 1, como superviviente      


####  ANADIMOS LA CLASE DE CABINA (Pclass)

# 1. MODELADO CON ARBOLES DE CLASIFICACION

arbol1=rpart(
  formula = Survived ~Sex + Age + Pclass,
  data=titanic_train,
  method = 'class'
)


# 2. GRAFICO DEL ARBOL

fancyRpartPlot(arbol1)

