setwd("C:/Users/alvar/OneDrive/Escritorio/CUNEF 2º.2/MÁSTER/CLASE/ANALISIS DE TEXTO")

library(readr)
library(quanteda)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(gbm)
library(wordcloud)
library(wordcloud2)
library(tidytext)  
library(gridExtra)

Datos_tokens<- tokens(sample_trump_tweets$text, 
                      remove_symbols = TRUE,
                      remove_punct = TRUE, 
                      remove_numbers = TRUE)
Datos_tokens<-tokens_remove(Datos_tokens, stopwords("english"))
Datos_tokens<-tokens_wordstem(Datos_tokens)

Repetición_palabras<- dfm(Datos_tokens, tolower = TRUE, verbose = FALSE)
topfeatures(Repetición_palabras)



# Hay relación entre el número de veces que se retuitea un tweet (retweet_count) y el número de likes (favorite_count)?
  
Retweet_favorito<- sample_trump_tweets%>%
  select(retweet_count, favorite_count)%>%
  filter(retweet_count != is.na(retweet_count) )

plot_correlation(Retweet_favorito)

Relación_Retweet_favorito<- ggplot(Retweet_favorito, aes(x=retweet_count, y=favorite_count))+
  geom_point(size= 0.5, color="Pink")+
  xlim(0,232320)+
  theme_light()+
  labs(x= "Número de retweet", y="Número de favoritos", 
       title="Relación entre retweets y favoritos",
       subtitle = "Muestra Total")
Relación_Retweet_favorito
Relación_Retweet_favorito_2<- ggplot(Retweet_favorito, aes(x=retweet_count, y=favorite_count))+
  geom_point(size= 0.5, color="Pink")+
  xlim(0,100000)+
  ylim(0,100000)+
  theme_light()+
  labs(x= "Número de retweet", y="Número de favoritos", 
       title="Relación entre retweets y favoritos",
       subtitle = "Muestra reducida")

grid.arrange(Relación_Retweet_favorito,Relación_Retweet_favorito_2)


 
Grupos<- sample_trump_tweets%>%
  mutate(Grupos = case_when(
    retweet_count > favorite_count ~ "Grupo Retweet",
    retweet_count < favorite_count ~ "Grupo Favorito"
  ))

Datos_Retweet<- Grupos%>%
  filter(Grupos == "Grupo Retweet")%>%
  select(text)

Tabla_Retweet<- tibble(Value = Datos_Retweet$text)
Tabla_Retweet <- unnest_tokens(Tabla_Retweet, Word, Value)
Tabla_Retweet<- anti_join(Tabla_Retweet, stop_words, by=c("Word"="word"))

Número_Retweet<- Tabla_Retweet%>%
  filter(Word != "http" & Word != "https" & Word != "t.co")%>%
  count(Word, sort = TRUE)

W1<- wordcloud2(Número_Retweet, size = 0.5, shape = 'circle')
W1


Datos_Favorito<- Grupos%>%
  filter(Grupos == "Grupo Favorito")%>%
  select(text)

Tabla_Favoritos<- tibble(Value = Datos_Favorito$text)
Tabla_Favoritos <- unnest_tokens(Tabla_Favoritos, Word, Value)
Tabla_Favoritos<- anti_join(Tabla_Favoritos, stop_words, by=c("Word"="word"))

Número_Favoritos<- Tabla_Favoritos%>%
  filter(Word != "http" & Word != "https" & Word != "t.co")%>%
  count(Word, sort = TRUE)

W2<- wordcloud2(Número_Favoritos, size = 0.5, shape = 'circle')
W2


W3<- wordcloud(words = Número_Favoritos$Word, freq = Número_Favoritos$n, max.words = 200, random.order = FALSE, random.color = TRUE)

