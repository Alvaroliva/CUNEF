library(tidyverse)
library(janitor)
BrentOilPrices <- read_csv("C:/Users/alvar/OneDrive/Escritorio/DATA/Data/BrentOilPrices.csv")
#Ej.1 
detector <- function(tb, rows, cols){
  a <- nrow(tb)
  b <- ncol(tb)
  if(a >= rows | b >= cols){
    stop("The data is too big")
  } else {
    cat("It could be read appropiatly")
  }
}

detector(BrentOilPrices, 70000, 67)
## We could deliver this data frame because the rows and the colums are enought short.

#Ej.2
max(BrentOilPrices$Price)
price_detector <-  function(tb, a){
  tb %>% 
    clean_names() %>%
    mutate(price = if_else(price > a, "should have kept", "should have bought", missing = NULL)) %>%
    group_by(price) %>%
    ggplot(aes(price, fill = price)) +
    geom_bar() + 
    theme(legend.position = "none", 
          panel.background = element_rect(colour = "darkblue"),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(title = "Buy or keep")
  
}

price_detector(BrentOilPrices, 50)
##We should have bought around 5000 times

#Ej.3
datasaurus_dozen <- datasauRus::datasaurus_dozen
unique(datasaurus_dozen$dataset)
dat_sau_dozen_graphic <- function(datasets){
  datasaurus_dozen %>%
    filter(dataset %in% datasets) %>%
    group_by(dataset) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    facet_grid(dataset ~ .)
}


dat_sau_dozen_graphic(datasets = c("away", "h_lines", "bullseye"))
dat_sau_dozen_graphic(datasets = c("dino", "away", "v_lines"))
dat_sau_dozen_graphic(datasets = c("x_shape", "star", "high_lines"))
dat_sau_dozen_graphic(datasets = c("dots", "circle", "slant_up"))
dat_sau_dozen_graphic(datasets = c("h_lines", "wide_lines", "slant_down"))

#Ej.4 
vectors <- function(n = 1000,b,c, graphic){
  if(graphic == TRUE){
    un_tibble <- tibble(
      normal_distribution = rnorm(n, mean = b, sd = c),
      uniform_distribution = runif(n, min = c, max = b) 
    )
    colour <- c("blue","red")
    ggplot(un_tibble) +
      geom_point(aes(x = normal_distribution, y = uniform_distribution), colour = sample(colour, 1))
  } else{ 
    un_tibble <- tibble(
      normal_distribution = rnorm(n, mean = b, sd = c),
      uniform_distribution = runif(n, min = c, max = b) 
    )
    un_tibble <- un_tibble %>%
      arrange(normal_distribution)
    return(un_tibble)
    view(un_tibble)
  }
}

vectors(b = 3, c = 1.5, graphic = TRUE)
vectors(b = 3, c = 1.5, graphic = FALSE)

