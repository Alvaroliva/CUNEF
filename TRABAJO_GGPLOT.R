library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(tidyr)
library(readr)
#PART 1
setwd("C:/Users/alvar/OneDrive/Escritorio/DATA/Data")
New_york <- read_csv("AB_NYC_2019.csv")
unique(New_york$neighbourhood_group)
## 1
unique(New_york$neighbourhood_group)
New_york %>%
  group_by(neighbourhood_group) %>%
  summarise(count = n()) %>%
  ggplot(aes(neighbourhood_group, count, fill = neighbourhood_group)) +
  geom_col() +
  theme(legend.position = "none") +
  theme(panel.background = element_blank()) +
  labs(title = "AirBnB in each NYC big neighbourhood") 
## In this plot we could see who many AirBnBs are in each big 
## neighbourhood. Manhattan and Brooklyn have the mayority of them.

## 2
New_york %>%
  filter(price < 1000) %>%
  ggplot(aes(price)) +
  geom_histogram(binwidth = 10, fill = "darkblue") +
  labs(title = "Night prices of AirBnB",
       subtitle = "Cheaper than 1000$") 

New_york %>%
  filter(price < 1000) %>%
  ggplot(aes(price)) +
  geom_area(stat = "bin", binwidth = 30, fill = "darkblue") +
  labs(title = "Night prices of AirBnB",
       subtitle = "Cheaper than 1000$") 
## With this two graphics we could see the distribution of night prices 
## (cheaper than 1000). Most of them have praces between 50 and 250$.


##3 
New_york %>% 
  filter(neighbourhood_group == "Manhattan" | neighbourhood_group == "Brooklyn" ) %>%
  filter(price <= 500) %>% 
  ggplot(aes(price, fill = neighbourhood_group)) +
  geom_area(stat = "bin", bins = 30) +
  theme(legend.position = "bottom") + 
  labs(title =  "Night prices of AirBnB",
       subtitle = "Cheaper than 500$",
       caption = "Manhattan and Brooklyn")
## The different prices between Brookyn and Manhattan could be seen by 
## this plot. Brooklyn have higher prices than Manhattan.

#4
New_york %>%
  group_by(neighbourhood_group) %>%
  summarise(media = mean(price)) %>%
  ggplot() +
  geom_col(aes(neighbourhood_group, media, fill = neighbourhood_group)) +
  theme(legend.position = "left") +
  theme(panel.background = element_blank()) +
  theme(axis.text.x = element_text( colour = "white")) 
## With

#PART 2  
Human_resources <- read_csv("human_resources.csv")  
unique(Human_resources$left)
##1
class(Human_resources$left)
Human_resources <- Human_resources %>% 
  mutate(left = if_else(left == 1, "leave", "don't leave")) 
Human_resources %>%
  group_by(left) %>%
  summarise(count = n())

ggplot(Human_resources) +
  geom_bar(aes(x = left, fill = left)) +
  theme(legend.position = "none") +
  theme(panel.background = element_blank()) +
  labs(title = "Workers during the aviable history",
       subtitle = "Leave or don't leave")

##2
ggplot(Human_resources) +
  geom_histogram(aes(x = last_evaluation, fill = left), bins = 30) +
  facet_grid(left ~ .) +
  labs(title = "Last evaluation") +
  theme(axis.text = element_text(colour =  "red")) +
  theme(legend.position = "bottom")
 
##3
Human_resources %>%
  group_by(left) %>%
  summarise(media = mean(satisfaction_level)) %>%
  ggplot() +
  geom_col(aes(left, media, fill = left)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.background = element_rect( fill = "light yellow")) +
  theme(legend.position = "none") +
  labs(title = "Average of the satisfaction level") 

##4
ggplot(Human_resources) +
  geom_histogram(aes(x = satisfaction_level, fill = left), bins = 30) +
  theme(legend.position = "none") +
  facet_grid(left ~ .)
  labs(title = "Satisfaction level",
       caption = "It's lower among the workers who leave the company",
       x = "satisfaction level")
  
##5
Human_resources %>%
  mutate(salary = factor(salary, levels = c("low", "medium", "high"))) %>%
  ggplot(aes(left, fill = salary)) +
  geom_bar() +
  facet_grid(salary ~ .) +
  ylim(c(0, 5500)) +
  labs(title = "")


