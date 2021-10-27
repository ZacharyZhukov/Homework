library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(sf)


greendb = read_csv("~/Downloads/greendb.csv")

greendb %>% summary
greendb |> summary()

#######
### Посчитать регрессионную зависимость высоты от диаметра ствола для район
### Матушкино вида Ясень обыкновенный
######

fit_data = greendb %>% 
  filter(species_ru == "Ясень обыкновенный", adm_region == "район Матушкино", height_m < 100) %>% select(
    height_m, d_trunk_m
  )
ggplot(fit_data, aes(x=height_m, y=d_trunk_m)) +
  geom_point()+
  geom_smooth(method=lm)