library(tidyverse)
library(dplyr)
library(readr)
data = read_csv("C:/Users/ZaharZhukov/Desktop/Моделирование/greendb_1.csv")
View(greendb_1)
data = greendb_1
rad = data$d_trunk_m /2
basal=rad^2*3.14
basal
data$basal=basal
data$basal = (data$d_trunk_m/2)*(data$d_trunk_m/2)*3.14


# 1 В таблице data создать колонку Vtrunk в которой будет
#посчитан объем ствола
data$Vtrunk = data$basal * data$height_m

unique(data$species_ru)
data$species_ru %>% unique
data$species_ru|>unique()

data$species_ru = factor(data$species_ru)
summary(data$species_ru)

summary.table = greendb_1 %>% group_by(species_ru)%>%
                     summarise(
                               diam_m=mean(d_trunk_m, na.rm=T),
                               num=n(),
                               height_m=mean(height_m, na.rm=T)
                     )
