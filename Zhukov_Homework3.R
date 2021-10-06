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

summary.table = summarise(group_by(greendb_1, species_ru),
    diam_m=mean(d_trunk_m, na.rm=T),
    num=n(),
    height_m=mean(height_m, na.rm=T)
  )

divers= greendb_1 %>% group_by(adm_region, species_ru)%>%
  summarise(
              nspecies = n()
            )%>% select(-nspecies)%>% ungroup() %>% group_by(adm_region)%>%
            summarise(nspecies=n()
            )

# Получить сводную таблицу, где для каждого района будет 3
# доминирующих вида и количество деревьев в этом районе 
# для каждого из видов.

top3= greendb_1 %>% group_by(adm_region, species_ru)%>%
  summarise(
    nspecies = n()
    ) %>% group_by (adm_region)%>%
  arrange(adm_region, desc(nspecies)) %>%
  mutate(order=order(nspecies, decreasing = T))%>%
  filter(order<4)%>%select(-order)

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)

transp = greendb_1 %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
  ) %>% pivot_wider(names_from = species_ru, values_from = nspecies) %>%
  select(starts_with("Липа"))


#Построить карту на которой будет по районам отображена 
#средняя высота доминирующего вида растений


library(sf)
library(ggplot2)
library(ggthemes)


map = st_read("boundary-polygon-lvl8.geojson", 
              options = "ENCODING=UTF-8")
plot(map)

domin = greendb_1 %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
  ) %>% group_by(adm_region) %>% 
  arrange(adm_region, desc(nspecies)) %>%
  mutate(order = order(nspecies, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -nspecies) %>%
  rename(NAME = adm_region)


map  =left_join(map, domin, by="NAME")


ggplot() + geom_sf(data = map, aes(fill=species_ru))+
  theme_foundation() + theme(legend.title = element_blank())

heights = greendb_1 %>%group_by(adm_region) %>% 
  summarise(
    num = mean(height_m, na.rm=T),
    height_m = mean(height_m, na.rm=T)
  ) %>% group_by(adm_region) %>%
  arrange(adm_region, desc(height_m)) %>%
  mutate(order = order(height_m, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -height_m) %>%
  rename(NAME = adm_region)

map2 = left_join(map,heights, by="NAME")
ggplot() + geom_sf(data = map2, aes(fill=num))+
  theme_foundation() + theme(legend.title = element_blank())





