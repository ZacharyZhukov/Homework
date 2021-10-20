library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(sf)


greendb = read_csv("~/Downloads/greendb.csv")

greendb %>% summary
greendb |> summary()


glimpse(greendb)
colnames(greendb)
names(greendb)


greendb = greendb %>% select(-species_l, -d_canopy_m)
greendb = greendb %>% select(-source)


greendb$class |> unique() 

greendb = greendb %>% filter(height_m >10) %>% 
                      filter(class == "Tree")

greendb = greendb[greendb$height_m > 10 & greendb$class =="Tree",]

greendb$species_ru[greendb$species_ru == "Пень"] = NA
greendb$species_ru[greendb$species_ru == "Пень"]


greendb = greendb %>% mutate(d_radius_m = d_trunk_m /2)

greendb = greendb %>% mutate(species = case_when(
  species_ru == "Пень" ~ "NA",
  TRUE ~ species_ru))

greendb = greendb %>% filter(species_ru != "NA", !is.na(species_ru))


greendb = greendb %>% mutate(r_trunk_m = d_trunk_m/2,
                             area_trunk_m2 = r_trunk_m^2*pi)



greendb$species_ru %>% unique()

data = greendb
data$d_trunk_m
rad = data$d_trunk_m / 2
basal = rad * rad * pi 
basal
data$basal = basal
data$basal = (data$d_trunk_m /2)*(data$d_trunk_m/2)* pi
 

# Задание 1: В таблице data создать колонку Vtrunk в которой 
# будет 
# посчитан объем ствола
   

unique(data$species_ru)
data$species_ru %>% unique
data$species_ru |> unique()

data$species_ru = factor(data$species_ru)
summary(data$species_ru)


sum_table = greendb %>% group_by(species_ru) %>% 
                summarise(
                          diam_m = mean(d_trunk_m, na.rm=T),
                          num = n(),
                          height_m = mean(height_m, na.rm=T)
                )


sum_table  = greendb %>% group_by(species_ru) %>%
                summarise(
                      num = n()
                ) %>% arrange(desc(num))



sum_table = summarise(group_by(greendb, species_ru), 
    diam_m = mean(d_trunk_m, na.rm=T),
    num = n(),
    height_m = mean(height_m, na.rm=T)
  )


divers = greendb %>% group_by(adm_region, species_ru) %>%
            summarise(
              nspecies = n()
            ) %>% select(-nspecies) %>% 
                  ungroup() %>% group_by(adm_region) %>%
            summarise(
              nspecies = n()
            )

# Получить сводную таблицу, где для каждого района будет 3
# доминирующих вида и количество деревьев в этом районе 
# для каждого из видов.
  
divers = greendb %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
  ) %>% group_by(adm_region) %>% 
  arrange(adm_region, desc(nspecies)) %>%
  mutate(order = order(nspecies, decreasing = T)) %>%
  filter( order <= 3) %>% select(-order)

library(tidyr)

transp = greendb %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
  ) %>% pivot_wider(names_from = species_ru, values_from = nspecies) %>%
 select(starts_with("Липа")) 



### MAPS
library(sf)
library(ggplot2)
library(ggthemes)

map = st_read("~/Downloads/boundary-polygon-lvl8.geojson", 
                 options = "ENCODING=UTF-8")
plot(map)

domin = greendb %>% group_by(adm_region, species_ru) %>%
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

#Построить карту на которой будет по районам отображена 
#средняя высота доминирующего вида растений
