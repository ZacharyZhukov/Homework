library(dplyr)


data = read_csv(file = "greendb.csv")

data = data %>% filter(adm_region = "район Матушкино")
model = lm(d_trunk_m ~ species_ru, data)

anova(model)