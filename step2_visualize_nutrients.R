
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)


jap_nutr <- read.csv("./data/jap_nutr.csv")
nutr_key <- read_excel('./data-raw/nutr_key.xlsx') 

nutr_key <- nutr_key %>% select(1,2,10) 

names(nutr_key) <- c('nutrient', "japan.nutrient", "ph_ranking")

# observe nutrient names
nutr_names <- jap_nutr %>% select(nutrient) %>% unique()
print(nutr_names)

jap_nutr <- jap_nutr %>% merge(nutr_key, by.x="nutrient", by.y="japan.nutrient") %>%
  rename(nutrient_orig=nutrient) %>%
  rename(nutrient=nutrient.y) %>%
  select(X, id, nutrient, everything()) 

# all seaweeds 
jap_nutr %>%
  ggplot(aes(x=nutrient, y=value, fill=nutrient)) +
  geom_boxplot() +
  scale_y_log10() 



# all seaweeds by type (green, red, brown)


# all raw seaweds