

library(data.table)
library(knitr)
library(dplyr)

#all foods 
food_nutr <- read.csv('./data/jap_food.csv')
algae_nutr <- read.csv('./data/jap_algae.csv')

# euclidean <- function(a, b) sqrt(sum((a - b)^2))

food_nutr_mean <- food_nutr %>% 
  group_by(food_group, nutrient) %>%
  summarize(mean=mean(value), sd=sd(value), n=n()) %>%
  mutate(CV=sd/mean)

algae_nutr_mean <- algae_nutr %>% 
  group_by(genus, nutrient) %>%
  summarize(mean=mean(value), sd=sd(value), n=n()) %>%
  mutate(CV=sd/mean) %>%
  ungroup() 

# variation within each genus
algae_nutr_mean_wide = algae_nutr_mean %>% 
  setDT() %>%
  dcast(nutrient~genus, value.var=c("mean", "sd", "CV", "n"))

## TODO: make table 


