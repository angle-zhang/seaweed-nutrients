library(readr)
library(tidyverse)
library(dplyr)

usda_nutrients_raw <- read_csv("usda_nutrients_raw.csv")

af_nutrients <- usda_nutrients_raw %>% 
  filter(food_catg=="Vegetables (without potatoes)") %>%
  select(food_name) %>%
  unique()
