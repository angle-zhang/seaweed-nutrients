
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)


jap_nutr <- read.csv("./data/jap_nutr.csv")
nutr_key <- read_excel('./data-raw/nutr_key.xlsx') 
dri_data <- read.csv('./data-raw/dri_nutrients.csv')

### helper functions
convert_units <- function(value, unit_orig, unit_new) {  
  if(unit_new=="" | is.na(unit_new)) {
    return(value)
  }
  else { 
    unit_conversionf <- c("g"=1, "mg"=1000, "mcg"=1000000, "ug"=1000000, "µg"=1000000)
    result <- unname(value*unit_conversionf[unit_new]/unit_conversionf[unit_orig])[1]
    return(result)
  }
}

nutr_key <- nutr_key %>% select(1,2,10) 

names(nutr_key) <- c('nutrient', "japan.nutrient", "ph_ranking")

# observe nutrient names
nutr_names <- jap_nutr %>% select(nutrient) %>% unique()
print(nutr_names)

# rename & fix units
jap_nutr1 <- jap_nutr %>% 
  merge(nutr_key, by.x="nutrient", by.y="japan.nutrient") %>%
  rename(nutrient_orig=nutrient) %>%
  rename(nutrient=nutrient.y) %>%
  mutate(units=gsub('\\/100 g', "", units)) %>%
  select(X, id, nutrient, everything()) 

convert_units  <- Vectorize(convert_units)

## get units and nutrients from DRI 
nutr_unit_key = dri_data %>%
  select(nutrient, nutrient_units) %>%
  rename(unit_conversion=nutrient_units) %>%
  filter(nutrient!="Protein") %>% # removing protein for now
  unique() 

jap_nutr2 = jap_nutr1 %>% 
  merge(nutr_unit_key) %>%
  mutate(value=convert_units(value, units, unit_conversion)) %>%
  mutate(units_orig=units, 
         units=unit_conversion) %>%
  select(-unit_conversion)

# get UL bounds with descriptions of groups (age, sex, etc)
ref_ul <- dri_data %>% 
  filter(units!="g/kg" | nutrient!="Protein" ) %>%
  group_by(nutrient, dri_type) %>%
  mutate(
    max_ref = max(ref_value),
    min_ref = min(ref_value), # min or max 
    mean_ref = mean(ref_value)
  ) %>%
  gather("summary_type", "value", min_ref, max_ref, mean_ref) %>%
  filter(summary_type=="mean_ref"| ref_value==value | ref_value==value) %>%
  mutate(across(c(sex_stage, sex, stage, age_range), ifelse, test=summary_type=="mean_ref", yes=NA)) %>%
  select(-data_type, -relevance, -X, -ref_value, -ref_units, -units) %>%
  rename(units=nutrient_units) %>%
  ungroup() %>%
  unique()


# get summary UL bounds without descriptions of groups 
ref_ul_summary <- ref_ul %>%
  select(-age_range, -sex, -sex_stage, -stage) %>%
  unique()

t <- ref_ul_summary %>%
  select(nutrient) %>%
  unique()
  

jap_dri1 = jap_nutr2 %>% 
  group_by(nutrient) %>%
  mutate(n = n()) %>%
  merge(ref_ul_summary, by=c("nutrient", "units")) %>%
  mutate(ID = row_number(), 
         proportion = value.x/value.y) %>%
  select(ID, everything()) 

# all seaweeds 
jap_nutr1 %>%
  ggplot(aes(x=nutrient, y=value, fill=nutrient)) +
  geom_boxplot() +
  scale_y_log10() 

# DRi of seaweeds 
jap_dri1 %>%
  filter(dri_type=="AI", summary_type=="mean_ref") %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=nutrient)) +
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip() 

# AI by seaweed type
jap_dri1 %>%
  filter(dri_type=="AI", summary_type=="mean_ref") %>%
  drop_na(algae_type) %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=algae_type)) +
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip()

# UL by seaweed type
jap_dri1 %>%
  filter(dri_type=="UL", summary_type=="mean_ref") %>%
  drop_na(algae_type) %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=nutrient)) +
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip() + 
  facet_wrap(~algae_type)




# all seaweeds by type (green, red, brown)


# all raw seaweds

