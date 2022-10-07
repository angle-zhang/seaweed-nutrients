library(readr)
library(tidyverse)
library(dplyr)
library(readxl)


load("./data-raw/afcd_sci.rda")

#japan data 
japan_scinames <- read_excel("data-raw/Japan/Japan_foodcomp_scinames.xlsx")
japan_nutrients <- read_excel("data-raw/Japan/Japan_foodcomp_values.xlsx")
japan_nutrients1 <- read_excel("./data-raw/Japan/Japan_aminoacid_values.xlsx")[,-c(1,3)]
japan_nutrients2 <- read_excel("./data-raw/Japan/Japan_fattyacid_values.xlsx")[,-c(1,3)]
#japan_nutrients3 <- read_excel("./data-raw/Japan/Japan_carb_values.xlsx")


taxa_table <- readRDS("data-raw/taxa_table.Rds")


#filter out algae & get same genuses from AFCD 
japan_sciname_key <- japan_scinames %>%
  drop_na(scientific.name) %>%
  filter(grepl('Algae',english.name.of.food.source)) %>%
  separate(english.name.of.food.source, into=c("algae", "common_name1", "common_name2"), sep=", ") %>%
  separate(scientific.name, into=paste0("scientific_name", 1:2), sep="\\[Syn. ") %>%
  mutate(genus=word(scientific_name1, 1)) %>%
  mutate(across(everything(), gsub, pattern = "-", replacement = " "),
         across(everything(), trimws),
         across(everything(), gsub, pattern = "\"|]", replacement = ""),
         across(c(common_name1, common_name2, genus), tolower)) %>%
  separate(item.no., into=paste0("item.no", 1:2), sep=", ") %>%
  gather("key", "id", item.no1, item.no2) %>%
  select(id, everything(), -c(key, algae)) %>%
  drop_na(id)


# clean japan food comp data  
rename_columns <- function (name_row_idx, df) { 
  japan_colnames <- unname(unlist(df[name_row_idx,]))
  colnames(df) <- tolower(make.names(japan_colnames))
  
  return(df)
}

japan_scinames = rename_columns(2, japan_scinames)
japan_nutrients = rename_columns(4, japan_nutrients)
japan_nutrients1 = rename_columns(4, japan_nutrients1)
japan_nutrients2 = rename_columns(4, japan_nutrients2)
#japan_nutrients3 = rename_columns(3, japan_nutrients3)
 

jap_nutr = japan_nutrients %>% merge(japan_nutrients1, by=c("item...no.", "water", "food.and.description"), all=T) %>%
  merge(japan_nutrients2, by.x=c("item...no.", "water", "food.and.description"), by.y=c("item.no.", "water", "food.and.description"), all=T) %>%
  select(-c(lipid.x)) %>%
  select(1, 3:6, water, everything()) 
                              

## filter japan_nutrients for kelp values 
## gather data 
jap_nutr2 <- jap_nutr %>%
  rename(id=item...no.) %>%
  gather("nutrient", "value", 6:153) %>%
  filter(id %in% japan_sciname_key$id) 

## get units key
units_key <- jap_nutr[jap_nutr$food.and.description=="Unit",] %>%
  drop_na(food.and.description) %>%
  gather(nutrient, units, 6:153)%>%
  select(nutrient, units)

## merge scientific data 
jap_data <- japan_sciname_key %>%
  merge(jap_nutr2, by="id") %>%
  merge(units_key, by="nutrient") %>%
  merge(taxa_table, by="genus") %>% # merge nutr data with taxa table to get broader groups
  mutate(nutrient=(str_replace_all(nutrient, "\\.", " ") %>% str_squish())) %>%
  select(-c(food...group, index...no.)) %>%
  rename(notes=remarks.y,
         food_desc=food.and.description) %>%
  # classify nutr types 
  mutate(algae_type=case_when(class=="phaeophyceae" ~ "brown",
                              phylum=="rhodophyta" ~ "red",
                              genus=="spirulina" ~ "blue-green")) %>%
  subset(value!="-") %>% # clean values
  mutate(value=gsub("[()]", "", value)) %>% #parentheses denote estimated value
  mutate(trace=case_when(value=="Tr" ~ TRUE, 
                         TRUE ~ FALSE),
         value=case_when(value=="Tr" ~ "0", 
                         TRUE ~ value),
         value=as.numeric(value)
  )  #indicate if trace values present



# TODO fuzzy match to nutrient list
# nutrient_key = jap_nutr %>%
#   select(nutrient) %>%
#   unique()
# 


# extract features: raw, cooked, dried, wet, etc

# get nutrients of interest (from AFCD?)

write.csv(jap_data, "./data/jap_nutr.csv")
  

