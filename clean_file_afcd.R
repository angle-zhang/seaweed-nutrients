library(readr)
library(tidyverse)
library(dplyr)
library(readxl)


load("data-raw/AFCD/afcd_sci.rda")

#japan data 
japan_scinames <- read_excel("data-raw/Japan/Japan_foodcomp_scinames.xlsx")
japan_nutrients <- read_excel("data-raw/Japan/Japan_foodcomp_values.xlsx")
japan_nutrients1 <- read_excel("./data-raw/Japan/Japan_aminoacid_values.xlsx")[,-c(1,3)]
japan_nutrients2 <- read_excel("./data-raw/Japan/Japan_fattyacid_values.xlsx")[,-c(1,3)]

taxa_table <- readRDS("data-raw/AFCD/taxa_table.Rds")

# clean japan food comp data  
rename_columns <- function (name_row_idx, df) { 
  japan_colnames <- unname(unlist(df[name_row_idx,]))
  colnames(df) <- tolower(make.names(japan_colnames))
  
  return(df)
}

# col names
japan_scinames = rename_columns(2, japan_scinames)
japan_nutrients = rename_columns(4, japan_nutrients)
japan_nutrients1 = rename_columns(4, japan_nutrients1)
japan_nutrients2 = rename_columns(4, japan_nutrients2)

jap_nutr = japan_nutrients %>% merge(japan_nutrients1, by=c("item...no.", "water", "food.and.description"), all=T) %>%
  merge(japan_nutrients2, by.x=c("item...no.", "water", "food.and.description"), by.y=c("item.no.", "water", "food.and.description"), all=T) %>%
  select(-c(lipid.x)) %>%
  select(1, 3:6, water, everything()) %>%
  rename(id=item...no.,
         food_desc=food.and.description,
         food_group=food...group) %>%
  gather("nutrient", "value", 6:65, 67:153) %>%
  mutate(food_group=as.numeric(food_group)) 

units_key <- jap_nutr[jap_nutr$food_desc=="Unit",] %>%
  drop_na(food_desc) %>%
  mutate(nutrient=(str_replace_all(nutrient, "\\.", " ") %>% str_squish())) %>%
  mutate(units=gsub('\\/100 g', "", units)) %>%
  mutate(units=gsub('\\/100 ｇ', "", units)) %>%
  select(nutrient, units) 

jap_nutr1 = jap_nutr %>%
  filter(food_group==9) %>%
  mutate(nutrient=(str_replace_all(nutrient, "\\.", " ") %>% str_squish())) %>%
  #merge(nutr_key, by.x="nutrient", by.y="japan.nutrient") %>% 
  select(-index...no.) %>%
  rename(notes=remarks.y) %>%
  #select(-nutrient) %>%
  #rename(nutrient=nutrient.y) %>%
  subset(value!="-") %>% # clean values
  mutate(value=gsub("[()]", "", value)) %>% #parentheses denote estimated value
  # mutate(trace=case_when(value=="Tr" ~ TRUE, 
  #                        TRUE ~ FALSE),
  #        value=case_when(value=="Tr" ~ "0", 
  #                        TRUE ~ value),
  #        value=as.numeric(value)) %>% #indicate if trace values present
  # combine nutrients (make sure unites are the same)
  unique() %>%
  #(units_key, by="nutrient") %>%
  spread(nutrient, value) 

## ALGAE DATA
#filter out algae & get same genuses from AFCD 
japan_sciname_key <- japan_scinames %>%
  filter(grepl("Algae", english.name.of.food.source)) %>%
  drop_na(scientific.name) %>%
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
  merge(taxa_table, by="genus") %>% # merge nutr data with taxa table to get broader groups
  drop_na(id) 

jap_algae_afcd = jap_nutr1 %>% 
  # merge by common name instead 
  mutate(common_name1=tolower(word(food_desc, 2, sep=", "))) %>%
  mutate(common_name1=(str_replace_all(common_name1, "\"", " ") %>% str_squish())) %>%
  merge(japan_sciname_key, by="common_name1") %>%
  rename(id=id.x, common_name=common_name1, sciname=scientific_name1) %>%
  select(-scientific_name2, -common_name2) %>%
  select(-id.y, -id) %>%
  select(sciname, kingdom, phylum, class, order, family, genus, common_name, everything()) 

write.csv(jap_algae_afcd, "./data/jap_algae_fct.csv")
write.csv(units_key, "./data/jap_algae_unit_key.csv")
