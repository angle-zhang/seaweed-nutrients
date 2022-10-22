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

#DRI and nutr data
nutr_key <- read_excel('./data-raw/nutr_key.xlsx') 
dri_data <- read.csv('./data-raw/dri_nutrients.csv')
nutr_key <- nutr_key %>% select(1,2,10) 

taxa_table <- readRDS("data-raw/taxa_table.Rds")

# clean japan food comp data  
rename_columns <- function (name_row_idx, df) { 
  japan_colnames <- unname(unlist(df[name_row_idx,]))
  colnames(df) <- tolower(make.names(japan_colnames))
  
  return(df)
}

# col names
names(nutr_key) <- c('nutrient', "japan.nutrient", "ph_ranking")
japan_scinames = rename_columns(2, japan_scinames)
japan_nutrients = rename_columns(4, japan_nutrients)
japan_nutrients1 = rename_columns(4, japan_nutrients1)
japan_nutrients2 = rename_columns(4, japan_nutrients2)

### helper functions & nutrient key 
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

convert_units  <- Vectorize(convert_units)

## get units and nutrients from DRI 
nutr_unit_key = dri_data %>%
  select(nutrient, nutrient_units) %>%
  filter(!(nutrient=="Protein" & nutrient_units=="g/kg")) %>%
  rename(unit_conversion=nutrient_units) %>%
  unique() 

## OVERALL DIETARY DATA
# merge other data sources (other nturients)
jap_nutr = japan_nutrients %>% merge(japan_nutrients1, by=c("item...no.", "water", "food.and.description"), all=T) %>%
  merge(japan_nutrients2, by.x=c("item...no.", "water", "food.and.description"), by.y=c("item.no.", "water", "food.and.description"), all=T) %>%
  select(-c(lipid.x)) %>%
  select(1, 3:6, water, everything()) %>%
  rename(id=item...no.,
         food_desc=food.and.description,
         food_group=food...group) %>%
  gather("nutrient", "value", 6:65, 67:153) %>%
  mutate(food_group=as.numeric(food_group)) %>%
  mutate(food_group=recode(food_group,
                           `1`='Cereals',
                           `2`='Potatoes and starches', 
                           `4`='Pulses',
                           `5`='Nuts and seeds', 
                           `6`='Vegetables', 
                           `8`='Mushroom',
                           `9`='Algae',
                           `10`='Fish, mollusks, and crustacean',
                           `11`="Meat",
                           `12`="Eggs"))

## get units key
units_key <- jap_nutr[jap_nutr$food_desc=="Unit",] %>%
  drop_na(food_desc) %>%
  mutate(nutrient=(str_replace_all(nutrient, "\\.", " ") %>% str_squish())) %>%
  merge(nutr_key, by.x="nutrient", by.y="japan.nutrient") %>% 
  select(-nutrient) %>%
  rename(nutrient=nutrient.y) %>%
  rename(units=value) %>%
  mutate(units=gsub('\\/100 g', "", units)) %>%
  mutate(units=gsub('\\/100 ｇ', "", units)) %>%
  select(nutrient, units) 

jap_nutr1 = jap_nutr %>%
  mutate(nutrient=(str_replace_all(nutrient, "\\.", " ") %>% str_squish())) %>%
  #merge(nutr_key, by.x="nutrient", by.y="japan.nutrient") %>%
  select(-index...no.) %>%
  rename(notes=remarks.y) %>%
  #select(-nutrient) %>%
  #rename(nutrient=nutrient.y) %>%
  subset(value!="-") %>% # clean values
  mutate(value=gsub("[()]", "", value)) %>% #parentheses denote estimated value
  mutate(trace=case_when(value=="Tr" ~ TRUE,
                         TRUE ~ FALSE),
         value=case_when(value=="Tr" ~ "0",
                         TRUE ~ value),
         value=as.numeric(value)) %>% #indicate if trace values present
  # combine nutrients (make sure unites are the same)
  merge(units_key, by="nutrient") %>%
  spread(nutrient, value) %>%
  #mutate(`Marine-source omega-3s`=DHA+EPA) %>%
  #select(-DHA, -EPA) %>%
  gather("nutrient", "value", 9:153) %>%
  select(id, nutrient, everything()) %>%
  drop_na(food_group, value)

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

jap_algae <- jap_nutr1 %>%
  filter(food_group=="Algae") %>%
  # merge by common name instead
  mutate(common_name1=tolower(word(food_desc, 2, sep=", "))) %>%
  mutate(common_name1=(str_replace_all(common_name1, "\"", " ") %>% str_squish())) %>%
  merge(japan_sciname_key, by="common_name1") %>%
  # classify nutr types
  # TODO add green algae
  mutate(algae_type=case_when(class=="phaeophyceae" ~ "brown",
                              phylum=="rhodophyta" ~ "red",
                              genus=="spirulina" ~ "blue-green")) %>%
  (common_name1!="tengusa") %>% # filter out agar agar product
  rename(id=id.x, common_name=common_name1, sciname=scientific_name1) %>%
  select(-scientific_name2, -common_name2) %>%
  select(-id.y) %>%
  select(id, nutrient, everything()) #%>%
  mutate(raw=case_when(str_detect(food_desc, "raw") ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(dried=case_when(str_detect(food_desc, "rehydrated and boiled|soaked in water|blanched and salted") ~ FALSE,
    str_detect(food_desc, "boiled and dried|steamed and dried") ~ TRUE,
    str_detect(food_desc, "dried") ~ TRUE))

jap_algae_afcd = jap_nutr1 %>% 
  # merge by common name instead 
  mutate(common_name1=tolower(word(food_desc, 2, sep=", "))) %>%
  mutate(common_name1=(str_replace_all(common_name1, "\"", " ") %>% str_squish())) %>%
  merge(japan_sciname_key, by="common_name1") %>%
  # classify nutr types 
  # TODO add green algae
  mutate(algae_type=case_when(class=="phaeophyceae" ~ "brown",
                              phylum=="rhodophyta" ~ "red",
                              genus=="spirulina" ~ "blue-green")) %>%
  (common_name1!="tengusa") %>% # filter out agar agar product
  rename(id=id.x, common_name=common_name1, sciname=scientific_name1) %>%
  select(-scientific_name2, -common_name2) %>%
  select(-id.y, -id) %>%
  select(sciname, kingdom, phylum, class, order, family, genus, common_name, everything()) 

write.csv(jap_algae_afcd, "./data/jap_algae_fct.csv")
write.csv(units_key, "./data/jap_algae_unit_key.csv")


#inspect algae names 
jap_algae %>% select(food_desc) %>%
  unique()


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


jap_dri1 = jap_nutr1 %>% 
  merge(nutr_unit_key) %>%
  mutate(value=convert_units(value, units, unit_conversion)) %>%
  mutate(units_orig=units, 
         units=unit_conversion) %>%
  select(-unit_conversion) %>% 
  group_by(nutrient) %>%
  mutate(n = n()) %>%
  merge(ref_ul_summary, by=c("nutrient", "units")) %>%
  mutate(ID = row_number(), 
         proportion = value.x/value.y) %>%
  rename(nutrient_value=value.x, dri_value=value.y) %>%
  select(ID, everything())

## filter japan nutrients for all foods values
jap_food_dri <- jap_dri1 %>%
  drop_na(food_group)

## merge scientific data 
jap_algae_dri <- jap_dri1 %>%
  merge(jap_algae %>% select(id, common_name1, 11:22), by="id") 



# TODO fuzzy match to nutrient list
# nutrient_key = jap_nutr %>%
#   select(nutrient) %>%
#   unique()



# extract features: raw, cooked, dried, wet, etc

# get nutrients of interest (from AFCD?)

write.csv(jap_algae_dri, "./data/jap_algae_dri.csv")
write.csv(jap_food_dri, "./data/jap_food_dri.csv")
write.csv(jap_nutr1, "./data/jap_food.csv")
write.csv(jap_algae, "./data/jap_algae.csv")

write.csv(ref_ul_summary, "./data/ref_summary.csv") 

