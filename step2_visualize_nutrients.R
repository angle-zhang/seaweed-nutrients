
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)

algae_dri <- read.csv('./data/jap_algae_dri.csv')
food_dri <- read.csv('./data/jap_food_dri.csv')
#just nutrient vals
food_nutr <- read.csv('./data/jap_food.csv')
algae_nutr <- read.csv('./data/jap_algae.csv')

ref_summary <- read.csv('./data/ref_summary.csv') 

# get median food dri values (w/std dev)
food_dri_med = food_dri %>%
  group_by(nutrient, food_group, dri_type) %>%
  summarize(median=median(proportion), 
            sd=sd(proportion))

algae_dri_med = algae_dri %>%
  group_by(nutrient, food_group, dri_type, algae_type, genus, raw, dried) %>%
  summarize(median=median(proportion), 
            sd=sd(proportion)) 

# all seaweeds 
algae_dri %>%
  filter(dri_type=="AI") %>%
  ggplot(aes(x=nutrient, y=nutrient_value, fill=nutrient)) +
  geom_boxplot() +
  scale_y_log10()

# DRi of seaweeds 
algae_dri %>%
  filter(dri_type=="AI", summary_type=="mean_ref") %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=reorder(nutrient, proportion))) +
  geom_point(aes(color=reorder(nutrient, proportion)), alpha=.5)+
  geom_boxplot(aes(alpha=.2)) +
  geom_hline(yintercept=1, color="red") + 
  theme_light() +
  scale_y_log10() + 
  # facet_wrap(~dried, scales="free")+
  coord_flip() 

# AI by seaweed type
algae_dri %>%
  filter(dri_type=="AI", summary_type=="mean_ref") %>%
  drop_na(algae_type) %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=algae_type)) +
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip()

# AI by raw
algae_dri %>%
  filter(dri_type=="AI", summary_type=="mean_ref") %>%
  drop_na(algae_type) %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=raw)) +
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip()

# AI by dried
algae_dri %>%
  drop_na(dried) %>%
  filter(dri_type=="AI", summary_type=="mean_ref") %>%
  drop_na(algae_type) %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=dried)) +
  geom_point(aes(color=dried), alpha=.5)+
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip()

# get mean
algae_nutr_mean = algae_nutr %>%
  group_by(nutrient, dried) %>%
  summarize(mean_value=mean(value)) %>%
  mutate(order=(case_when(nutrient=="Water" ~ 100000, TRUE ~ mean_value)))

algae_nutr_mean %>%
  drop_na(dried) %>%
  ggplot(aes(x=reorder(nutrient, order), y=mean_value, fill=dried)) + 
  geom_bar(position="fill", stat="identity")+ 
  coord_flip()

# algae_ref = algae_nutr %>%
#   rename(nutrient_value=value) %>%
#   merge(ref_summary, by=c("nutrient", "units"), all=T)
# 
# algae_ref %>%
#   filter(summary_type=="mean_ref", dri_type=="AI") %>%
#   drop_na(dried) %>%
#   group_by(nutrient) %>%
#   mutate(nutrient_id=cur_group_id()) %>%
#   ungroup() %>%
#   ggplot(aes(x=reorder(nutrient, nutrient_value), y=nutrient_value, fill=dried)) +
#   geom_point(aes(color=dried), alpha=.5)+
#   geom_boxplot() +
#   geom_hline(aes(yintercept=value)) + 
#  # geom_point(aes(nutrient, value), shape=108, size=20) +
#  # geom_segment(aes(x=reorder(nutrient_id, nutrient_value), xend=reorder(nutrient_id-.25, nutrient_value), y=value, yend=value)) +
#   scale_y_log10() +
#   facet_wrap(~nutrient, scales="free", nrow=1) 
  


# UL by seaweed type
algae_dri %>%
  filter(dri_type=="UL", summary_type=="mean_ref") %>%
  drop_na(algae_type) %>%
  ggplot(aes(x=reorder(nutrient, proportion), y=proportion, fill=nutrient)) +
  geom_boxplot() +
  geom_hline(yintercept=1, color="red") + 
  scale_y_log10() + 
  coord_flip() + 
  facet_wrap(~algae_type)


## heatmap
food_dri_med %>%
  filter(dri_type=="AI") %>%
  ggplot(aes(x=reorder(nutrient, median), y=food_group, fill=median)) + 
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "red", breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), limits=c(0, 1)) + 
  coord_flip() + 
  face

algae_dri_med %>%
  filter(dri_type=="AI") %>%
  ggplot(aes(x=reorder(nutrient, median), y=genus, fill=median)) + 
  geom_tile(colour = "white", height=1, width=1) +
  scale_fill_gradient(low = "white", high = "red", breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), limits=c(0, 1)) +
  facet_grid(.~algae_type, scales="free", space="free") + 
  coord_flip()

algae_dri_med %>%
  filter(dri_type=="AI") %>%
  ggplot(aes(x=reorder(nutrient, median), y=algae_type, fill=median)) + 
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "red", breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), limits=c(0, 1))+
  coord_flip()

algae_dri_med %>%
  filter(dri_type=="AI") %>%
  ggplot(aes(x=reorder(nutrient, median), y=dried, fill=median)) + 
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "red", breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), limits=c(0, 1))+
  coord_flip()



# all seaweeds by type (green, red, brown)


# all raw seaweds

