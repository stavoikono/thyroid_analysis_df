library(dplyr)
library(data.table)
library(tidyverse)
library(MLDataR)
library(GGally)
library(ggplot2)

df <- MLDataR::thyroid_disease

#df %>% is.na() %>% colSums() %>% data.frame() %>% filter(.>0)

df <- df %>%
  mutate_at(vars(patient_gender:TSH_measured,T3_measured,T4_measured,
                 thyrox_util_rate_T4U_measured,FTI_measured), function(x) as.factor(as.character(x))) %>%
  mutate(Thyroid_Class = factor(ThryroidClass),
         patient_gender = factor(case_when(patient_gender==1~"Female",
                                               patient_gender==0~"Male",
                                               TRUE~NA_character_))) %>%
  select(-ref_src,-ThryroidClass)


df %>% select(which(sapply(.,class)=="numeric"),patient_gender) %>%
  ggpairs(aes(colour = patient_gender, alpha = 0.7),
          lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))


df %>% select(TSH_reading) %>%
  ggplot(aes(x=TSH_reading)) + geom_histogram() + scale_x_log10()

