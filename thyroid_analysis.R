ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr","data.table","ggplot2","missForest", "MLDataR","tidymodels",
              "skimr","themis","stacks","doParallel","knitr","GGally","naniar")

ipak(packages)

df <- MLDataR::thyroid_disease

#df %>% is.na() %>% colSums() %>% data.frame() %>% filter(.>0)



df <- df %>%
  select(-TSH_measured,-T3_measured,-T4_measured,-thyrox_util_rate_T4U_measured,
         -FTI_measured,-ref_src) %>%
  mutate_at(vars(patient_gender:psych_condition), function(x) as.factor(as.character(x))) %>%
  mutate(Thyroid_Class = factor(ThryroidClass),
         patient_gender = factor(case_when(patient_gender==1~"Female",
                                               patient_gender==0~"Male",
                                               TRUE~NA_character_))) %>%
  select(-ThryroidClass)

skim(df)



ggplot(df, aes(x=Thyroid_Class, fill=Thyroid_Class)) + geom_bar() + theme_bw() +
  xlab("Theroid Class") + ggtitle("Thyroid Class distribution")

df %>% select(which(sapply(.,class)=="numeric"),Thyroid_Class) %>%
  ggpairs(aes(colour = Thyroid_Class, alpha = 0.7),
          lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))

ggpairs(df)


df %>% select(TSH_reading) %>%
  ggplot(aes(x=TSH_reading)) + geom_histogram() + scale_x_log10()

df %>% gg_miss_upset()


set.seed(1234)
df_split <- initial_split(data = df,prop = 0.8 ,strata = Thyroid_Class)

df_train <- training(df_split)
df_test <- testing(df_split)

train_recipe <- 
  recipe(Thyroid_Class ~ . ,data = df_train) %>%
  step_impute_bag(all_predictors())
  step_downsample(Thyroid_Class) %>%
  step_nzv(all_predictors()) %>%
  step_log(TSH_reading) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep()
  
juice(train_recipe)

test_proc <- bake(object = train_recipe,new_data = df_test)


### bootstrap