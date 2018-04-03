# METADATA ====
# Description: First attempt at a simple Life Satisfcation classifier using NIDS data.
# Created: 2018-04-03 (Reid Falconer)
# Updated: 2018-04-03 (Reid Falconer)
# Reviewed: 

# SUMMARY: This script creates an ML model which can be used to classify individuals satisfcation
# using just the NIDS data.
# This is purely a trial run, and it may be the case that alternative ML algorithms and/or differnt 
# data processing techniques will yeild more accurate results. 


# INITIALISE ====

rm(list=ls())

#> Libraries ----
library(caret) #train-test splitting
library(tidyverse) #load tidyverse last to avoid namespace conflicts
library(readstata13)
library(dplyr)
library(fastDummies)
library(xgboost)

#> Set options ----

# disable scientific notation
options(scipen = 999)


# LOAD DATA & WRANGLE ====

#read data into df 
df <- read.dta13("data/raw/nids_data.dta")

# source the cleaning code from `text_cleaning.R` script in order to keep
# the code clean and allow one to edit the code freely.
# referencing the script here
source("scripts/text_cleaning.R") 

# create new df for modelling %>% convert all factors to dummies 
df_new <- df %>% 
  mutate(satisfactions = factor(satisfaction)) %>% 
  mutate(satisfactions = as.numeric(satisfactions)) %>% 
  mutate(income = log(hhincome)) %>% 
  mutate(members = hhsize) %>% 
  mutate(room = rooms) %>% 
  mutate(education = as.integer(educ)) %>% 
  mutate(childrens = as.integer(children)) %>% 
  mutate(adult = as.integer(adults)) %>% 
  mutate(elder = as.integer(elders)) %>% 
# Weight, height and BMI
  mutate(weight_kg = weight) %>% 
  mutate(height_m = height/100) %>% 
  mutate(bmi = weight_kg/(height_m^2)) %>% 
# Per capita income 
  mutate(lpercy = log(percy)) %>% 
# Food expenditure 
  mutate(food_exp = log(food)) %>% 
# Non-Food expenditure 
  mutate(non_food_exp = log(nonfood)) %>% 
# Medical Aid medical_aid
  mutate(medical_aid_yes = as.integer(ifelse((medical_aid == 1), 1, 0))) %>%
  mutate(medical_aid_no = as.integer(ifelse((medical_aid == 0), 1, 0))) %>% 
# Dwellings
  mutate(dwelling_na = as.integer(ifelse((dwellingtype == "Missing"), 1, 0))) %>% 
  mutate(dwelling_house = as.integer(ifelse((dwellingtype == "Dwelling/house or brick structure on a separate stand or yar"), 1, 0))) %>% 
  mutate(dwelling_hut = as.integer(ifelse((dwellingtype == "Traditional dwelling/hut/structure made of traditional mater"), 1, 0))) %>% 
  mutate(dwelling_flat = as.integer(ifelse((dwellingtype == "Flat or apartment in a block of flats"), 1, 0))) %>% 
  mutate(dwelling_semidetached = as.integer(ifelse((dwellingtype == "Town/cluster/semi-detached house"), 1, 0))) %>% 
  mutate(dwelling_backyard.house = as.integer(ifelse((dwellingtype == "Dwelling/house/flat/room in backyard"), 1, 0))) %>% 
  mutate(dwelling_shack = as.integer(ifelse((dwellingtype == "Informal dwelling/shack not in backyard"), 1, 0))) %>% 
  mutate(dwelling_backyard.shack = as.integer(ifelse((dwellingtype == "Informal dwelling/shack in backyard"), 1, 0))) %>% 
  mutate(dwelling_flatlet = as.integer(ifelse((dwellingtype == "Room/flatlet"), 1, 0))) %>% 
  mutate(dwelling_caravan = as.integer(ifelse((dwellingtype == "Caravan/tent"), 1, 0))) %>% 
  mutate(dwelling_retirement = as.integer(ifelse((dwellingtype == "Unit in retirement village"), 1, 0))) %>% 
# Provinces 
  mutate(prov_Western_Cape = as.integer(ifelse((province == "Western Cape"), 1, 0))) %>% 
  mutate(prov_Northern_Cape = as.integer(ifelse((province == "Northern Cape"), 1, 0))) %>% 
  mutate(prov_Eastern_Cape = as.integer(ifelse((province == "Eastern Cape"), 1, 0))) %>% 
  mutate(prov_Free_State = as.integer(ifelse((province == "Free State"), 1, 0))) %>% 
  mutate(prov_KwaZulu_Natal = as.integer(ifelse((province == "KwaZulu-Natal"), 1, 0))) %>% 
  mutate(prov_North_West = as.integer(ifelse((province == "North West"), 1, 0))) %>% 
  mutate(prov_Gauteng = as.integer(ifelse((province == "Gauteng"), 1, 0))) %>% 
  mutate(prov_Mpumalanga = as.integer(ifelse((province == "Mpumalanga"), 1, 0))) %>% 
  mutate(prov_Limpopo = as.integer(ifelse((province == "Limpopo"), 1, 0))) %>% 
# Race 
  mutate(race_african = as.integer(ifelse((race == "African"), 1, 0))) %>% 
  mutate(race_coloured = as.integer(ifelse((race == "Coloured"), 1, 0))) %>% 
  mutate(race_indian = as.integer(ifelse((race == "Asian/Indian"), 1, 0))) %>% 
  mutate(race_white = as.integer(ifelse((race == "White"), 1, 0))) %>%  
# Age Squared
  mutate(age2 = as.integer(age^2)) %>% 
# Household Hed
  mutate(head_female = as.integer(ifelse((female_head == 1), 1, 0))) %>%
  mutate(head_male = as.integer(ifelse((female_head == 0), 1, 0))) %>% 
  mutate(head_yes = as.integer(ifelse((hh_head == 1), 1, 0))) %>%
  mutate(head_no = as.integer(ifelse((hh_head == 0), 1, 0))) %>% 
# Shortcut but same result with all following varibles 
 dummy_cols(select_columns = c("owns_dwelling", "happier", "home_loan", "personal_loan", "micro_loan", 
                                "vehicle_finance", "theft", "stokvel", "exercise", "watersource", "toilet", "electricity",
                                "landline", "refuse", "dc", "area", "birthmonth", "hopeful", "religion", "health_status", 
                                "disability", "roofs", "interview_month","marital", "walls"), remove_first_dummy = F)

df_full <- df_new %>% 
  select(satisfactions:walls_Plastic,shock1:shock12) 

df <- df_full %>% 
  drop_na(satisfactions)

# the XGBoost algorithm requires that the class labels (job family) start at 0 and increase sequentially 
# to the maximum number of classes. This is a bit of an inconvenience as you need to keep track of what job 
# family name goes with which label. Also, you need to be very careful when you add or remove a 1 to go from 
# the zero based labels to the 1 based labels. Outcome:
# 0 - ana, 1 - ass, 2 - cre, 3 - dri, 4 - ele, 5 - pro, 6 - sss
df <- df %>% mutate(satisfactions = satisfactions - 1)

glimpse(df)

# TRAIN-TEST SPLIT AND CV FOLDS ====

# create and attach a numerical id 
df <- df %>% mutate(id = row_number())

# use caret to generate a 70-30 train-test-split index
set.seed(8910)
train_index <- createDataPartition(df$satisfactions, p = 0.70, list = FALSE, times = 1)

# use index to split data into train and test sets
train <- df %>% slice(train_index) 
test <- df %>% slice(-train_index) 

# Define training control. 10-fold cross-validation 
#train_control <- trainControl(method = "cv", number = 10)
cv <- createFolds(train$satisfactions, k = 10)


# MODELLING ====

# prepare the train and test matrices for xgboost...
dtrain <- xgb.DMatrix(data = train %>% select(-satisfactions) %>% data.matrix(), 
                      label = train %>% pull(satisfactions),
                      missing = NA)
dtest <- xgb.DMatrix(data = test %>% select(-satisfactions) %>% data.matrix(), 
                     label = test %>% pull(satisfactions),
                     missing = NA)

# set xgboost parameters
params <- list(booster = 'gbtree', 
               objective = 'multi:softprob',
               num_class = 10,
               eval_metric = 'merror',  
               eta = 0.05,
               max_depth = 10, 
               min_child_weight = 1/sqrt(0.088), 
               subsample = 0.5, 
               colsample_bytree = 1)

# preform cross-validated gboost to get the best iteration
xgboost.cv <-  xgb.cv(param = params, 
                      data = dtrain, 
                      folds = cv, 
                      nrounds = 500, 
                      early_stopping_rounds = 100, 
                      metrics = 'merror',
                      verbose = TRUE,
                      prediction = TRUE)

best_iteration = xgboost.cv$best_iteration

# Out-of-Fold Prediction Error
# use max.col() to assign the family that has the highest probability
oof_pred <- xgboost_cv$pred %>% 
  data.frame() %>%
  mutate(max_prob = max.col(., ties.method = 'last') %>% 
           factor(labels = class_levels),
         label = (train$satisfactions + 1) 
  )
head(oof_pred)

# Out-of-Fold confusion matrix
confusionMatrix(oof_pred$label,oof_pred$max_prob)


