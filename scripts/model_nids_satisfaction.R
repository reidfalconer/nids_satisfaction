# METADATA ====
# Description: First attempt at a simple Life Satisfcation classifier using NIDS data.
# Created: 2018-04-03 (Reid Falconer)
# Updated: 2018-04-03 (Reid Falconer)
# Reviewed: 

# SUMMARY: This script creates an ML model which can be used to classify individuals satisfcation
# using just the NIDS data. The varibles chosen are stongly captured by the nids survey and thus contain 
# few missing values. 
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
library(forecast)

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
  mutate(income = (hhincome)) %>% 
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
  mutate(percy = (percy)) %>% 
# Food expenditure 
  mutate(food_exp = (food)) %>% 
# Non-Food expenditure 
  mutate(non_food_exp = (nonfood)) %>% 
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
# Education Squared
  mutate(education2 = as.integer(education^2)) %>% 
# Household Hed
  mutate(head_female = as.integer(ifelse((female_head == 1), 1, 0))) %>%
  mutate(head_male = as.integer(ifelse((female_head == 0), 1, 0))) %>% 
  mutate(head_yes = as.integer(ifelse((hh_head == 1), 1, 0))) %>%
  mutate(head_no = as.integer(ifelse((hh_head == 0), 1, 0))) %>% 
# Shortcut but same result with all following varibles 
 dummy_cols(select_columns = c( "area", "owns_dwelling", "watersource", "toilet", "electricity", "landline", "refuse","language", 
                                 "interview_month","home_loan", "personal_loan", "micro_loan",
                                "vehicle_finance", "stokvel", "dc", "roofs",  "walls" ,
                                "religion", "disability",  
                                "marital", "medical_aid" 
                                ), remove_first_dummy = F) %>% 
  mutate(hopefuls = as.numeric(factor(hopeful))) %>% 
  mutate(rank1s = as.numeric(factor(rank1))) %>% 
  mutate(rank2s = as.numeric(factor(rank2))) %>% 
  mutate(rank3s = as.numeric(factor(rank3))) %>% 
  mutate(rank4s = as.numeric(factor(rank4))) %>% 
  mutate(health = as.numeric(factor(health_status))) %>% 
  mutate(thefts = as.numeric(factor(theft))) %>% 
  mutate(happy = as.numeric(factor(happier)))  %>% 
  mutate(exercises = as.numeric(factor(exercise)))


# Full data set with test and training from rulof
df_full <- df_new %>% 
  select(satisfactions:exercises, shock1:shock12) 

# Data set of only training subset 
df <- df_full %>% 
  drop_na(satisfactions) 

glimpse(df)
sapply(df, class)


# TRAIN-TEST SPLIT AND CV FOLDS ====

# create and attach a numerical id (used later for some modeling)
#df <- df %>% mutate(id = row_number())

# use caret to generate a 70-30 train-test-split index
set.seed(8910)
train_index <- createDataPartition(df$satisfactions, p = 0.70, list = FALSE, times = 1)

# use index to split data into train and test sets
train <- df %>% dplyr::slice(train_index) 
test <- df %>% dplyr::slice(-train_index) 

# Define training control. 10-fold cross-validation 

cv <- createFolds(train$satisfactions, k = 10)
train_control <- trainControl(method = "cv", number =  5, verboseIter = T, allowParallel = T)

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
               objective = 'reg:linear',
               eval_metric = 'rmse',  
               eta = 0.08,
               max_depth = 10, 
               min_child_weight = 1/sqrt(0.088), 
               subsample = 0.8, 
               colsample_bytree = 1)

# preform cross-validated gboost to get the best iteration
xgboost.cv <-  xgb.cv(param = params, 
                      data = dtrain, 
                      folds = cv, 
                      nrounds = 1500, 
                      early_stopping_rounds = 100, 
                      metrics = 'rmse',
                      verbose = TRUE,
                      prediction = TRUE)

best_iteration = xgboost.cv$best_iteration #1091

# Out-of-Fold Prediction Error
# use max.col() to assign the family that has the highest probability
oof_pred <- xgboost.cv$pred %>% 
  data.frame() %>%
  mutate(satisfcation_pred = round(xgboost.cv$pred)) %>% 
  mutate(satisfcation_true = as.numeric(train$satisfactions))
head(oof_pred)

accuracy(oof_pred$satisfcation_true, oof_pred$satisfcation_pred)

#> Train model ----

# fit xgboost model on training data
mod_xgb <- xgb.train(data = dtrain, 
                     params = params,
                     nrounds = best_iteration)

# load xgb model form file path
#mod_xgb <- xgb.load("models/xgb_job_family_classifier_v0.0.0.RDS")
mod_xgb


# POSTESTIMATION ====

# Predict hold-out test set. 
# use max.col() to assign the family that has the highest probability
test_prediction <- predict(mod_xgb, newdata = dtest) %>% 
  matrix(nrow = dim(dtest)[1], byrow = T) %>% 
  data.frame() %>% 
  mutate(satisfcation_pred = round(test_prediction$.)) %>% 
  mutate(satisfcation_true = (test$satisfactions))
head(test_prediction)


# confusion matrix of test set and accuracy
accuracy(as.numeric(test_prediction$satisfcation_true), test_prediction$satisfcation_pred)
#confusionMatrix(test_prediction$satisfcation_true, test_prediction$satisfcation_pred)

#> Feature importance ----

# get the trained model.
model <- xgb.dump(mod_xgb, with_stats = TRUE)

# get the feature real names.
names <- dimnames(dtrain)[[2]]

# compute feature importance matrix
importance_matrix <- xgb.importance(names, model = mod_xgb)

# plot feature importance maxtrix for top 30 most important features
xgb.plot.importance(importance_matrix[0:50] )














