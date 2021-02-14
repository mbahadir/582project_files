library(readxl)
library(data.table)
library(glmnet)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(Information)
library(FactoMineR)
library(caTools)
library(skimr)
library(randomForest)
library(caret)
library(ranger)
library(e1071)
library(rattle)
library(stepPlr)
library(gbm)

datapath1 = 'C:/Users/ufuk9/Desktop/IE582_Fall20_ProjectTrain.xlsx'
train_data = read_excel(datapath1)
train_data = as.data.frame(train_data)

datapath2 = 'C:/Users/ufuk9/Desktop/IE582_Fall20_ProjectTest.xlsx'
test_data = read_excel(datapath2)
test_data = as.data.frame(test_data)

train_data <- train_data %>% mutate(y=ifelse(y=="a",0,1))
skim(train_data)
view(str(train_data))

pca_train <- princomp(train_data[1:(ncol(train_data)-1)])
summary(pca_train)

pca_test <- princomp(test_data)
summary(pca_test)

new_train = as.data.frame(cbind(pca_train$scores[,1:9], train_data$y))
new_train$V10 = as.factor(new_train$V10)

new_test = as.data.frame(pca_test$scores[,1:9])



