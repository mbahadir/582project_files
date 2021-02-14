setwd("C:/Users/fatma/Desktop/IE582/project")
library(caret)
library(Information)
library(glmnet)
library(binaryLogic)
data<-read.csv("IE582_Fall20_ProjectTrain.csv")
TestData<-read.csv("IE582_Fall20_ProjectTest.csv")
str(data)

nonbinary<-c()
binary<-c()

for(i in 1:61){
  if(is.binary(data[,i])==FALSE){
    if(length(unique(data[,i]))>1)
      nonbinary<-c(nonbinary,i)
    
  }
  else
    if(length(unique(data[,i]))==2)
      binary<-c(binary,i)
}
length(binary)+length(nonbinary)

data<-data[,-c(50,52)]


for (i in 1:58){
  hist(data[,i])
}

str(data)
####################Test Data

TestData<-TestData[,-c(50,52)]

imbalanced<-c(13,15,16,18,19,20,21,22,24,25,26,28,29,31,33,34,35,36,37,38,39,40,42,43,45,46,47,48,49,50,51,53,55,56,57,58)

TestData<-TestData[,-imbalanced]
str(TestData)


##Split

#Splitting
set.seed(570)
spl=sample.split(data$y, SplitRatio = 0.8)
Train=subset(data,spl==TRUE)
Test=subset(data,spl==FALSE)
str(downTrain)

downTrain<-downSample(x=Train[,1:58],y=as.factor(Train$y),yname = "y")

upTrain<-upSample(x=Train[,1:58],y=as.factor(Train$y),yname = "y")

###################sgb

fitcontrol =trainControl(method="cv", number=3, search="grid")

GBMtuneGrid=expand.grid(interaction.depth = c(2, 4, 6), 
                        shrinkage = c(0.05,0.1, 0.15),
                        n.trees = c(100, 150, 250),
                        n.minobsinnode = 15)

GBMmodel <- train(as.factor(y) ~ ., 
                  data=as.matrix(Train), 
                  method="gbm", 
                  metric="Accuracy", 
                  trControl=fitcontrol,
                  tuneGrid = GBMtuneGrid,
)
max(GBMmodel$results$Accuracy)
Predicted_GBM2<-predict(GBMmodel,Test)

TAcc_GBM_BreastC<-max(GBMmodel$results)

CM_GBM<-table(as.factor(Test$y),Predicted_GBM2)
Acc_GBM <- sum(diag(CM_GBM)) / sum(CM_GBM)





###################sgb with up-sampling

fitcontrol =trainControl(method="cv", number=3, search="grid")

GBMtuneGrid=expand.grid(interaction.depth = c(2, 4, 6), 
                        shrinkage = c(0.05,0.1, 0.15),
                        n.trees = c(100, 150, 250),
                        n.minobsinnode = 15)

GBMmodel <- train(as.factor(y) ~ ., 
                  data=upTrain, 
                  method="gbm", 
                  metric="Accuracy", 
                  trControl=fitcontrol,
                  tuneGrid = GBMtuneGrid,
)
max(GBMmodel$results$Accuracy)
Predicted_GBM2<-predict(GBMmodel,Test)

TAcc_GBM_BreastC<-max(GBMmodel$results)

CM_GBM<-table(as.factor(Test$y),Predicted_GBM2)
Acc_GBM <- sum(diag(CM_GBM)) / sum(CM_GBM)



###################sgb with down-sampling

fitcontrol =trainControl(method="cv", number=3, search="grid")

GBMtuneGrid=expand.grid(interaction.depth = c(2, 4, 6), 
                        shrinkage = c(0.05,0.1, 0.15),
                        n.trees = c(100, 150, 250),
                        n.minobsinnode = 15)

GBMmodel <- train(as.factor(y) ~ ., 
                  data=downTrain, 
                  method="gbm", 
                  metric="Accuracy", 
                  trControl=fitcontrol,
                  tuneGrid = GBMtuneGrid,
)
max(GBMmodel$results$Accuracy)
Predicted_GBM2<-predict(GBMmodel,Test)

TAcc_GBM_BreastC<-max(GBMmodel$results)

CM_GBM<-table(as.factor(Test$y),Predicted_GBM2)
Acc_GBM <- sum(diag(CM_GBM)) / sum(CM_GBM)

