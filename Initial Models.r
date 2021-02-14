library(skimr)
library(ggplot2)
library(GGally, quietly=TRUE)

library(data.table)

dt=fread("IE582_Fall20_ProjectTrain.csv")
final_test=fread("IE582_Fall20_ProjectTest.csv")

library(corrplot)
ggcorr(data.frame(dt[,0:60]), method = c("everything", "pearson")) 

dt$y_num=as.numeric(as.factor(dt$y))
dt[,y:=NULL]
#str(dt)
#head(dt)

library(caTools)

set.seed(35)
spl=sample.split(dt$y, SplitRatio = 0.8)
train=subset(dt,spl==TRUE)
test=subset(dt,spl==FALSE)

library(rpart)
library(rattle)

cla_tree=rpart(y_num~.,train,method='class',minbucket=100)
fancyRpartPlot(cla_tree)
cla_tree$variable.importance

PredictCART=predict(cla_tree,newdata=test, type="class")

res_tree=table(test$y,PredictCART)
res_tree

library(caret)
library(e1071)

confusionMatrix(data = PredictCART, reference = as.factor(test$y_num), mode = "prec_recall")

confusionMatrix(data = PredictCART, reference = as.factor(test$y_num), mode = "sens_spec")

perf_dt=function(type,actual,forecast){
    name=type
    n=length(actual)
    error=actual-forecast
    mean=mean(actual)
    sd=sd(actual)
    FBias=sum(error)/sum(actual)
    MPE=sum(error/actual)/n
    MAPE=sum(abs(error/actual))/n
    RMSE=sqrt(sum(error^2))/n
    MAD=sum(abs(error))/n
    WMAPE=MAD/mean
    l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
    return(l)
}

perf_dt("Decision Tree",test$y_num,as.numeric(PredictCART))

library(randomForest)

random_forest=randomForest(as.factor(y_num)~.,data=train,ntree=200,nodesize=20)
random_forest

varImpPlot(random_forest)

PredictRandomForest=predict(random_forest,newdata=test)
table(test$y_num,PredictRandomForest)

confusionMatrix(data = PredictRandomForest, reference = as.factor(test$y_num), mode = "prec_recall")

perf_dt("Decision Tree",test$y_num,as.numeric(PredictCART))
perf_dt("Random Forest",test$y_num,as.numeric(PredictRandomForest))

set.seed(35)

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:300)*0.0001)
tr=train(as.factor(y_num)~.,
      data=train, 
      method="rpart",
      trControl=numFolds,
      tuneGrid= cpGrid)
tr

best_tree=tr$finalModel
fancyRpartPlot(best_tree)
best_tree$variable.importance

prediction_best_cv=predict(best_tree,newdata=test,type="class")
table(test$y_num,prediction_best_cv)

confusionMatrix(data = prediction_best_cv, reference = as.factor(test$y_num), mode = "prec_recall")

perf_dt("Decision Tree",test$y_num,as.numeric(PredictCART))
perf_dt("Random Forest",test$y_num,as.numeric(PredictRandomForest))
perf_dt("Decision Tree with CV",test$y_num,as.numeric(prediction_best_cv))

res_tree

PenaltyMatrix = matrix(c(0,1.5,2,0), byrow=TRUE, nrow=2)
PenaltyMatrix

DecTree_pen = rpart(as.factor(y_num) ~ .,
                   data=train, method="class", parms=list(loss=PenaltyMatrix),
                   cp=0.01)
fancyRpartPlot(DecTree_pen)
DecTree_pen$variable.importance

penalized_tree=predict(DecTree_pen,newdata=test,type="class")
table(test$y_num,penalized_tree)

confusionMatrix(data = penalized_tree, reference = as.factor(test$y_num), mode = "prec_recall")

perf_dt("Decision Tree",test$y_num,as.numeric(PredictCART))
perf_dt("Random Forest",test$y_num,as.numeric(PredictRandomForest))
perf_dt("Tree with CV",test$y_num,as.numeric(prediction_best_cv))
perf_dt("Penalty Matrix",test$y_num,as.numeric(penalized_tree))




