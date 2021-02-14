library(data.table)

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

data_train=fread("IE582_Fall20_ProjectTrain.csv")
data_train$y=as.factor(data_train$y)

levels(data_train$y)=c("No","Yes")

library(caTools)

spl=sample.split(data_train$y, SplitRatio = 0.8)
train=subset(data_train,spl==TRUE)
test=subset(data_train,spl==FALSE)
str(train)

library(xlsx)
library(caret)
library(e1071)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(Information)
library(FactoMineR)
library(caTools)
library(ROCR)
library(pROC)

library(yardstick)
library(mlbench)
library(cvAUC)
library(forecast)

fourStats <- function (data, lev = levels(data$obs), model = NULL) { 
      auc_val=AUC(data$Yes, data$obs)
      
      out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
      coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
                   ncol = 2, 
                   byrow = TRUE)
      #print(out)  
  #print(BER(data$obs, data$pred))
      colnames(coords) <- c("Spec", "Sens")
      rownames(coords) <- c("Best", "Current") 
      a=as.numeric((auc_val+(coords[2]+coords[4])/2)/2)+coords[2]/10
      c(AUC_value=auc_val, Prime=a, Spec=coords[4], Sens=coords[2])
}

rf_grid=expand.grid(mtry=c(8,10,12,15,20),
                   splitrule = c("extratrees","gini","hellinger"),
                   min.node.size= c(5,10,15,20,25))

gbmGrid=expand.grid(interaction.depth = c(1,3,5,8), 
                        n.trees = (1:6)*50, 
                        shrinkage = c(0.1, 0.05, 0.01),
                        n.minobsinnode = c(5,10,15))

xgbGrid=expand.grid(max_depth = c(3, 5), 
                        nrounds = (2:4)*50, 
                        eta = c(0.1, 0.3),
                        min_child_weight=c(1,5),
                        gamma=c( 1, 1.5, 2),
                        colsample_bytree=c(0.8),
                        subsample=c(0.8))

fitControl_random=trainControl(method = "repeatedcv",
                               number = 10, repeats=4, summaryFunction = fourStats,
                              classProbs = T, 
                              verboseIter=FALSE)

random_forest=train(y ~. , data = train, 
                 method = "ranger", 
                 trControl = fitControl_random, num.trees=400,
                 metric="Prime",
                 maximize=TRUE,
                 tuneGrid = rf_grid,
                 class.weights  = c(sum(data_train$y=="No")/nrow(data_train),sum(data_train$y=="Yes")/nrow(data_train)))

random_forest

pred_rf=predict(random_forest, test,type="prob")

sum(pred_rf$Yes<0.5)/nrow(pred_rf)

max(pred_rf$Yes)

pred_rf[,c(2)]=pred_rf[,c(2)]+(1-max(pred_rf[,c(2)]))

pred_rf=pred_rf[,2]

sum(pred_rf<0.5)/length(pred_rf)

data_train_sgm=as.data.frame(train)
data_test_sgm=as.data.frame(test)

data_train_sgm=data_train_sgm[,c(-37,-50,-52)]
data_test_sgm=data_test_sgm[,c(-37,-50,-52)]

gbm_dt=train(y ~ ., data = data_train_sgm, 
                 method = "gbm", 
                 trControl = fitControl_random,
                 metric="Prime",
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_dt

pred_gbm=predict(gbm_dt, data_test_sgm,type="prob")

pred_gbm=pred_gbm[,2]

sum(pred_gbm<0.5)/length(pred_gbm)

pred_gbm=pred_gbm+(1-max(pred_gbm))

max(pred_gbm)

sum(pred_gbm<0.5)/length(pred_gbm)

xgboost_dt=train(y ~ ., data = train, 
                 method = "xgbTree", 
                 trControl = fitControl_random,
                 metric="Prime",  
                 tuneGrid = xgbGrid,
                 weight=c(sum(data_train$y=="No")/nrow(data_train),sum(data_train$y=="Yes")/nrow(data_train)),
                 verbose=F)

pred_xgb=predict(xgboost_dt, test, type="prob")

pred_xgb=pred_xgb[,2]

sum(pred_xgb<0.5)/length(pred_xgb)

pred_xgb=pred_xgb+(1-max(pred_xgb))

max(pred_xgb)

test$y=as.numeric(as.factor(test$y))-1

library(xlsx)

all_results_analysis=data.table(random_forest=pred_rf,
                                stochastic=pred_gbm,
                                xgboost_res=pred_xgb,
                                actual_val=test$y)

write.xlsx(all_results_analysis,"model_control.xlsx")

last_num=(pred_xgb+pred_rf+pred_gbm)/3

sum(last_num<0.5)/length(last_num)

sum(last_num<0.5)/length(last_num)

manys=0
for(i in 1:length(last_num)){
    if(last_num[i]<=0.5){
        count=0
        count1=0
        sum=0
        sum1=0
        if(pred_rf[i]>0.35){
            sum=pred_rf[i]+sum+0.155
            count=count+1
        }
        if(pred_gbm[i]>0.35){
            sum=pred_gbm[i]+sum+0.155
            count=count+1
        }
        if(pred_xgb[i]>0.35){
            sum=pred_xgb[i]+sum+0.155
            count=count+1
        }
        else if(pred_rf[i]>0.22 & pred_gbm[i]>0.22){
            sum1=(pred_rf[i]+pred_gbm[i])/2+sum1+0.285  
            count1=count1+1
        } 
        else if(pred_gbm[i]>0.22 & pred_xgb[i]>0.22){
            sum1=(pred_gbm[i]+pred_xgb[i])/2+sum1+0.285 
            count1=count1+1
        } 
        else if(pred_rf[i]>0.22 & pred_xgb[i]>0.22){
            sum1=(pred_rf[i]+pred_xgb[i])/2+sum1+0.285    
            count1=count1+1
        } 
#         else if(pred_rf[i]>0.20 & pred_gbm[i]>0.20 & pred_xgb[i]>0.20){
#             last_num[i]=((pred_rf[i]+pred_gbm[i]+pred_xgb[i])/3)+0.305     
#         } 
        
        if(count>0){
            manys=manys+(count/count)
            last_num[i]=sum/count
        }   
        if(count1>0){
            manys=manys+(count1/count1)
            last_num[i]=sum1/count1
        }  
    }
}

res=data.table(last_exp=last_num)

write.xlsx(res,"res_cont.xlsx")

manys

sum(data_train$y=="No")/nrow(data_train)

sum(last_num<0.5)/length(last_num)

send_submission(last_num, token, url=subm_url, submit_now= submit_now)


