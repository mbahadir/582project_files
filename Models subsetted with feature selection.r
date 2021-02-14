# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
    
    post_body = list(username=username,password=password)
    post_url_string = paste0(url_site,'/token/')
    result = POST(post_url_string, body = post_body)

    # error handling (wrong credentials)
    if(result$status_code==400){
        print('Check your credentials')
        return(0)
    }
    else if (result$status_code==201){
        output = content(result)
        token = output$key
    }

    return(token)
}



send_submission <- function(predictions, token, url_site, submit_now=F){
    
    format_check=check_format(predictions)
    if(!format_check){
        return(FALSE)
    }
    
    post_string="list("
    for(i in 1:length(predictions)){
        if(i<length(predictions)){
            post_string=sprintf("%s%s,",post_string,predictions[i])
        } else {
            post_string=sprintf("%s%s)",post_string,predictions[i])
        }
    }
    
    submission = eval(parse(text=post_string))
    json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
    submission=list(submission=json_body)
    print(submission)

    if(!submit_now){
        print("You did not submit.")
        return(FALSE)      
    }
    

    header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
    post_url_string = paste0(url_site,'/submission/')
    result = POST(post_url_string, header, body=submission)
    
    if (result$status_code==201){
        print("Successfully submitted. Below you can see the details of your submission")
    } else {
        print("Could not submit. Please check the error message below, contact the assistant if needed.")
    }
    
    print(content(result))
    
}

check_format <- function(predictions){
    
    if(all(is.numeric(predictions)) & all(predictions<=1)){
        print("Format OK")
        return(TRUE)
    } else {
        print("Wrong format")
        return(FALSE)
    }
    
}

# this part is main code
subm_url = 'http://46.101.121.83'

u_name = "Miners"
p_word = "NsY7hhlU9zjl8DH3"
submit_now = TRUE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
# this part is where you need to provide your prediction method/function or set of R codes


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

library(xlsx)
library(caret)
library(e1071)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(Information)
library(FactoMineR)
library(caTools)

TrainData<-read.csv("IE582_Fall20_ProjectTrain.csv")

TrainData<-TrainData[,-c(37,50,52,57)]

TestData<-read.csv("IE582_Fall20_ProjectTest.csv")

TestData<-TestData[,-c(37,50,52,57)]

TrainData=as.data.table(TrainData)
TestData=as.data.table(TestData)
TrainData$index=1:nrow(TrainData)
TestData$index=1:nrow(TestData)

balanced_TrainData=TrainData[x60==0 & x59==0 & x58==0& x46==0& x26==0& x24==0& x16==0& x49==0&
                x43==0 & x39==0 & x29==0 & x19==0 & x18==0,]
balanced_TestData=TestData[x60==0 & x59==0 & x58==0& x46==0& x26==0& x24==0& x16==0& x49==0&
                x43==0 & x39==0 & x29==0 & x19==0 & x18==0,]

balanced_TrainData<-balanced_TrainData[,-c("x60", "x59", "x58", "x46", "x26", "x24", "x16", "x49",
                                           "x43", "x39", "x29", "x19", "x18" ,"x54")]


balanced_TestData<-balanced_TestData[,-c("x60", "x59", "x58", "x46", "x26", "x24", "x16", "x49",
                                           "x43", "x39", "x29", "x19", "x18","x54")]

otherbalanced_TrainData=TrainData[!(x60==0 & x59==0 & x58==0& x46==0& x26==0& x24==0& x16==0& x49==0&
                x43==0 & x39==0 & x29==0 & x19==0 & x18==0& x54==0)]

otherbalanced_TestData=TestData[!(x60==0 & x59==0 & x58==0& x46==0& x26==0& x24==0& x16==0& x49==0&
                x43==0 & x39==0 & x29==0 & x19==0 & x18==0& x54==0)]

otherbalanced_TrainData$y=as.factor(otherbalanced_TrainData$y)

balanced_TrainData$y=as.numeric(balanced_TrainData$y)

table(balanced_TrainData$y)

table(otherbalanced_TrainData$y)

fitControl=trainControl(method = "repeatedcv",
                           number = 20)

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                        n.trees = (1:5)*25, 
                        shrinkage = c(0.1, 0.3, 0.5),
                        n.minobsinnode = 20)

gbm_fit=train(as.factor(y) ~ .-index, data = otherbalanced_TrainData, 
                 method = "gbm", 
                 trControl = fitControl,  
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_fit
plot(gbm_fit)

predictions_train=predict(gbm_fit,otherbalanced_TrainData)

perf_dt("Performance Measure", as.numeric(predictions_train), as.numeric(otherbalanced_TrainData$y))

confusionMatrix(data = as.factor(predictions_train), reference = as.factor(otherbalanced_TrainData$y), mode = "prec_recall")

otherbalanced_TrainData$y_pred_sth=as.numeric(predictions_train)-1

gbm_fit1=train(y ~ .-index, data = TrainData, 
                 method = "gbm", 
                 trControl = fitControl,  
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_fit1
plot(gbm_fit1)

final_prediction=predict(gbm_fit1,TrainData)

table(TrainData$y,final_prediction)

confusionMatrix(data = as.factor(final_prediction), reference = as.factor(TrainData$y), mode = "prec_recall")

TrainData$pred=as.numeric(final_prediction)-1

j=1
for(i in 1:nrow(TrainData)){
    if(TrainData[i]$index==otherbalanced_TrainData[j]$index){
        TrainData[i]$pred=otherbalanced_TrainData[j]$y_pred_sth
        j=j+1
    }  
}

confusionMatrix(data = as.factor(TrainData$pred), reference = as.factor(as.numeric(TrainData$y)-1), mode = "prec_recall")

library(ranger)

rf_grid=expand.grid(mtry=c(4,8,10,15,20,30),
                   splitrule = c("extratrees"),
                   min.node.size= c(20))
rf_grid

rf_fit=train(as.factor(y) ~ .-index, data = otherbalanced_TrainData, 
                 method = "ranger", 
                 trControl = fitControl, num.trees=500,
                 tuneGrid = rf_grid)
rf_fit

PredictRandomForest=predict(rf_fit,newdata=otherbalanced_TrainData)

perf_dt("Random Forest",as.numeric(otherbalanced_TrainData$y),as.numeric(PredictRandomForest))

confusionMatrix(data = as.factor(PredictRandomForest), reference = as.factor(otherbalanced_TrainData$y), mode = "prec_recall")

rf_fit1=train(as.factor(y) ~ .-index, data = TrainData, 
                 method = "ranger", 
                 trControl = fitControl, num.trees=500,
                 tuneGrid = rf_grid)
rf_fit1

PredictRandomForest1=predict(rf_fit1,newdata=TrainData)

perf_dt("Random Forest",as.numeric(TrainData$y),as.numeric(PredictRandomForest1))

confusionMatrix(data = as.factor(PredictRandomForest1), reference = as.factor(TrainData$y), mode = "prec_recall")

TrainData$last_y=PredictRandomForest1

otherbalanced_TrainData$y_pred_rnd=PredictRandomForest

j=1
for(i in 1:nrow(TrainData)){
    if(TrainData[i]$index==otherbalanced_TrainData[j]$index){
        TrainData[i]$last_y=otherbalanced_TrainData[j]$y_pred_rnd
        j=j+1
    }  
}

confusionMatrix(data = as.factor(TrainData$last_y), reference = as.factor(TrainData$y), mode = "prec_recall")

predictions_train=predict(gbm_fit,otherbalanced_TestData)

otherbalanced_TestData$y_pred_sth=as.numeric(predictions_train)-1

final_prediction=predict(gbm_fit1,TestData)

TestData$pred=as.numeric(final_prediction)-1

j=1
for(i in 1:nrow(TestData)){
    if(TestData[i]$index==otherbalanced_TestData[j]$index){
        TestData[i]$pred=otherbalanced_TestData[j]$y_pred_sth
        j=j+1
    }  
}

table()

PredictRandomForest=predict(rf_fit,newdata=otherbalanced_TestData)

otherbalanced_TestData$y_pred_rnd=PredictRandomForest

PredictRandomForest1=predict(rf_fit1,newdata=TestData)

TestData$last_y=PredictRandomForest1

j=1
for(i in 1:nrow(TestData)){
    if(TestData[i]$index==otherbalanced_TestData[j]$index){
        TestData[i]$last_y=otherbalanced_TestData[j]$y_pred_rnd
        j=j+1
    }  
}

TestData$last_y=as.numeric(TestData$last_y)-1

table(TestData$last_y)

send_submission(as.numeric(final_prediction)-1, token, url=subm_url, submit_now= submit_now)


