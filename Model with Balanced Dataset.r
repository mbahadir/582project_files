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


library(ggplot2)
library(dplyr)
library(tidyverse)
library(Information)
library(FactoMineR)
library(caTools)

TrainData<-read.csv("IE582_Fall20_ProjectTrain.csv")

TrainData<-TrainData[,-c(50,52)]

TrainData=as.data.table(TrainData)

set.seed(35)
ind_a=sample(1:1565, 400, replace=FALSE)
ind_b=sample(1:509, 400, replace=FALSE)

dt_new_sub_a=TrainData[TrainData[,y=="a"]][ind_a]
dt_new_sub_b=TrainData[TrainData[,y=="b"]][ind_b]

dt_new_sub_a_test=TrainData[TrainData[,y=="a"]][!ind_a]
dt_new_sub_b_test=TrainData[TrainData[,y=="b"]][!ind_b]

dt_new_train=rbind(dt_new_sub,dt_new_sub_b)
dt_new_test=rbind(dt_new_sub_a_test,dt_new_sub_b_test)

set.seed(150)
spl=sample.split(dt_new_train$y, SplitRatio = 0.8)
train=subset(dt_new_train,spl==TRUE)
test=subset(dt_new_train,spl==FALSE)
str(train)

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

library(caret)
library(e1071)

fitControl=trainControl(method = "repeatedcv",
                           number = 10)

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                        n.trees = (1:5)*10, 
                        shrinkage = c(0.1, 0.3, 0.5),
                        n.minobsinnode = 20)

gbm_fit=train(y ~ ., data = dt_new, 
                 method = "gbm", 
                 trControl = fitControl,  
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_fit
plot(gbm_fit)

predictions_train=predict(gbm_fit,test)

confusionMatrix(data = as.factor(predictions_train), reference = as.factor(test$y), mode = "prec_recall")

perf_dt("Performance Measure", as.numeric(predictions_train), as.numeric(test$y))

table(test$y,predictions_train)

final_test=read.csv("IE582_Fall20_ProjectTest.csv")

final_test<-final_test[,-c(50,52)]

gbm_fit=train(y ~ ., data = dt_new_train, 
                 method = "gbm", 
                 trControl = fitControl,  
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_fit
plot(gbm_fit)

predictions=predict(gbm_fit,final_test)

final_prediction=as.numeric(predictions)-1

library(randomForest)

random_forest=randomForest(as.factor(y)~.,data=train,ntree=500,nodesize=20)
random_forest

varImpPlot(random_forest)

PredictRandomForest=predict(random_forest,newdata=test)
table(test$y,PredictRandomForest)

confusionMatrix(data = PredictRandomForest, reference = as.factor(test$y), mode = "prec_recall")

perf_dt("Random Forest",as.numeric(test$y),as.numeric(PredictRandomForest))

random_forest_final=randomForest(as.factor(y)~.,data=TrainData,ntree=500,nodesize=20)
random_forest_final

predictions_rf=predict(random_forest_final,final_test)
predictions_rf_final=as.numeric(predictions_rf)-1

table(final_prediction,predictions_rf_final)

library(rpart)
library(GGally, quietly=TRUE)
library(rattle)

PenaltyMatrix = matrix(c(0,1.5,5,0), byrow=TRUE, nrow=2)
PenaltyMatrix

DecTree_pen = rpart(y ~ .,
                   data=train, method="class", parms=list(loss=PenaltyMatrix),
                   cp=0.01)
fancyRpartPlot(DecTree_pen)
DecTree_pen$variable.importance

penalized_tree=predict(DecTree_pen,newdata=test,type="class")
table(test$y,penalized_tree)

confusionMatrix(data = penalized_tree, reference = as.factor(test$y), mode = "prec_recall")

DecTree_pen = rpart(y ~ .,
                   data=TrainData, method="class", parms=list(loss=PenaltyMatrix),
                   cp=0.01)

penalized_tree_final=predict(DecTree_pen,newdata=final_test,type="class")
penalized_tree_fin_nm=as.numeric(penalized_tree_final)-1

length(penalized_tree_fin_nm)

table(test$y)

table(test$y,predictions_train,penalized_tree)

table(test$y,PredictRandomForest,penalized_tree)

y_num=as.numeric(test$y)-1
rf_num=as.numeric(PredictRandomForest)-1
pr_num=as.numeric(penalized_tree)-1
sb_num=as.numeric(predictions_train)-1

length(test$y)

last_num=array(1:2073)

for(i in 1:length(final_prediction)){
    if(final_prediction[i]==1 | predictions_rf_final[i]==1){
        last_num[i]=1
    }
    else{
        last_num[i]=0
    }
}

head(last_num,60)

head(final_prediction,60)

head(predictions_rf_final,60)

table(final_prediction,predictions_rf_final,penalized_tree_fin_nm)

send_submission(last_num, token, url=subm_url, submit_now= submit_now)


