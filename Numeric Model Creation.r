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

TrainData$y=as.numeric(TrainData$y)-1

spl=sample.split(TrainData$y, SplitRatio = 0.8)
train=subset(TrainData,spl==TRUE)
test=subset(TrainData,spl==FALSE)
str(train)

library(xlsx)

write.xlsx(test, "cont_train.xlsx")

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
                        n.trees = (1:5)*25, 
                        shrinkage = c(0.1, 0.3, 0.5),
                        n.minobsinnode = 20)

gbm_fit=train(y ~ .-x57, data = train, 
                 method = "gbm", 
                 trControl = fitControl,  
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_fit
plot(gbm_fit)

predictions_train=predict(gbm_fit,test)

length(predictions_train)

perf_dt("Performance Measure", as.numeric(predictions_train), as.numeric(test$y))

final_test=read.csv("IE582_Fall20_ProjectTest.csv")

final_test<-final_test[,-c(50,52)]

gbm_fit=train(y ~ .-x37, data = TrainData, 
                 method = "gbm", 
                 trControl = fitControl,  
                 tuneGrid = gbmGrid,
                 verbose=F)

gbm_fit
plot(gbm_fit)

final_prediction=predict(gbm_fit,final_test)

library(randomForest)

random_forest=randomForest(y~.,data=train,ntree=500,nodesize=20)
random_forest

PredictRandomForest=predict(random_forest,newdata=test)

perf_dt("Random Forest",as.numeric(test$y),as.numeric(PredictRandomForest))

random_forest_final=randomForest(y~.,data=TrainData,ntree=500,nodesize=20)
random_forest_final

predictions_rf_final=predict(random_forest_final,final_test)

library(rpart)
library(GGally, quietly=TRUE)
library(rattle)

PenaltyMatrix = matrix(c(0,1.5,3,0), byrow=TRUE, nrow=2)
PenaltyMatrix

DecTree_pen = rpart(y ~ .,
                   data=train, parms=list(loss=PenaltyMatrix),
                   cp=0.01)
fancyRpartPlot(DecTree_pen)
DecTree_pen$variable.importance

penalized_tree=predict(DecTree_pen,newdata=test)

DecTree_pen = rpart(y ~ .,
                   data=TrainData, parms=list(loss=PenaltyMatrix),
                   cp=0.01)

penalized_tree_final=predict(DecTree_pen,newdata=final_test)

library(glmnet)

TrainData=as.data.table(TrainData)
final_test=as.data.table(final_test)

train=as.data.table(train)
test=as.data.table(test)

train_mat_spam=data.matrix(train[complete.cases(train),-c("y"),with=F])

result_vec_spam=as.vector(t(train[complete.cases(train),"y"]))

cvfit_spam=cv.glmnet(train_mat_spam,result_vec_spam,nfolds = 10,type.measure = "mse")

test_mat_spam=data.matrix(test[,-c("y")])

lasso_model <- glmnet(train_mat_spam,result_vec_spam, alpha = 1, lambda = cvfit_spam$lambda.min, standardize = FALSE)

lasso_predicts_train <- predict(lasso_model, s = cvfit_spam$lambda.min, newx = test_mat_spam)

train_mat_spam=data.matrix(TrainData[complete.cases(TrainData),-c("y"),with=F])

result_vec_spam=as.vector(t(TrainData[complete.cases(TrainData),"y"]))

cvfit_spam=cv.glmnet(train_mat_spam,result_vec_spam,nfolds = 10,type.measure = "mse")

test_mat_spam=data.matrix(final_test[,-c("y")])

lasso_model <- glmnet(train_mat_spam,result_vec_spam, alpha = 1, lambda = cvfit_spam$lambda.min, standardize = FALSE)

lasso_predicts <- predict(lasso_model, s = cvfit_spam$lambda.min, newx = test_mat_spam)

final_prediction_bin=ifelse(final_prediction<0.485,0,1)

predictions_rf_final_bin=ifelse(predictions_rf_final<0.485,0,1)

penalized_tree_final_bin=ifelse(penalized_tree_final<0.485,0,1)

lasso_predicts_bin=ifelse(lasso_predicts<0.485,0,1)

avg_num=(final_prediction+predictions_rf_final+lasso_predicts)/3

avg_num_bin=ifelse(avg_num<0.485,0,1)

cus_avg=(ifelse(final_prediction>0.5,(final_prediction+(final_prediction-0.5)*3),final_prediction)+
    ifelse(predictions_rf_final>0.5,(predictions_rf_final+(predictions_rf_final-0.5)*3),predictions_rf_final)+
    ifelse(lasso_predicts>0.5,(lasso_predicts+(lasso_predicts-0.5)*3),lasso_predicts))/3

cus_avg_bin=ifelse(cus_avg<0.485,0,1)

two_avg=ifelse(final_prediction_bin==1|predictions_rf_final_bin==1,1,0)

a=(final_prediction+predictions_rf_final)/2

a[4]

str(a[1])

for(i in 1:length(a)){
    if(a[i]<0){
        a[i]=0
    }
    if(a[i]>1){
        a[i]=1
    }
}

last_num=array(1:2073)

for(i in 1:length(final_prediction)){
    if(two_avg[i]==1 | cus_avg_bin[i]==1 | avg_num[i]==1){
        last_num[i]=1
    }
    else{
        last_num[i]=0
    }
}

#table(final_prediction,predictions_rf_final,penalized_tree_fin_nm)

# library(xlsx)

# numbers_index=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,0,0,18,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,35,0,0,0,0,40,0,0,43,0,0,0,0,48,0,50,0,0,0,0,0,0,0,0,59,0,0,0,0,0,0,66,0,0,0,0,0,0,0,74,0,0,0,0,0,0,0,0,0,84,0,0,0,0,0,0,91,92,0,0,0,0,97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,121,0,0,0,125,0,127,0,129,0,131,132,0,0,0,136,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,152,0,154,0,156,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,174,0,0,0,0,0,180,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,0,0,0,0,0,0,0,0,209,0,0,0,0,0,215,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,232,0,0,0,0,0,238,0,0,0,0,243,0,0,0,0,0,0,0,0,0,253,0,0,0,257,258,0,0,0,0,0,0,0,266,0,0,0,0,271,0,273,0,275,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,310,0,0,0,0,0,0,0,318,0,320,321,0,0,0,0,0,0,328,0,330,0,0,0,334,0,0,0,0,339,340,0,0,0,0,345,0,0,348,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,377,0,0,0,0,0,383,384,0,0,0,0,0,0,0,392,0,0,0,396,397,0,0,0,0,0,0,404,0,0,407,408,409,0,0,412,0,0,0
# )

# indexes=unique(numbers_index)

# indexes=indexes[2:length(indexes)]

# write.xlsx(train[indexes], file="indexes.xlsx", 
#            sheetName="1")

# library(kernlab)

# svm1 <- train(as.factor(y) ~., data = train, method = "svmLinear",
#               trControl = fitControl,  preProcess = c("center","scale"))
# #View the model
# svm1

# svm1_pred_train=predict(svm1,test)

# confusionMatrix(data = as.factor(svm1_pred_train), reference = as.factor(test$y), mode = "prec_recall")

# svm3 <- train(as.factor(y) ~., data = train, method = "svmRadial",
#               trControl = fitControl, preProcess = c("center","scale"), tuneLength = 10)
# # Print the best tuning parameter sigma and C that maximizes model accuracy
# svm3

# svm3_pred_train=predict(svm3,test)

# confusionMatrix(data = as.factor(svm3_pred_train), reference = as.factor(test$y), mode = "prec_recall")

# svm4 <- train(as.factor(y) ~., data = train, method = "svmPoly", 
#               trControl = fitControl, preProcess = c("center","scale"), tuneLength = 4)
# # Print the best tuning parameter sigma and C that maximizes model accuracy
# svm4

# svm4_pred_train=predict(svm4,test)

# confusionMatrix(data = as.factor(svm4_pred_train), reference = as.factor(test$y), mode = "prec_recall")

max(a)

send_submission(a, token, url=subm_url, submit_now= submit_now)


