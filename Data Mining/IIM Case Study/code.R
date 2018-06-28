

sample_data= read.csv('C:/Users/rakesh/Desktop/Business  data mining/Assignment 3/sample_data.csv')

sample_data
summary(sample_data)
table(sample_data$`C-MANIPULATOR`)

## Balancing data using ROSE package
install.packages("ROSE")
library(ROSE)
sample_data_2 <- sample_data[,-c(1,10)]
sample_data_2$`C-MANIPULATOR` <- as.factor(sample_data_2$`C-MANIPULATOR`)
str(sample_data_2)
prop.table(table(sample_data_2$`C-MANIPULATOR`))

colnames(sample_data_2)[9] <- c('target')
colnames(sample_data_2)
data_balanced_both <- ROSE(target ~ ., data = sample_data_2, seed = 1)$data

prop.table(table(data_balanced_both$target))
table(data_balanced_both$target)

# creating training and test datasets after balancing 
set.seed(1234)
index = sample(2, nrow(data_balanced_both), replace = TRUE, prob = c(0.7,0.3))
TrainData = data_balanced_both[index == 1, ]
nrow(TrainData)
TestData = data_balanced_both[index == 2,]
nrow(TestData)


# Question-3
## Using logistic regression for the sample data 
# iteration-1
log_model <- glm(target ~ ., family = binomial(link = "logit"), 
                 TrainData)
summary(log_model)
# iteration-2
log_model <- glm(target ~ DSRI+GMI+AQI+SGI+DEPI+ACCR+SGAI, family = binomial(link = "logit"), 
                 TrainData)
summary(log_model)
# iteration-3
log_model <- glm(target ~ DSRI+GMI+SGI, family = binomial(link = "logit"), 
                 TrainData)
summary(log_model)

# training error and estimation
pred_train_model <-predict(log_model,TrainData,type = 'response')
pred_train_model <- ifelse(pred_train_model>0.43,1,0)

confusion_matrix <- table(pred_train_model,TrainData$target,dnn=c("Predicted","Actual"))
confusion_matrix

accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

recall = confusion_matrix[2,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) 
recall

precision= confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])
precision

f_score=2*precision*recall/(precision+recall)
f_score

# testing error and estimation
pred_test_model <-predict(log_model,TestData,type = 'response')
pred_test_model <- ifelse(pred_test_model>0.40,1,0)

confusion_matrix <- table(pred_test_model,TestData$target,dnn=c("Predicted","Actual"))
confusion_matrix

accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

recall = confusion_matrix[2,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) 
recall

precision= confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])
precision

f_score=2*precision*recall/(precision+recall)
f_score


# acc <- list()
# rec <- list()
# prec <- list()
# pred_test <- list()


# for (i in 1:10){
# pred_test[[i]] <-predict(log_model,TestData,type = 'response')
# pred_test[[i]] <- ifelse(pred_test[[i]]>i/100,1,0)
# 
# confusion_matrix <- table(pred_test[[i]],TestData$target,dnn=c("Predicted","Actual"))
# confusion_matrix
# 
# acc[[i]]=sum(diag(confusion_matrix))/sum(confusion_matrix)
# 
# rec[[i]]= confusion_matrix[2,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) 
# 
# prec[[i]]= confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])
# 
# f_score[[i]]=2*prec[[i]]*rec[[i]]/(prec[[i]]+rec[[i]])
# 
# }







#########complete data set balancing ##################

complete_data_2 <- complete_data[,-c(1,10)]
complete_data_2$`C-MANIPULATOR` <- as.factor(complete_data_2$`C-MANIPULATOR`)
str(complete_data_2)
prop.table(table(complete_data_2$`C-MANIPULATOR`))

colnames(complete_data_2)[9] <- c('target')
colnames(complete_data_2)
complete_balanced_data <- ROSE(target ~ ., data = complete_data_2, seed = 1)$data

prop.table(table(complete_balanced_data$target))


# creating training and test datasets after balancing 
set.seed(1234)
index = sample(2, nrow(complete_balanced_data), replace = TRUE, prob = c(0.7,0.3))
TrainData = complete_balanced_data[index == 1, ]
nrow(TrainData)
TestData = complete_balanced_data[index == 2,]
nrow(TestData)

## Using logistic regression for the complete data 
# iteration-1
log_model <- glm(target ~ ., family = binomial(link = "logit"), 
                 TrainData)
summary(log_model)
# iteration-2
log_model <- glm(target ~ DSRI+GMI+AQI+SGI+ACCR+SGAI, family = binomial(link = "logit"), 
                 TrainData)
summary(log_model)

# training error and estimation
pred_train_model <-predict(log_model,TrainData,type = 'response')
pred_train_model <- ifelse(pred_train_model>0.47,1,0)

confusion_matrix <- table(pred_train_model,TrainData$target,dnn=c("Predicted","Actual"))
confusion_matrix

accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

recall = confusion_matrix[2,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) 
recall

precision= confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])
precision

f_score=2*precision*recall/(precision+recall)
f_score



# testing error and estimation
pred_test_model <-predict(log_model,TestData,type = 'response')
pred_test_model <- ifelse(pred_test_model>0.47,1,0)

confusion_matrix <- table(pred_test_model,TestData$target,dnn=c("Predicted","Actual"))
confusion_matrix

accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

recall = confusion_matrix[2,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) 
recall

precision= confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])
precision

f_score=2*precision*recall/(precision+recall)
f_score





