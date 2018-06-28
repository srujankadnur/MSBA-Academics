
install.packages("read_xl")
library(readxl)
inc_data <- read_excel("C:/Users/srujan/Desktop/Business  data mining/Assignment 2/income.data.xlsx", 
                          col_names = FALSE, na = "NA")
str(inc_data)
colSums(is.na(inc_data))
colnames(inc_data) <- c('annual_inc',
                      'sex', 'MaritalStatus',
                      'Age', 'Education', 'Occupation',
                      'duration','dual_inc',
                      'ppl_household','ppl_u18','HHStatus',
                      'home_type','Ethnicity','Language')
inc_data$annual_inc <- factor(inc_data$annual_inc,c("1","2","3","4","5","6","7","8","9"))
str(inc_data$annual_inc)
inc_data$sex <- factor(inc_data$sex,c('1','2'),c('Male','Female'))
inc_data$MaritalStatus <- factor(inc_data$MaritalStatus)
inc_data$Age <- factor(inc_data$Age)
inc_data$Education <- factor(inc_data$Education)
inc_data$Occupation <- factor(inc_data$Occupation)
inc_data$duration <- factor(inc_data$duration)
inc_data$dual_inc <- factor(inc_data$dual_inc)
inc_data$ppl_household <- factor(inc_data$ppl_household)
inc_data$ppl_u18 <- factor(inc_data$ppl_u18)
inc_data$HHStatus <- factor(inc_data$HHStatus)
inc_data$home_type <- factor(inc_data$home_type)
inc_data$Ethnicity <- factor(inc_data$Ethnicity)
inc_data$Language <- factor(inc_data$Language)
summary(inc_data)


Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

#Imputing the missing values with mode
inc_data$Education[is.na(inc_data$Education)] <- Mode(inc_data$Education)
inc_data$Occupation[is.na(inc_data$Occupation)] <- Mode(inc_data$Occupation)
inc_data$MaritalStatus[is.na(inc_data$MaritalStatus)]<- Mode(inc_data$MaritalStatus)
inc_data$Ethnicity[is.na(inc_data$Ethnicity)] <- Mode(inc_data$Ethnicity)
inc_data$Language[is.na(inc_data$Language)] <- Mode(inc_data$Language)
inc_data$home_type[is.na(inc_data$home_type)] <- Mode(inc_data$home_type)
inc_data$ppl_household[is.na(inc_data$ppl_household)] <- Mode(inc_data$ppl_household)
inc_data$HHStatus[is.na(inc_data$HHStatus)] <- Mode(inc_data$HHStatus)

library(vcd)

### PART-A #############################

# Important variables analysed from the plots that follow,
# These are major variables that could be used in prediction
   # Age
   # Marital status
   # Education
   # occupation
   # household status
   # home type
   # ethnicity

par(mfrow=c(1,1))
# Increasing trend observed between age and income, that can be winessed 
# from high proportion of 3,4,5,6 age groups as we from annual income class 1 to 9
mosaicplot(inc_data$annual_inc ~ inc_data$Age, inc_data)
# No significant trend observed between annual income and sex as can be seen from plot
mosaicplot(inc_data$annual_inc ~ inc_data$sex, inc_data)
# People who are married or living together (class1 and class2) show positive trend
# with annual income as observed from the below plots
mosaicplot(inc_data$annual_inc ~ inc_data$MaritalStatus, inc_data)
# Education shows an increasing trend with income, higher the education higher the income, 
# which can be inferred from the graphs
mosaicplot(inc_data$annual_inc ~ inc_data$Education, inc_data)
#People who belong to occupation(1,2) tend to increase as annual income increases, 
# which shows it's an important variable
mosaicplot(inc_data$annual_inc ~ inc_data$Occupation, inc_data)
# Duration lived doesn't have any impact on the annual income earned
mosaicplot(inc_data$annual_inc ~ inc_data$duration, inc_data)
#dual_inc (particularly class2) shows positive trend with annual income 
mosaicplot(inc_data$annual_inc ~ inc_data$dual_inc, inc_data)
# No significant trends are observed between people in house and annual incomes
mosaicplot(inc_data$annual_inc ~ inc_data$ppl_household, inc_data)
# No significant trends are observed between people under 18 and annual incomes
mosaicplot(inc_data$annual_inc ~ inc_data$ppl_u18, inc_data)
# Having an own house is directly proportional to income,
# which can be witnessed from the plot
mosaicplot(inc_data$annual_inc ~ inc_data$HHStatus, inc_data)
#There is increasing trend in home_type 1 as we from income class 1 to 9
mosaicplot(inc_data$annual_inc ~ inc_data$home_type, inc_data)
# White ethnicity shows positive relation with higher incomes, 
# but the trend is not as sharp
mosaicplot(inc_data$annual_inc ~ inc_data$Ethnicity, inc_data)
# No significant trends are observed between Language and annual incomes
mosaicplot(inc_data$annual_inc ~ inc_data$Language, inc_data)

### PART-B #############################

# We fix the seed so that every time we run the model we do not work with different samples
set.seed(1234)
nrow(inc_data)
index = sample(2, nrow(inc_data), replace = TRUE, prob = c(0.6,0.4))
TrainData = inc_data[index == 1, ]
nrow(TrainData)
TestData = inc_data[index == 2,]
nrow(TestData)

# ******************************* DECISION TREES WITH PARTY PACKAGE*******************************************************************************
# install.packages("party")
# install.packages("rpart")
# install.packages("rattle")
# install.packages("caret")

library(party)
# constructing decision trees using default values and plotting the decision trees
library(rattle)
library(caret)
library(rpart)
library(rpart.plot)
library(partykit)
tree_default = rpart(annual_inc~., data = TrainData,method = "class")
# rpart.plot(tree_default)
fancyRpartPlot(tree_default,tweak=1.5)
summary(tree_default)
tree_default1 <- as.party(tree_default)
nodeids(tree_default1,terminal = TRUE)

#Number of leaves in the system are 6 as can be seen from plot
#Size of leaf node below as follows
# Node number 2: 522 observations
# Node number 7: 1927 observations
# Node number 12: 596 observations
# Node number 26: 903 observations
# Node number 54: 822 observations
# Node number 55: 640 observations

### PART-C #############################

# Age,Education,Occupation,HHStatus,MaritalStatus are important variables from the tree
# as suggested by variable importance and primary splits

# Variable importance
# Age     Education      HHStatus    Occupation MaritalStatus      dual_inc 
# 46            14            14            10             7             5 
# home_type ppl_household       ppl_u18 
# 3             1             1 

# Primary splits:
#   Age           splits as  LRRRRRR,   improve=289.9383, (0 missing)
# Occupation    splits as  RRRRRLRRL, improve=280.1222, (0 missing)
# Education     splits as  LLRRRR,    improve=224.2028, (0 missing)
# HHStatus      splits as  RRL,       improve=212.6706, (0 missing)
# MaritalStatus splits as  RRRLL,     improve=173.8055, (0 missing)

# These variables show high similarity with the variables that were obtained in part A), 
# in terms of trends with income, all these variables had a significant trend.

# Look at the two-way table to check the performance of the mdoel on train data
a <- table(predict(tree_default,type = "class"), TrainData$annual_inc, dnn = c("predicted", "actual"))  #Compare Predicted vs Actual
error_rate <- 1-sum(diag(a))/sum(a)
error_rate
# Accuracy of the default model= (877+148+644)/5410 = 30.8%
# prediction of the model is below par, with almost 70% error on the training data

### PART-D #############################
fancyRpartPlot(tree_default,tweak=1.5)

# Rule1: Person who is living in his own house (HH status=Own) is likely to earn higher income
# Explanation: As can be seen from the leaf node 7 (for which support is 36%)
# the confidence of higher income groups (6,7,8,9) : 0.16,0.16,0.26,0.19, 
# this suggests there is a likelihood of 76% to earn more than30,000 per year if you own a house 

# Rule2: If a person who belongs to age group (2,6,7) is likely to earn low income per year, i.e., 
# Young adults(18-24) and senior citizens(55 and more) are probable to earn low annual income
# Explanation: For Node 26 (for which support is 17%), 
# the confidence of lower income groups (1,2,3,4): 0.20,0.20,0.15,0.12
# this suggests there is a likelihood of 67% to earn less than 30,000 per year 
# if you belong to these age groups

### PART-E #############################
# Surrogate splits were not used in the construction of the tree, 
# but CART in default provides the surrogate splits

# Meaning of surrogate:

# The ideal surrogate splits the data in exactly the same way as the primary split, 
# in other words, we are looking for clones, close approximations, 
# something else in the data that can do the same work that the 
# primary splitter accomplished.
# Surrogates have two primary functions: first, to split the data when 
# the primary splitter is missing.
# Now, the primary splitter may never have been missing in the training data. 
# However, when it comes time to make predictions on future data, 
# we have no idea whether that particular splitter will always be available.
# When it is missing, then the surrogates will be able to take over 
# and take on the work that the primary splitter accomplished during the 
# initial building of the tree. In addition, surrogates reveal common patterns 
# among predictors and the data set.

# Example of surrogate split in the tree constructed here:

# Node number 1: 5410 observations,    complexity param=0.05201916
# Surrogate splits:
#   Education splits as  LLRRRR,     agree=0.939, adj=0.368, (0 split)
# ppl_u18   splits as  RRRRRRRRLR, agree=0.904, adj=0.002, (0 split)


### PART-F #############################

# confusion matrix
b <- table(predict(tree_default,TestData,type = "class"), TestData$annual_inc, dnn = c("predicted", "actual"))
error_b <- 1-sum(diag(b))/sum(b)
error_b
# Accuracy of model test data = (615+0+0+91+0+0+0+389+0)/3583 = 30.5%
# so, test error is 69.5%

### PART-G #############################
##Characteristic/Profile of high income groups
# By following the path of node 1-3-7 on extreme right (p of node 7:36)
# it gives out the below profile of high income groups
# Age group: Above 18
# Household status : Own 

# By following the path of nodes 1-3-6-13-27-55 (p of 55 node:17%) 
# it gives out the below profile of high income groups
# Occupation:1,2,3,4,5,7,8 (except student and unemployed)
# Age:3,4,5 (from 25-54)
# Marital Status:1,2 (Married or living together)

# Combining both these paths info, profile should look as below:
#   Age Group: 25-54
#   Household status: Own
#   Occupation:Some Sort of employment 
#   Marital status : Married or living together

### PART-H #############################
inc_big <- read_excel("C:/Users/rakesh/Desktop/Business  data mining/Assignment 2/income.big.xlsx", 
                       col_names = FALSE, na = "NA")
summary(inc_big)
colnames(inc_big) <- c('annual_inc',
                        'sex', 'MaritalStatus',
                        'Age', 'Education', 'Occupation',
                        'duration','dual_inc',
                        'ppl_household','ppl_u18','HHStatus',
                        'home_type','Ethnicity','Language','spur_1','spur_2','spur_3',
                        'spur_4','spur_5','spur_6','spur_7','spur_8','spur_9','spur_10')

colSums(is.na(inc_big))
inc_big <- as.data.frame(lapply(inc_big,factor))
str(inc_big)
inc_big$sex <- factor(inc_big$sex,c('1','2'),c('Male','Female'))

# treating missing values with mode 

for(i in 1:ncol(inc_big)){
  inc_big[is.na(inc_big[,i]), i] <- mode(inc_big[,i])
}

# checking if missing values are treated
colSums(is.na(inc_big))

# constructing tree for income_big by taking training data with same 
# number of observations in training data of part(c) ~ 5410 samples 
index = sample(2, nrow(inc_big), replace = TRUE, prob = c(0.835,0.165))
TrainData_big = inc_big[index == 1, ]
nrow(TrainData_big)
TestData_big = inc_big[index == 2,]
nrow(TestData_big)

big_tree = rpart(annual_inc~., data = TrainData_big,method = "class")
rpart.plot(big_tree)
fancyRpartPlot(big_tree,tweak=1.5)
summary(big_tree)

# calculating training error for income_big
table(predict(big_tree,type = "class"), TrainData_big$annual_inc, dnn = c("predicted", "actual"))  #Compare Predicted vs Actual

# accuracy of the training model income_big=(925+252+509)/5437 = 31.09% (more or less same as c)
# Accuracy of the default model in part(c)= (877+148+644)/5410 = 30.8%

# Variable importance training model income_big
# Occupation      HHStatus           Age     Education MaritalStatus     home_type      dual_inc 
# 38            22            19             6             6             4             4 

# Variable importance for default model in part(c)
# Age     Education      HHStatus    Occupation MaritalStatus      dual_inc 
# 46            14            14            10             7             5 
# home_type ppl_household   ppl_u18 
# 3             1             1 

# Top 5 important variables remain same, 
# but the order of splitting and importance is different in these model


### PART-I #############################

for (i in 1:8){
indec = sample(2, nrow(inc_data), replace = TRUE, prob = c(0.6,0.4))
train_data = inc_data[indec == 1,]
test_data = inc_data[indec == 2,]
# assign(paste0("data",i),train_data)
# assign(paste0("data_t",i),test_data)
tree_def = rpart(annual_inc~., data = train_data,method = "class")
temp <- table(predict(tree_def,type = "class"), train_data$annual_inc, dnn = c("predicted", "actual"))
test_temp <- table(predict(tree_def,test_data,type = "class"), test_data$annual_inc, dnn = c("predicted", "actual"))
assign(paste0("training_error_rate",i),1-sum(diag(temp))/sum(temp))
assign(paste0("testing_error_rate",i),1-sum(diag(test_temp))/sum(test_temp))
}

error_matrix <- matrix(c(testing_error_rate1,training_error_rate1,testing_error_rate2,training_error_rate2,
                         testing_error_rate3,training_error_rate3,testing_error_rate4,training_error_rate4,
                         testing_error_rate5,training_error_rate5,testing_error_rate6,training_error_rate6,
                         testing_error_rate7,training_error_rate7,testing_error_rate8,training_error_rate8),ncol=2,byrow = T)

colnames(error_matrix) <- c("test","train")
error_matrix

# All samples showed the similar error rates with test and train samples with tolerance arounf 1-3% from the above matrix, 
# which is also expected because of the random sampling method that is chosen

### PART-J #############################

# constructing a minimum pruning tree
set.seed(1234)
indec = sample(2, nrow(inc_data), replace = TRUE, prob = c(0.6,0.4))
train_data_min_prun = inc_data[indec == 1,]
test_data_min_prun = inc_data[indec == 2,]
# using cp value of 0.001 and constructing the tree
tree_def_min_prun = rpart(annual_inc~., data = train_data_min_prun,method = "class",cp=0.001)
library("partykit")
# summary(tree_def_min_prun)
tree_def_min_prun_kt <- as.party(tree_def_min_prun)
#Number of terminal nodes in minimum pruning tree = 52
nodeids(tree_def_min_prun_kt,terminal = TRUE)
rpart.plot(tree_def_min_prun)  

# constructing second tree with leaf node limit=100 and parent node limit=500
tree_def_split = rpart(annual_inc~., data = train_data_min_prun,method = "class",minsplit=100,minbucket=500)
# choosing the best cp from the plot
plotcp(tree_def_split)
tree_def_split = rpart(annual_inc~., data = train_data_min_prun,method = "class",minsplit=100,minbucket=500,cp=0.01)
rpart.plot(tree_def_split)
tree_def_split_kt <- as.party(tree_def_split)
#Number of terminal nodes in this tree = 4 
nodeids(tree_def_split_kt,terminal = TRUE)

# Comparing errors of both models 
pred_train_min_prun <- table(predict(tree_def_min_prun,type = "class"), train_data_min_prun$annual_inc, dnn = c("predicted", "actual"))
pred_train_split <- table(predict(tree_def_split,type = "class"), train_data_min_prun$annual_inc, dnn = c("predicted", "actual"))
pred_test_min_prun <- table(predict(tree_def_min_prun,test_data_min_prun,type = "class"), test_data_min_prun$annual_inc, dnn = c("predicted", "actual"))
pred_test_split <- table(predict(tree_def_split,test_data_min_prun,type = "class"), test_data_min_prun$annual_inc, dnn = c("predicted", "actual"))

e1 <- 1-sum(diag(pred_train_min_prun))/sum(pred_train_min_prun)
e2 <- 1-sum(diag(pred_train_split))/sum(pred_train_split)
e3 <- 1-sum(diag(pred_test_min_prun))/sum(pred_test_min_prun)
e4 <- 1-sum(diag(pred_test_split))/sum(pred_test_split)

error_matrix_both <- matrix(c(e1,e2,e3,e4),ncol = 2,byrow = T)
colnames(error_matrix_both) <- c("Minimum pruned model","second model")
row.names(error_matrix_both) <- c("train","test")

#error results of both models
error_matrix_both

# There is a difference in training errors of both models. 
# Also, in the second model, training and test errors are almost same, 
# but that is not tha case in minimum pruned model
# Although test error and training error is low for the minimum pruned model, 
# there might be overfitting scenario which is reflected by the difference in training and 
# test errors and also the size of terminal nodesin the minimum pruned model is much higher(52)
# compared to 4 terminal nodes in other model


### PART-K #############################

set.seed(1234)

# 50-50 split
ind = sample(2, nrow(inc_data), replace = TRUE, prob = c(0.5,0.5))
train_data_50 = inc_data[ind == 1,]
test_data_50 = inc_data[ind == 2,]
tree_def_50 = rpart(annual_inc~., data = train_data_50 ,method = "class")

# training error and testing error
pred_train_50 <- table(predict(tree_def_50,type = "class"), train_data_50$annual_inc, dnn = c("predicted", "actual"))
pred_test_50 <- table(predict(tree_def_50,test_data_50,type = "class"), test_data_50$annual_inc, dnn = c("predicted", "actual"))
err_train_50 <- 1-sum(diag(pred_train_50))/sum(pred_train_50)
err_test_50 <- 1-sum(diag(pred_test_50))/sum(pred_test_50)

# 70-30 split
ind = sample(2, nrow(inc_data), replace = TRUE, prob = c(0.7,0.3))
train_data_70 = inc_data[ind == 1,]
test_data_70 = inc_data[ind == 2,]
tree_def_70 = rpart(annual_inc~., data = train_data_70 ,method = "class")

# training error and testing error
pred_train_70 <- table(predict(tree_def_70,type = "class"), train_data_70$annual_inc, dnn = c("predicted", "actual"))
pred_test_70 <- table(predict(tree_def_70,test_data_70,type = "class"), test_data_70$annual_inc, dnn = c("predicted", "actual"))
err_train_70 <- 1-sum(diag(pred_train_70))/sum(pred_train_70)
err_test_70 <- 1-sum(diag(pred_test_70))/sum(pred_test_70)

# 90-10 split
ind = sample(2, nrow(inc_data), replace = TRUE, prob = c(0.9,0.1))
train_data_90 = inc_data[ind == 1,]
test_data_90 = inc_data[ind == 2,]
tree_def_90 = rpart(annual_inc~., data = train_data_90 ,method = "class")

# training error and testing error
pred_train_90 <- table(predict(tree_def_90,type = "class"), train_data_90$annual_inc, dnn = c("predicted", "actual"))
pred_test_90 <- table(predict(tree_def_90,test_data_90,type = "class"), test_data_90$annual_inc, dnn = c("predicted", "actual"))
err_train_90 <- 1-sum(diag(pred_train_90))/sum(pred_train_90)
err_test_90 <- 1-sum(diag(pred_test_90))/sum(pred_test_90)

error_matrix_all_splits <- matrix(c(err_train_50,err_test_50,
                                    err_train_70,err_test_70,err_train_90,err_test_90),ncol=2,byrow = T)
row.names(error_matrix_all_splits) <- c("split_50","split_70","split_90")
colnames(error_matrix_all_splits) <- c("train","test")
error_matrix_all_splits

# error on training and testing sets seem to increase as we move from 50 to 90, 
# I would prefer splitting at 50% beacause the training and test errors are comparatively
# low and number of observations in the terminal nodes have atleast 10%  of total, 
# which is acceptable also
rpart.plot(tree_def_50)

### PART-L #############################

# For Information gain using 60-40 split 
tree_info_gain = rpart(annual_inc~., data = TrainData,method = "class", parms = list(split = 'information'))
rpart.plot(tree_info_gain)
fancyRpartPlot(tree_info_gain,tweak=1.5)

pred_train_info_gain <- table(predict(tree_info_gain,type = "class"), TrainData$annual_inc, dnn = c("predicted", "actual"))
pred_test_info_gain <- table(predict(tree_info_gain,TestData,type = "class"), TestData$annual_inc, dnn = c("predicted", "actual"))

error_train_info <- 1-sum(diag(pred_train_info_gain))/sum(pred_train_info_gain)
error_test_info <- 1-sum(diag(pred_test_info_gain))/sum(pred_test_info_gain)

error_train_info 
error_test_info

# default gini tree
pred_def_train_gini <- table(predict(tree_default,type = "class"), TrainData$annual_inc, dnn = c("predicted", "actual"))
pred_def_test_gini <- table(predict(tree_default,TestData,type = "class"), TestData$annual_inc, dnn = c("predicted", "actual"))
error_train_gini <- 1-sum(diag(pred_def_train_gini))/sum(pred_def_train_gini)
error_test_gini <- 1-sum(diag(pred_def_test_gini))/sum(pred_def_test_gini)

error_matrix_info_gini <- matrix(c(error_train_info,error_test_info,
                                   error_train_gini,error_test_gini),ncol=2,byrow = T)

row.names(error_matrix_info_gini) <- c("info_Gain","gini")
colnames(error_matrix_info_gini) <- c("train","test")

error_matrix_info_gini

# Information gain shows a slight improvement in terms of error percentages compared to gini


