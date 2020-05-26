
# Install all needed packages if not already present
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(corrplot)) install.packages("corrplot")
if(!require(caret)) install.packages("caret")
if(!require(rms)) install.packages("rms")
if(!require(e1071)) install.packages("e1071")
if(!require(MASS)) install.packages("MASS")
if(!require(ROCR)) install.packages("ROCR")
if(!require(randomForest)) install.packages("randomForest")
if(!require(gplots)) install.packages("gplots")
if(!require(pROC)) install.packages("pROC")
if(!require(psych)) install.packages("psych")
if(!require(rpart)) install.packages("rpart")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(kableExtra)) install.packages("kableExtra")
 

# Load all the libraries

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(psych)
library(rpart)
library(randomForest)
library(ggpubr)
library(gridExtra)
library(kableExtra)
 

# reading the dataset
bank <- read_csv("~/Downloads/Churn_Modelling.csv")

# Check dimensions of dataframe
tibble("Length" = nrow(bank), "Columns" = ncol(bank)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# Getting the column names from the dataset
colnames(bank)

# counting the customers who churn
sum(bank$Exited==1)

### Analysis ###

## Exploring the Data

bank <- tibble(bank)

str(bank)
summary(bank)
 

### Cleaning the data

bank_clean <- bank [,c(-2,-3)]

### Discrete Variables 

# checking the number of levels for each discrete variable
unique(bank_clean$Gender) 

unique(bank_clean$Geography)

unique(bank_clean$NumOfProducts)

# converting binary values to factors

categorical <- bank_clean %>%
  mutate(Exited = ifelse(Exited==1,"Yes","No")) %>%
  mutate(HasCrCard = ifelse(HasCrCard==1,"Yes","No")) %>%
  mutate(IsActiveMember = ifelse(IsActiveMember==1,"Yes","No"))
 
c2 <- categorical %>% #selecting the discrete variables
  dplyr::select(Gender, Geography, NumOfProducts, HasCrCard, IsActiveMember, Exited)

p1 <- ggplot(c2)+ # plotting the 
  geom_bar(aes(x = Gender, fill = Exited), position = "fill", stat = "count")

p2 <- ggplot(c2) +
  geom_bar(aes(x = Geography, fill = Exited), position = "fill", stat = "count") 

p3 <- ggplot(c2) +
  geom_bar(aes(x=NumOfProducts, fill=Exited),position="fill",stat="count")

p4 <- ggplot(c2) +
  geom_bar(aes(x=HasCrCard,fill=Exited),position="fill", stat="count")

p5 <- ggplot(c2) +
  geom_bar(aes(x=IsActiveMember,fill=Exited),position="fill", stat="count")

grid.arrange(p1,p2,p3,p4,p5,ncol=2)
 
### Continuous Variables 

p6 <- ggplot(categorical,aes(Age,colour=Exited)) +
  geom_freqpoly(binwidth = 6,size = 0.8)

p7 <- ggplot(data=categorical,aes(CreditScore,colour=Exited)) +
  geom_freqpoly(binwidth = 25, size = 0.8)

p8 <- ggplot(data=categorical,aes(Tenure,colour=Exited)) +
  geom_freqpoly(binwidth = 2.35, size = 0.8)

p9 <- ggplot(data=categorical,aes(Balance,colour=Exited)) +
  geom_freqpoly(binwidth=25000, size=0.8)

p10 <- ggplot(data=categorical,aes(EstimatedSalary,colour=Exited)) +
  geom_freqpoly(binwidth=10000, size=0.8)

grid.arrange(p6,p7,p8,p9,p10,ncol=2)

# **Correlations between Variables**
categorical %>%
  dplyr::select(Age, CreditScore, Tenure, Balance, EstimatedSalary) %>%
  cor() %>%
  corrplot.mixed(upper = "number", lower = "color", tl.col = "black", number.cex=2)
 
# **Checking the Churn Rate for the complete dataset**
  
bank_clean %>%
  summarise(Total = n(), Churn_count= sum(Exited == 1), Churn_probability = Churn_count/Total)

## Logistic Regression Model
  
### Data Cleaning
bank_new <- categorical %>%
  mutate(Exited = ifelse(Exited=="Yes",1,0))
dummy <- dummyVars(" ~ .", data = bank_new)
dummy <- data.frame(predict(dummy, newdata = bank_new))

# Now, we split the data into training and test sets (75% against 25%):
  
set.seed(818)
assignment <- sample(0:1, size= nrow(dummy), prob = c(0.75,0.25), replace = TRUE)
train <- dummy[assignment == 0, ]
test <- dummy[assignment == 1, ]

# **The Training Set**
train %>%
  summarise(Total = n(), Churn_count= sum(Exited == 1), Churn_probability = Churn_count/Total)
 
# **The Test Set**
test %>%
  summarise(Total = n(), Churn_count = sum(Exited == 1), Churn_probability = Churn_count/Total)
 

### Training Set Models

model1 <- glm(Exited ~., family = "binomial", data = train)
 
# Then we use AIC, to easily test the model’s performance, and to exclude variables based on their significance and create `model2`.

model2 <- stepAIC(model1, trace = 0)
summary(model2)
vif(model2)

model3 <- 
  glm(formula = Exited ~  CreditScore + GeographyGermany + GenderFemale
      + Age + Tenure + Balance + NumOfProducts + IsActiveMemberNo, 
      family = "binomial", data = train)
summary(model3)

### Cross Validation 

### We set the default threshold value as 0.5.

Lmodel <- model3
train_prob <- predict(Lmodel, data = train, type = "response") 
test_prob <- predict(Lmodel, newdata = test, type = "response")
train_pred <- factor(ifelse(train_prob >= 0.5, "Yes", "No"))
train_actual <- factor(ifelse(train$Exited == 1, "Yes", "No"))
test_pred <- factor(ifelse(test_prob >= 0.5, "Yes", "No"))
test_actual <- factor(ifelse(test$Exited == 1, "Yes", "No"))
 
# Now, we compute the confusion matrix and ROC for both training and test sets.

##**The Training Set**
 {r echo= TRUE, warning = FALSE, message=FALSE, fig.height=3}
confusionMatrix(data = train_pred, reference = train_actual)
roc <- roc(train$Exited, train_prob, plot= TRUE, print.auc=TRUE)
 

## **The Test Set**
confusionMatrix(data = test_pred, reference = test_actual)
roc <- roc(test$Exited, test_prob, plot= TRUE, print.auc=TRUE)
 

# Therefore we get the following table of results:

table <- matrix(c(0.8142,0.8038,0.2162,0.1827,0.9666,0.9646,0.765,0.780),ncol=2,byrow=TRUE)
rownames(table) <- c("Accuracy","Specificity","Sensitivity","AUC Value")
colnames(table) <- c("Training Set", "Test Set")
table   
 
### Finding the optimal cutoff

pred <- prediction(train_prob, train_actual)
perf <- performance(pred, "spec", "sens")

thres <- data.frame(threshold=perf@alpha.values[[1]], specificity=perf@x.values[[1]], 
                      sensitivity= perf@y.values[[1]])
 
opt_thres <- thres[which.min(abs(thres$specificity-thres$sensitivity)),]
opt_thres %>% knitr::kable()
 
ggplot(data = thres) +
  geom_line(aes(x = threshold, y = specificity, color ="red"), size = 1.5)+
  geom_line(aes(x = threshold, y = sensitivity, color = "blue"), size = 1.5) +
  labs(x = "cutoff", y ="value") +
  scale_color_discrete(name = "", labels = c("Specificity", "Sensitivity"))+
  geom_vline(aes(xintercept = opt_thres$threshold))+
  geom_text(aes(x= 0.55, y= 0.75),label="opt_threshold = 0.2",hjust=1, size=4)

## **Prediction on training set with threshold = 0.2:**

train_pred_c <- factor(ifelse(train_prob >= 0.2, "Yes", "No"))
confusionMatrix(data = train_pred_c, reference = train_actual)
 
## Prediction on test set with threshold = 0.2

test_prob <- predict(Lmodel, newdata = test, type = "response")
test_pred_c <- factor(ifelse(test_prob >= 0.2, "Yes", "No"))
confusionMatrix(data = test_pred_c, reference = test_actual)

## Classification Tree Model

### Data Preparation
banktree <- bank_clean
banktree <- banktree %>%
  mutate_if(is.character, as.factor)
str(banktree)
 
set.seed(818)
tree <- sample(0:1, size= nrow(banktree), prob = c(0.75,0.25), replace = TRUE)
traintree <- banktree[tree == 0, ]
testtree <- banktree[tree == 1, ]
 

### Train Model1
model_tree1 <- rpart(formula = Exited ~., data = traintree, 
                     method = "class", parms = list(split = "gini"))
 

### Cross Validation
traintree_pred1 <- predict(model_tree1, data = traintree, type = "class")  
traintree_prob1 <- predict(model_tree1, data = traintree, type = "prob")  
testtree_pred1 <- predict(model_tree1, newdata= testtree, type = "class")  
testtree_prob1 <- predict(model_tree1, newdata = testtree, type = "prob") 
 

### **For the Training Set**
confusionMatrix(data = as.factor(traintree_pred1), reference = as.factor(traintree$Exited))
traintree_actual <- ifelse(traintree$Exited==1,1,0)
roc <- roc(traintree_actual, traintree_prob1[,2], plot= TRUE, print.auc=TRUE)
 

### **For the Test Set**
confusionMatrix(data = as.factor(testtree_pred1), reference = as.factor(testtree$Exited))
testtree_actual <- ifelse(testtree$Exited == 1, 1,0)
roc <- roc(testtree_actual, testtree_prob1[,2], plot = TRUE, print.auc = TRUE)

table <- matrix(c(0.859,0.860,0.406,0.394,0.975,0.981,0.759,0.750),ncol=2,byrow=TRUE)
rownames(table) <- c("Accuracy","Specificity","Sensitivity","AUC Value")
colnames(table) <- c("Training Set", "Test Set")
table 
 

###### Random Forest #####

### Train Model
set.seed(802)
modelrf1 <- randomForest(formula = as.factor(Exited) ~., data = traintree)
print(modelrf1)
 
### Cross Validation
trainrf_pred <- predict(modelrf1, traintree, type = "class")
trainrf_prob <- predict(modelrf1, traintree, type = "prob")  
testrf_pred <- predict(modelrf1, newdata = testtree, type = "class") 
testrf_prob <- predict(modelrf1, newdata = testtree, type = "prob") 
 

### for the training set
confusionMatrix(data = as.factor(trainrf_pred), reference = as.factor(traintree$Exited))
trainrf_actual <- ifelse(traintree$Exited == 1, 1,0)
roc <- roc(trainrf_actual, trainrf_prob[,2], plot= TRUE, print.auc=TRUE)
 

### for the test set
confusionMatrix(data = as.factor(testrf_pred), reference = as.factor(testtree$Exited))
testrf_actual <- ifelse(testtree$Exited == 1, 1,0)
roc <- roc(testrf_actual, testrf_prob[,2], plot = TRUE, print.auc = TRUE)
 

# table of results
table <- matrix(c(1,0.86,1,0.452,1,0.966,1,0.856),ncol=2,byrow=TRUE)
rownames(table) <- c("Accuracy","Specificity","Sensitivity","AUC Value")
colnames(table) <- c("Training Set", "Test Set")
table 
 
### Tuning 

#### Tuning mtry with tuneRF

set.seed(818)
modelrf2 <- tuneRF(x = subset(traintree, select = -Exited), y = as.factor(traintree$Exited),
                   ntreeTry = 500, doBest = TRUE)
print(modelrf2)
 
#### Grid Search based on OOB error

mtry <- seq(2, ncol(traintree) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(traintree) * c(0.7, 0.8)
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)
 
oob_err <- c()
for (i in 1:nrow(hyper_grid)) {
  model <- randomForest(formula = as.factor(Exited) ~ ., 
                        data = traintree,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
 

### Train model 2 with optimal hyperparameters.
set.seed(802)
modelrf3 <- randomForest(formula = as.factor(Exited) ~., data = traintree,
                         mtry = 4, nodesize = 5, sampsize = 5230.4)
print(modelrf3)

trainrf_pred2 <- predict(modelrf2, traintree, type = "class")
trainrf_prob2 <- predict(modelrf2, traintree, type = "prob")  
testrf_pred2 <- predict(modelrf2, newdata = testtree, type = "class") 
testrf_prob2 <- predict(modelrf2, newdata = testtree, type = "prob") 

# For the Training Set:
confusionMatrix(data = as.factor(trainrf_pred2), reference = as.factor(traintree$Exited))
trainrf_actual <- ifelse(traintree$Exited == 1, 1,0)
roc <- roc(trainrf_actual, trainrf_prob2[,2], plot= TRUE, print.auc=TRUE)

#For the Test Set
confusionMatrix(data = as.factor(testrf_pred2), reference = as.factor(testtree$Exited))
testrf_actual <- ifelse(testtree$Exited == 1, 1,0)
roc <- roc(testrf_actual, testrf_prob2[,2], plot = TRUE, print.auc = TRUE)


# Hence, we get the following table of results:
table <- matrix(c(1.000,0.859,1,0.452,1,0.965,1,0.855),ncol=2,byrow=TRUE)
rownames(table) <- c("Accuracy","Specificity","Sensitivity","AUC Value")
colnames(table) <- c("Training Set", "Test Set")
table 

# Final Table of Results for all 3 models

table <- matrix(c(0.703,0.860,0.859,0.700,0.394,0.452,0.704,0.981,0.965,0.780,0.750,0.855),ncol=3,byrow=TRUE)
rownames(table) <- c("Accuracy","Specificity","Sensitivity","AUC Value")
colnames(table) <- c("Logistic Regression", "Classification Tree","Random Forest")
table 


# Comparison of ROC and AUC for Logistic Regression, Classification Tree and Random Forest models

preds_list <- list(test_prob, testtree_prob1[,2],testrf_prob2[,2])
m <- length(preds_list)
actuals_list <- rep(list(testtree$Exited), m)

pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves for 3 Models")
legend(x = "bottomright",
       legend = c("Logistic Regression", "Classification Tree", "Random Froest"),
       fill = 1:m)
 

