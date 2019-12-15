library(ISLR); 
library(ggplot2); 
library(caret); 
library(caTools)
library(magrittr)
library(dplyr)
library(tidyr)
library(ROCR)


setwd('~/Desktop/APAN5200')
data = read.csv('eBayAssignment.csv')
summary(data)

#SECTION 1 ----------
#1
nrow(data)

#2
summary(data$color == 'Black')

#3
summary(data$productline)

#4
summary(data$startprice)
data$UniqueID[data$startprice == 999]


#SECTION 2 ----------

#1 split data -sample.split

set.seed(196)
split = sample.split(data$sold, SplitRatio = 0.8)
train = data[split,]
test = data[!split,]

nrow(train)
nrow(test)
nrow(data)


#2
tapply(train$startprice,train$sold,median)

#3-4
tapply(train$startprice,train$sold,median)

#5
model1 = glm(sold~biddable+startprice+condition+cellular+carrier+color+
                 storage+productline+noDescription+upperCaseDescription+startprice_99end,
               data=train,
               family='binomial')
summary(model1)$aic

#5-7
summary(model1)


#SECTION 3 ----------
#1
model2 = glm(sold~biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end,
             data = train,
             family = 'binomial')

summary(model2)$aic

#2
summary(model2)

#3
exp(summary(model2)$coef[10]) 

#4
100*(exp(summary(model2)$coef[3])-1)

#5
exp(summary(model2)$coef[12]) 
100*(exp(summary(model2)$coef[12])-1) 

#6
model_productline = glm(sold~productline, data = train, family = 'binomial')
summary(model_productline)
summary(model2)

#SECTION 4 ----------

#1 run model2 in test with UniqueID
pred = predict(model2, test, type='response')
pred[test$UniqueID==10940]

#2 Accuracy for test
ct = table(sold = test$sold,
           predictions = as.numeric(pred>0.5))
ct

accuracy = sum(ct[1,1],ct[2,2])/nrow(test)
accuracy

#3
baseline = table(test$sold)[1]/nrow(test)
baseline

baseline = sum(test$sold==0)/nrow(test)
baseline

#4
accuracy < baseline

#5
ROCRpred = prediction(pred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve






















