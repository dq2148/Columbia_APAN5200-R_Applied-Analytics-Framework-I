library(ISLR)
library(dplyr)
library(rpart)
library(ROCR)
library(caret)
data(OJ)
View(OJ)

library(rpart); library(rpart.plot)


#SECTION 1 --------------------
#1
set.seed(1234)
split = sample.split(Y = OJ$Purchase, SplitRatio = 0.7)
train = OJ[split,]
test = OJ[!split,]
nrow(train)


#2
head(train)
summary(train$Purchase)

#3 
#What is the average Price for Minute Maid (in the train sample)? 
mean(train$PriceMM)

#4
#What is the average Discount for Minute Maid (in the train sample)? 
mean(train$DiscMM)

#5 
#How many purchases of Minute Maid were made in Week 275?
head(train)
count(train %>%
        filter(WeekofPurchase == 275) %>%
        filter(Purchase == 'MM'))


#SECTION 2 --------------------

#1
classtree1 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                     SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, 
                   data = train)

rpart.plot(classtree1)
pred_classtree1 = predict(classtree1,newdata=test)
ROCRpred_classtree1 = prediction(pred_classtree1[,2],test$Purchase)
as.numeric(performance(ROCRpred_classtree1,"auc")@y.values) 

#2
set.seed(100)
trControl = trainControl(method="cv",number=10) 
tuneGrid = expand.grid(.cp=seq(0,0.1,0.001))  
trainCV = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                  SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data=train,
                method="rpart", trControl=trControl,tuneGrid=tuneGrid)
trainCV$bestTune


#3
treeCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                 SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data=train,
               control=rpart.control(cp=trainCV$bestTune))
predCV = predict(treeCV,newdata=test)
ROCRpredCV = prediction(predCV[,2],test$Purchase)
as.numeric(performance(ROCRpredCV,"auc")@y.values) 

#SECTION 3 --------------------
#1
library(randomForest)
set.seed(617)
bag = randomForest(PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                     SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                   data=train.,data=train,mtry = ncol(train)-1,ntree=1000)
predBag = predict(bag,newdata=test, type = 'prob')
ROCRpred_bag = prediction(predBag[,2],test$Purchase)
as.numeric(performance(ROCRpred_bag,"auc")@y.values) 


#2
library(randomForest)

set.seed(617)
forest = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                        SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                      data=train,ntree = 1000)
predForest = predict(forest,newdata=test, type = 'prob')
ROCRpredForest = prediction(predForest[,2],test$Purchase)
as.numeric(performance(ROCRpredForest,"auc")@y.values) 

#3
train$Purchase2 = as.numeric(train$Purchase)-1
test$Purchase2 = as.numeric(test$Purchase)-1
library(gbm)
set.seed(617)
boost = gbm(Purchase2~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
              SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
            data=train,distribution="bernoulli",
            n.trees = 1000,interaction.depth = 1,shrinkage = 0.04)
predBoost = predict(boost,newdata=test,n.trees = 100, type = 'response')
ROCRpredBoost = prediction(predBoost,test$Purchase2)
as.numeric(performance(ROCRpredBoost,"auc")@y.values) 


