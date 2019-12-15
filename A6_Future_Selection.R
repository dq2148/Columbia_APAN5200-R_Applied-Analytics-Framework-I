library(ISLR); 
library(ggplot2); 
library(caret); 
library(caTools)
library(magrittr)
library(dplyr)
library(tidyr)
library(ROCR)
library(leaps)

setwd('~/Desktop/APAN5200')
houses = read.csv('houses.csv')

summary(houses)

#SECTION 1 ----------

#1
set.seed(1031)
split = createDataPartition(y = houses$price, p = 0.7, list = F, groups = 100)
train = houses[split,]
test = houses[-split,]

mean(train$price)


#2
str(train)
cor(train[,-16])
round(cor(train[,-16]), 2)*100
library(tidyr); library(dplyr); library(ggplot2)
corMatrix = as.data.frame(cor(train[,-16]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')

#3
library(corrplot)
corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)



#4
cor(train$sqft_living, (train$sqft_above + train$sqft_basement))

#5
model1 = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+
              view+condition+grade+age,train)
vif(model1)



#SECTION 2 ----------
#1
subsets = regsubsets(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                       waterfront + view + condition + grade + age, data=train, nvmax=10)
summary(subsets)

#2 Subsets for best predictors, R^2 for subsets
names(summary(subsets))
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)
subsets_measures
model1 = lm(price~sqft_living + grade + age + waterfront + view + bedrooms, data = train)
summary(model1)

#3 Future selection for best predictors
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                waterfront + view + condition + grade + age,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')

summary(forwardStepwise)

#4 Backward selection
start_mod1 = lm(price ~., data = train)
empty_mod = lm(price ~ 1, data = train)
full_mod = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                waterfront + view + condition + grade + age, data = train)
backwardStepwise = step(start_mod1,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
summary(backwardStepwise)


#5 hybrid stepwise 
start_mod = lm(quality~1,data=train)
empty_mod = lm(quality~1,data=train)
full_mod = lm(quality~bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                waterfront + view + condition + grade + age,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')

summary(hybridStepwise)

#6 LASSO Model
x = model.matrix(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                   waterfront + view + condition + grade + age,data = train)
y = train$price

lassoModel = glmnet(x,y, alpha=1) 
plot(lassoModel,xvar='lambda',label=T)
plot(lassoModel,xvar='dev',label=T)
set.seed(1031)
cv.lasso = cv.glmnet(x,y,alpha=1) 
plot(cv.lasso)
coef(cv.lasso)

#7 r^2 for LASSO
model2 = lm(price~bathrooms + sqft_living + waterfront + view + grade + age, data = train)
summary(model2)

#8
library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

str(trainComponents)

#9
trainmodel = lm(price~.,trainComponents)
pred = predict(trainmodel)
sse = sum((pred-train$price)^2)
sst = sum((mean(train$price) - train$price)^2)
r2 = 1 - (sse/sst)
r2


#10
testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price
pred1 = predict(trainmodel,newdata=testComponents)
sse1 = sum((pred1-testComponents$price)^2)
sst1 = sum((mean(trainComponents$price) - testComponents$price)^2)
r2_test = 1 - sse1/sst1; r2_test

