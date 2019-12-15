library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(readr)
install.packages('rpart.plot')
library(rpart.plot)

#Not Sure- 9, 17, 20, 21

setwd('~/Desktop/APAN5200')
data = read.csv('versionA.csv')

head(data)
set.seed(617)
split = sample(1:nrow(data), nrow(data)*0.75)
train = data[split,]
test = data[-split,] 

#2 How many observations are in the train sample?
nrow(train)

#3 What is the class for “price_hilo”?
class(train$price_hilo)

#4 What is the mean price for diamonds of depth greater than 50 and color J? 
#  Use the train sample.
train %>%
  filter(color == "J") %>%
  filter(depth > 50) %>%
  summarise(mean(price))

#5 Construct a scatter plot of carat (on the x-axis) and price (on the y-axis). 
#  Use the train sample.
ggplot(data = train, aes(x = carat, y = price)) + 
  geom_point()

#6 What is the correlation between table and x? Use the train sample.
cor(train$table, train$x)

#7 Build a linear regression model to predict price using x. 
#  Call this model1. Use the train sample.
#  What is the model R2 (R-squared)?
model1 = lm(price~x, data = train)
summary(model1)

#8 Which of the following is the correct interpretation for the coefficient of x in model1?
coef(model1)[2]
summary(model1)

#9 Build a linear regression model to predict price using color. 
#  Call this model2. Use the train sample. 
model2 = lm(price~color, data = train)

ggplot(data=train, aes(x=price)) +
  geom_density(size=1.2)+
  facet_grid((color~.))

ggplot(train, aes(x = price, color = color)) +
  geom_density() 

#10 Build a linear regression model to predict price 
#   using all variables EXCEPT price_hilo, x, y, and z. 
#   Call this model3. Use the train sample.
#   What is the R2 (R-squared) for model3?
str(train)
model3 = lm(price ~ carat + cut + color + clarity + depth + table, data = train)
summary(model3)

#11 Based on model3, which of the following variables have a significant effect on price? 
#   (Check all that apply)
summary(model3)

#12 Based on model3, what is the predicted price for the first diamond in the train sample?
head(predict(model3))
predict(model3, head(train,n=1))

#13 Based on model3, what is the rmse (root mean squared error) for the test sample?
pred3 = predict(model3, newdata = test)
sse3 = sum((pred3 - test$price)^2)
sst3 = sum((mean(test$price)-test$price)^2)
model3_r2 = 1 - sse3/sst3; model3_r2
rmse3 = sqrt(mean((pred3-test$price)^2))
rmse3

#14 Now, construct a linear regression model to predict price using the following predictors: 
#   carat, depth, table, x, y, z. Use the train sample. 
#   Call this model4.
#   Which predictor has the second highest Variance Inflation Factor (VIF)?
model4 = lm(price ~ carat + depth + table + x + y + z, data = train)

install.pakcages('car')
library(car)
vif(model4)

#15 Run a hybrid stepwise regression using the same predictors as in model4.
#   Which of the following predictors are included in this model? (Check all that apply)
start_mod = lm(price ~ 1,data = train)
empty_mod = lm(price ~ 1,data = train)
full_mod = lm(price ~ carat + depth + table + x + y + z, data = train)
hybridStepwise = step(start_mod,
                      scope = list(upper = full_mod,lower = empty_mod),
                      direction = 'both')

#16 Now, construct a logistic regression model to predict price_hilo using the following predictors: 
#   carat, cut, color, clarity, depth, and table. 
#   Use the train sample. Call this model5. 
#   For the purpose of the exam, ignore the warning message from running this logistic regression model.
#   What is the AIC for model5?
model5 = glm(price_hilo ~ carat + cut + color + clarity + depth + table, 
             data = train, family = 'binomial')
summary(model5)$aic

#17 Compared to a Fair cut diamond, 
#   how much more likely is a Good cut diamond to have a high price?
exp(summary(model5)$coef[3]) 
100*(exp(summary(model5)$coef[3])-1)

#18 What is the accuracy of model5 in the test sample?
#   Use a threshold of 0.35.

pred5 = predict(model5, test, type='response')
ct5 = table(price_hilo = test$price_hilo,
            predictions = as.numeric(pred5>0.35))
accuracy5 = sum(ct5[1,1],ct5[2,2])/nrow(test)
accuracy5

#19 - 21
#   Construct a classification tree (NOT regression tree) to predict price_hilo using the following predictors:
#   carat, cut, color, clarity, depth and table. Use the train sample. 
#   Do not specify cp, or minbucket, and do not cross-validate. 
#   Call this model6.
#   Which is the most important predictor of price_hilo?
model6 = rpart(price_hilo ~ carat + cut + color + clarity + depth + table, 
               data = train, method = 'class')
summary(model6)
rpart.plot(model6)

predict(model6,newdata = data.frame(carat = 0.85),type = 'class')
predict(model6,newdata = data.frame(carat = 1.25),type = 'class')


#22 Construct a random forest model to predict price_hilo with the same predictors used in model6: 
#   carat, cut, color, clarity, depth and table. 
#   Use factor(price_hilo) as outcome instead of price_hilo. 
#   Use the train sample. Set number of trees to 200 and use a seed of 1031. 
#   Do not specify mtry. Do not tune or cross-validate. Call this model7.
library(randomForest)
set.seed(1031)
model7 = randomForest(factor(price_hilo) ~ carat + cut + color + clarity + depth + table,
                      data = train, ntree = 200) 
summary(model7)
pred7 = predict(model7, newdata=test, type="prob")[,2]
importance(model7)

#23 For model7, what is the Area under the ROC curve (AUC) in the test sample?
ROCRpred7 = prediction(pred7, test$price_hilo)
as.numeric(performance(ROCRpred7,"auc")@y.values)

#24 Use a Support Vector Machine to predict price_hilo using the same predictors as in model7: carat, cut, color, clarity, depth and table. 
#   Use factor(price_hilo) as outcome instead of price_hilo. 
#   Use the train sample. Use a radial basis function kernel. 
#   Do not set cost, gamma or coef0, in other words, use defaults. 
#   Do not tune or cross-validate. Call this model8.
model8 = svm(factor(price_hilo) ~ carat + cut + color + clarity + depth + table,
             data = train, kernel='radial', type='C-classification')
summary(model8)
pred8 = predict(model8)
table(pred8,train$price_hilo)

#25 What is the accuracy of model8 in the test sample?
pred8_test = predict(model8, newdata = test)
ct8 = table(pred8_test, test$price_hilo)
accuracy8 = sum(ct8[1,1],ct8[2,2])/nrow(test)
accuracy8
accuracy_model8 = mean(pred8_test == test$price_hilo)
accuracy_model8




