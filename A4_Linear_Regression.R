library(ISLR); 
library(ggplot2); 
library(caret); 
library(caTools)
library(magrittr)
library(dplyr)
library(tidyr)

#Question 

#Section 1--------------------------------------------------------
setwd('~/Desktop/APAN5200')
houses = read.csv('houses.csv')
head(houses)

#1
#Let us start by splitting the data into a train and test sample such that 70% of the data is in the train sample. 
#Use createDataPartition from the caret package with groups = 100. 
#Set seed to 1031 and be sure to do this just before passing createDataPartition()
#What is the average house price in the train sample? Do not round your answer.
set.seed(1031)
split = createDataPartition(y = houses$price, p = 0.7, list = F, group = 100)
train = houses[split,]
test = houses[-split,]
mean(train$price)

#2
#What is the average house price in the test sample? Do not round your answer
mean(test$price)


#3
train %>%
  select(id,price:sqft_lot,age)%>%
  gather(key=numericVariable,value=value,price:age)%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales='free_y')

summary(train$bedrooms)
train$sqft_living[train$bedrooms == 33] #sqft_living for most bedrooms

#4
#scatterplot for sqft_living and price
library(ggplot2)
ggplot(data=houses,aes(x=sqft_living,y=price))+
  geom_point()+
  coord_cartesian(ylim=c(0,200000))

#5
cor(train$sqft_living, train$price)


#Section 2--------------------------------------------------------

#1
model1 = lm(price~sqft_living,data=train)
model1
summary(model1)

#3
pred1 = predict(model1)
pred1
rmse1 = sqrt(mean((pred1 - train$price)^2))
rmse1

#5
predict(model1, newdata=data.frame(sqft_living = 1400))
paste('price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'sqrt_livinng')

summary(model1)

#7-9
model2 = lm(price~waterfront,data=train)
summary(model2)
pred2 = predict(model2)
model2


#10
rmse2 = sqrt(mean((pred2 - train$price)^2))
rmse2

#Section 3--------------------------------------------------------
model3 = lm(price~sqft_living+waterfront, data = train)
summary(model3)
model3

model4 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,
            data = train)
summary(model4)
pred4 = predict(model4)
rmse4 = sqrt(mean((pred4 - train$price)^2))
rmse4

#coef for bathrooms
coef(model4)[3]
summary(model4)

#strongest influence
library(lm.beta)
lm.beta(model4)

pred5 = predict(model4, newdata = test)

sse5_test = sum((pred5 - test$price)^2)
sst5_test = sum((mean(train$price)-test$price)^2)
model5_r2_test = 1 - sse5_test/sst5_test
model5_r2_test

rmse5_test = sqrt(mean((pred5-test$price)^2));
rmse5_test
