library(ISLR); 
library(ggplot2); 
library(caret); 
library(caTools)
library(magrittr)
library(dplyr)
library(tidyr)



setwd('~/Desktop/APAN5200')
wages = read.csv('assignment7_wages.csv')

#SECTION 1 --------------------
#1
summary(wages)

#2 fraction for females and males
wages = wages[wages$earn > 0,]
summary(wages$sex)
859/(509+859)

#3 Which of following races earns the least?
summary(wages$race)
wages %>%
  group_by(race) %>%
  summarize(mean(earn))

table(wages$race)
mean(wages$earn[wages$race == "black"])
mean(wages$earn[wages$race == "hispanic"])
mean(wages$earn[wages$race == "other"])
mean(wages$earn[wages$race == "white"])

#4
set.seed(1731)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]

nrow(train)


#SECTION 2 --------------------
#1 linear regression for all variables
model1 = lm(earn~., data = train)
summary(model1)

#2 rmse for model1
pred1 = predict(model1)
sse1 = sum((pred1 - train$earn)^2); sse1
sst1 = sum((mean(train$earn)-train$earn)^2); sst1
model1_r2 = 1 - sse1/sst1; model1_r2
rmse1 = sqrt(mean((pred1-train$earn)^2)); rmse1


#3 #4 
#What is the approximate difference in earn between 12 years and 16 years of education for Males?
library(ggplot2)
ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+ 
  geom_bar(stat="summary",fun.y="mean",position="dodge")

ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))


#5 #6
#Which of the following variables are significant?
model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)
summary(model_sex_ed)

#7 #8 rmse for model 2
model2 = lm(earn ~. + sex*ed, data = train)
pred2 = predict(model2)
sse2 = sum((pred2 - train$earn)^2); sse2
sst2 = sum((mean(train$earn)-train$earn)^2); sst2
model2_r2 = 1 - sse2/sst2; model2_r2
rmse2 = sqrt(mean((pred2-train$earn)^2)); rmse2

#9
model3 = lm(earn ~. + sex*ed + sex*age, data = train)
pred3 = predict(model3)
sse3 = sum((pred3 - train$earn)^2); sse3
sst3 = sum((mean(train$earn)-train$earn)^2); sst3
model3_r2 = 1 - sse2/sst2; model3_r2
rmse3 = sqrt(mean((pred3-train$earn)^2)); rmse3

#10
model4 = lm(earn ~ height + sex + race + ed + age + sex*ed + sex*age + age*ed, data = train)
pred4 = predict(model4)
sse4 = sum((pred4 - train$earn)^2); sse4
sst4 = sum((mean(train$earn)-train$earn)^2); sst4
model4_r2 = 1 - sse4/sst4; model4_r2
rmse4 = sqrt(mean((pred4-train$earn)^2)); rmse4


#11
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
pred5 = predict(model5)
sse5 = sum((pred5 - train$earn)^2); sse5
sst5 = sum((mean(train$earn)-train$earn)^2); sst5
model5_r2 = 1 - sse4/sst5; model4_r2
rmse5 = sqrt(mean((pred5-train$earn)^2)); rmse5

#12
summary(model5)


#SECTION 3 --------------------

#1-4
tree1 = rpart(earn ~ height + sex + race + ed + age, data = train)
rpart.plot(tree1, digits=5)

#5 rmse for tree1
rmse_tree1 = sqrt(mean((train$earn - predict(tree1)) ^ 2)); rmse_tree1

#6
treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
rpart.plot(treeSimp1, digits=5)

#7
rmse_treeSimp1 = sqrt(mean((train$earn - predict(treeSimp1)) ^ 2)); rmse_treeSimp1

#8
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
rpart.plot(treeSimp2, digits=5)

#9
rmse_treeSimp2 = sqrt(mean((train$earn - predict(treeSimp2)) ^ 2)); rmse_treeSimp2

#10
treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
rpart.plot(treeComplex1, digits=5)

rmse_treeComplex1 = sqrt(mean((train$earn - predict(treeComplex1)) ^ 2)); rmse_treeComplex1

#11
treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
rpart.plot(treeComplex2, digits=5)

rmse_treeComplex2 = sqrt(mean((train$earn - predict(treeComplex2)) ^ 2)); rmse_treeComplex2


#SECTION 4 --------------------

#1
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
pred5_test = predict(model5,newdata=test)
rmse5_test = sqrt(mean((test$earn - pred5_test) ^ 2)); rmse5_test

#2
rmse_tree1_test = sqrt(mean((test$earn - predict(tree1, newdata = test)) ^ 2)); rmse_tree1_test

#3
rmse_treeSimp2_test = sqrt(mean((test$earn - predict(treeSimp2, newdata = test)) ^ 2)); rmse_treeSimp2_test

#4
rmse_treeComplex2_test = sqrt(mean((test$earn - predict(treeComplex2, newdata = test)) ^ 2))
rmse_treeComplex2_test