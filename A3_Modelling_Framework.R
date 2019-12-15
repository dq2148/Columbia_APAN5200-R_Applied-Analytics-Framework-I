install.packages('ISLR');
install.packages('ggplot2'); 
install.packages('caret'); 
install.packages('caTools')

library(ISLR); 
library(ggplot2); 
library(caret); 
library(caTools)


#6
#Use simple random sampling to split the mpg dataset into a train and test sample with 80% of the data in the train sample. 
#Set the seed to 1706. The mpg dataset comes with ggplot2 package, so load the ggplot2 library first. 
#What is the difference in average highway gas mileage (hwy) between train and test samples?
head(mpg)
set.seed(1706)
split6 = sample(x = 1:nrow(mpg), size = 0.8*nrow(mpg))
split6[1:10]
train6 = mpg[split6,]
test6 = mpg[-split6,]
mean(train6$hwy)
mean(test6$hwy)
mean(train6$hwy) - mean(test6$hwy)

#7
#Use stratified sampling to split the mpg dataset into a train and test sample with 80% of the data in the train sample. 
#Do the sampling in such a way that the distribution of hwy is approximately equal in both samples. 
#Use a seed of 1706. Use createDataPartition for the split, set groups to 20, and list to F.
#What is the difference in average highway gas mileage (hwy) between train and test samples?
setseed(1706)
split7 = createDataPartition(y = mpg$hwy, p = 0.8, list = F)
train7 = mpg[split7,]
test7 = mpg[-split7,]
mean(train7$hwy)
mean(test7$hwy)
mean(train7$hwy) - mean(test6$hwy)

#8
#Use stratified sampling to split the OJ dataset into a train and test sample with 60% of the data in the train sample. 
#Ensure that the proportion of juices in the Purchase column is approximately equal across train and test samples. 
#Use a seed of 1706. Utilize sample.split() from the caTools package for this problem. 
#How many minute maid (MM) purchases are in the train dataset?
setseed(1706)
split8 = sample.split(Y = OJ$Purchase, SplitRatio = 0.6 )
train8 = OJ[split8,]
test8 = OJ[!split8,]
table(train8$Purchase)





















