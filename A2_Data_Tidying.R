library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd('~/Desktop/APAN5200')
diamondsData = read.csv('diamonds_tidying.csv')

#What is the average carat size of a diamond?
mean(diamonds$carat)

#What is the average carat size of an Ideal cut diamond?
  #solution 1
mean(diamondsData$carat[diamondsData$cut == 'Ideal'])
  #solution 2
idealcut = subset(diamondsData, cut == "Ideal")
mean(idealcut$carat)

#Which cut of diamond has the largest variance in carat size?
var(diamondsData$carat[diamondsData$cut == 'Ideal'])
var(diamondsData$carat[diamondsData$cut == 'Premium'])
var(diamondsData$carat[diamondsData$cut == 'Good'])
var(diamondsData$carat[diamondsData$cut == 'Very Good'])
var(diamondsData$carat[diamondsData$cut == 'Fair'])

#Compare number of diamonds by cut but only for color "D". 
#Which cut has the greatest selection (i.e., highest count) in color D?
colord = subset(diamondsData, color == "D")
count(subset(colord, cut == "Fair")) 
count(subset(colord, cut == "Good"))
count(subset(colord, cut == "Very Good"))
count(subset(colord, cut == "Premium"))
count(subset(colord, cut == "Ideal"))

#What is the average (i.e., mean) price in Euros of diamonds larger than 1 carat? 
#Assume the conversion is $1 = Euro 0.85.
larger_mean = mean(diamondsData$price[diamondsData$carat > 1])
Euro_conversion = larger_mean * 0.85
Euro_conversion


#Construct a density curve of price. 
#Now, add faceting based on cut. Hint: facet_grid(cut~.)
#Next, construct a similar density curve but instead of using faceting, add cut as a color aesthetic.
#Based on these plots, indicate your agreement with the following statement:
  #Ideal cut diamonds tend to be more expensive than Fair cut diamonds
ggplot(diamondsData, aes(x = price)) +
  geom_density() +
  facet_grid(cut~.)
ggplot(diamondsData, aes(x = price, color = cut)) +
  geom_density() 


#Based on these plots, indicate your agreement with the following statement:
#Ideal cut diamonds tend to be larger (i.e., higher carat) than Fair cut diamonds.
ggplot(diamondsData, aes(x = carat)) +
  geom_density() +
  facet_grid(cut~.)
ggplot(diamondsData, aes(x = carat, color = cut)) +
  geom_density() 


ggplot(data=diamondsData,aes(x=carat))+ 
  geom_histogram(binwidth = 0.01)+
  coord_cartesian(xlim=c(0,2.5))+
  scale_x_continuous(breaks=seq(0,2.5,0.1))


#We want to compute the average of variable x. 
#However, some of the values for x are 0, which is not possible. 
#So, we would like to compute the average without these 0 values. 
#Which of the following options will achieve this goal?
diamonds_new = diamondsData 
diamonds_new$x[diamonds_new$x==0] = NA
mean(diamonds_new$x, na.rm=T)















