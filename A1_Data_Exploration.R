#Compute 12345678 multiplied by 87654321 (Copy-paste answer from R) 
12345678*87654321

#What is the square root of 111222333444555666. Do not round answer from R.
sqrt(111222333444555666)

#What is the log to base 10 of 111222333444555666. Do not round answer from R.
log10(111222333444555666)

#What is the class for 5?
class(5)

#What is the class for 'FIVE'?
class('FIVE')

#What is the class for FALSE?   
class(FALSE)

#What class will the following statement return? Please read question carefully.
class(45 == 56)

#What class will the following statement return? Please read question carefully.
#as.numeric(F)
class(as.numeric(F))

#What will the following code yield?
#c(10,20,30,40) > 15
c(10,20,30,40) > 15

#What will the following code yield?
#c(10,20,30,40) > c(15,25,35,45)
c(10,20,30,40) > c(15,25,35,45)

#What will the following code yield?
#c(10,20,30,40) > c(15,25)
#(Note: This question illustrates recycling behavior in R)
c(10,20,30,40) > c(15,25)


#What is the total number of smartwatches sold during the week (Hint: Use sum())
day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold =  c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)
table(df)

sum(number_of_smartwatches_sold)
df[df$day_of_week=='Sunday','price_per_smartwatch']

revenue_by_day = number_of_smartwatches_sold*price_per_smartwatch
sum(revenue_by_day)
sum(number_of_smartwatches_sold > 25)

price_per_smartwatch[c(6,7)]
#return the days when sales were greater than average
df$day_of_week[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold)]
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),"day_of_week"]
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),1]
  