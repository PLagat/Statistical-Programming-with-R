#QUESTION ONE
#importing dataset from excel
install.packages("readxl") 
library(readxl)
dataset <-read_excel("D:/PROGRAMMING METHODOLOGY/Data.xlsx") #to load the data for this question
View(dataset)
attach(dataset)

#(a)
install.packages("ggplot2")
library(ggplot2)         #to load package
ggplot(dataset, aes(Ksh))  #the aes has only one input as the data has only 1 variable
a <- ggplot(dataset, aes(Ksh))
#Geom is used to represent data points.#Each function returns a layer
a + geom_area(stat = "bin") + 
        geom_density(kernel="gaussian") + 
        ggtitle("Question 1(a) plot")   #to give the plot a title

       
#(b) Creating the histogram using the hist() function
  
colors = c("red","blue")     #to specify colors in histogram

hist(dataset$Ksh, col = colors, main = "Histogram of money data", xlab ="Kenya Shillings",ylab ="Frequency" )


      
#QUESTION TWO
View(cars) #to view the dataset
input <- cars[,c('speed','dist')] #to get the input values

plot(x = input$dist , y = input$speed, xlab = "distance",ylab = "speed", main = "scatterplot for speed agaist distance")

#(b)Finding correlation
speed <- input$speed
distance <- input$dist
cor(speed,distance) #To get the correlation between speed and distance
#There is a positive correlation of 0.8068949

#(c) To get the line of best fit, we use abline() and lm() functions
col = c("red")  #color of the line of best fit
abline(lm(speed ~ distance, data = cars), col = "red") #this plots the line of best fit

#(d)
model <- lm(speed~distance) #Linear regression on the two variables
summary(model) #to get the p-value
1.49e-12 #This is the P-value of the model
#since the P-value is less than 0.05, the relationship is statistically significant

#The data shows an uphill pattern as we move from left to right, this indicates a positive relationship between distance and speed
#As distance increases, speed tends to increase
#The correlation coefficient = 0.8068949 indicates a strong positive relationship between speed and distance. 
# This strong correlation shows that the probability that the relationship between speed and distance is equal to zero is very low.(low p-values)



#QUESTION THREE
library(readr)
Salaries <- read_csv("D:/PROGRAMMING METHODOLOGY/Salaries.csv")
View(Salaries)
attach(Salaries)

#(a) tO get summary statistics
#(i)
table(Salaries$rank) #gives the values for the number of professors=266, associate professors=64, assistant professors=67
#(ii)
table(Salaries$sex) #to give number of male and female staff



#(B)(i)Measures of central tendency
summary(salary) #returns the mean,median, range, interquartile range

#Alternatively,
mean(salary) #to find average of staff salary = 113706.5
median(salary) #to find middle number of staff salary= 107300
names(table(salary))[table(salary)==max(table(salary))] #to find the mode which is 92000

#(B)(ii) Measures of dispersion
sd(salary) #To find standard deviation = 30289.04 
range(salary) #the range of staff salary is from 57800 to 231545
IQR(salary) #The interquartile range is 43185



#C)
#(i)Plotting years of service against salaries
plot(yrs.service,salary, type = "p",col="red", main = "Years of service against salary", xlab = "Years of service")

#(ii)Plotting dotplot for rank against salary
library(ggplot2)
ggplot(Salaries,aes(rank,salary))
d <- ggplot(Salaries,aes(rank,salary))
d + geom_dotplot(binaxis = "y",stackdir = "center", binwidth = 1.5) + geom_point(color = "red", fill = "black", size = 3) + ggtitle("Dotplot for rank against salary")
#Alternatively, geom_point() function can be used as follows
ggplot(Salaries) + geom_point(aes(x = rank, y = salary),color = "red", fill = "black", size = 1.5) + ggtitle("Dotplot for rank against salary")

#From the two graphs, we can deduce that there is a strong relationship between the two variables in both cases and the correlation is positive