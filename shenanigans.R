
load("D:\\INTRODUCTORY ECONOMETRICS\\Data files_Ch 4 -20200709\\R data files Ch 4\\hprice1.RData")
View(desc)
attach(data)
model <- lm(price~sqrft+bdrms)
summary(model)
#price = -19.31500 +  0.12844sqrft + 15.19819bdrms
predict(model,data[1,])
predict(model,data.frame(sqrft=2438,bdrms=4))
#residual = price- pricehat
residuals(model)
confint(model, level=0.99)


#Chapter 4
log.model <- lm(lprice~sqrft+bdrms)
summary(log.model)
nrow(data)

#log(price) =  4.766e+00 + 3.794e-04sqrft + 2.888e-02bdrms
#add one more bedroom that is 150sqrft

sb <- sqrft - 150*bdrms
model3 <- lm(lprice~sb+bdrms)
summary(model3)

#log(price) = 4.766 + 0.0003794sb + 0.0858bdrms
#standard error = 0.02677
#confidence interval = Bj + c.se(Bj)
#confidence interval = Bj - c.se(Bj)

confint(model3)
#0.0325803516 <bdrms< 0.1390223318


#Joint hypothesis test
#Ho: B2=0 and B3=0
#H1: at least one of the Bj is not equal to zero

#F-stat = 60.73
#F-critical value
qf(0.95,2,85)

#t-stat
#t-critical value
abs(qt(0.025,85))
qt(0.975,85)



#Heteroskedasticity
setwd("D:/INTRODUCTORY ECONOMETRICS/RData")

setwd("D:/INTRODUCTORY ECONOMETRICS")
library(foreign)
Gas1 <- read.dta("Gas1.dta")
View(Gas1)
attr(Gas1, "var.labels")

#Breusch-Pagan test
attach(Gas1)
model4 <- lm(pcon~reg+price)
summary(model4)
resid <- residuals(model4)
residsq <- resid*resid
model5 <- lm(residsq~reg+price)
summary(model5)
residuals(model5)
#Ho:a1=0 and a2=0
#LM statistic N*R = 50*0.197 = 9.85
#Critical value
#H1: