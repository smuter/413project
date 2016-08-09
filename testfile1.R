# Stuart Muter
# Predict 413
# final project, file 1
#

#8/08/16
##
##


# If necessary, install packages
install.packages("psych")
install.packages("ggplot2")
install.packages("ElemStatLearn")
install.packages("multilevel")
install.packages("lsr")
install.packages("xlsx")
install.packages("XML")
install.packages("data.table") 
install.packages("plyr")
install.packages("pscl")
install.packages("rpart")
install.packages("fma")
install.packages("forecast")
install.packages("car")
install.packages("MASS")
install.packages("TTR")
install.packages("lubridate")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)
library(lsr)
library(xlsx)
library(XML)
library(data.table) 
library(plyr) 
library(pscl) 
library(rpart)
library(fma)
library(forecast)
library(car)
library(MASS)
library(TTR)
library(lubridate)


# this is a test

# test
# discussion 7, loading monthly closing price data for CH Robinson (CHRW) that was downloaded through yahoo finance
newData<- read.table("./CHRW data.csv", sep=",", header = TRUE)

# check what was loaded
head(newData)


# creating new columns for month and year
newData$month <- month(mdy(newData$Date))
newData$year <- year(mdy(newData$Date))

# ordering data to end at most recent month
newData<- newData[order(as.Date(newData$Date,format="%m/%d/%y"), decreasing = FALSE), ]

# attach the variables & simplify the close column
attach(newData)  #attach the variables to address them directly outside of the data frame
close=Adj.Close

# check on the data
print(newData)


# see if it is seasonal, plot2
boxplot(close ~ month, col = 1:12, main = "Seasonal?", notch = T)


# create a time series for easier plotting
Chrw_TS <- ts(close, f =12, start = c(2011,8))

# plot 1
plot(Chrw_TS, main="Monthly Ch Robinson (CHRW) stock price from 2011-08 to 2016-07 ",
     ylab="", xlab="Month")

# additive & multiplicative decompose
Chrw.decomp<- decompose(Chrw_TS,type = "additive")
plot(Chrw.decomp)

Chrw.decomp2<- decompose(Chrw_TS,type = "multiplicative")
plot(Chrw.decomp2)
summary(Chrw.decomp2)

# build test and training sets
# training set to end of 2015, test set to be first 7 months of 2016
train_TS <- window(Chrw_TS, end = c(2015,12))
test_TS <- window(Chrw_TS, start = c(2016,1))


# holt winters, additive, multiplicative and damped multiplicative
fit1 <- hw(train_TS, h =7, seasonal="additive")
fit2 <- hw(train_TS, h = 7, seasonal="multiplicative")
fit3 <- hw(train_TS, h = 7, seasonal="multiplicative", damped = TRUE)

help(hw)
help(decompose)

# plot HW models
plot(Chrw_TS,ylab="CHRW stock HW methods",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year", ylim = c(45,75))
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fitted(fit3), col="blue", lty=2)
lines(fit1$mean, type="o", col="red")
lines(fit2$mean, type="o", col="green")
lines(fit3$mean, type="o", col="blue")
legend("topleft",lty=1, pch=1, col=1:4, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative", "Holt Winters Damped"))

# compare the accuracies:
accuracy(fit1, test_TS)
accuracy(fit2, test_TS)
accuracy(fit3, test_TS)

# try an ets model
# use auto default

fit4 <- ets(train_TS)
fcast <- forecast(fit4, h = 7)
accuracy(fcast, test_TS)


# plot the ets model, plot 3
plot(fcast, col = "black")
lines(test_TS, col = "red")

# up to here
