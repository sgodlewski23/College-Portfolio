#1
library("foreign")
library(readr)
library("ggplot2")
insurance <- read_csv("insurance.csv")

View(insurance)

#2
insurance$sex <- factor(insurance$sex, level = c("male","female"), labels = c("0","1"))
insurance$smoker <- factor(insurance$smoker, level = c("no","yes"), labels = c("0","1"))
insurance$region <- factor(insurance$region, level = c("southwest", "southeast","northwest","northeast"), labels = c("1","2","3","4"))

#3
mydata_1 <- insurance[which(insurance$region=='1'),]
mydata_2 <- insurance[which(insurance$region=='2'),]
mydata_3 <- insurance[which(insurance$region=='3'),]
mydata_4 <- insurance[which(insurance$region=='4'),]

#4
lm.fit1 <- lm(charges ~ age + bmi + children, data = mydata_1, na.action = na.omit)
summary(lm.fit1)




lm.fit2 <- lm(charges ~ age + bmi + children, data = mydata_2, na.action = na.omit)
summary(lm.fit2)





lm.fit3 <- lm(charges ~ age + bmi + children, data = mydata_3, na.action = na.omit)
summary(lm.fit3)




lm.fit4 <- lm(charges ~ age + bmi + children, data = mydata_4, na.action = na.omit)
summary(lm.fit4)





#5
#SouthEast
lm.fit2 <- lm(charges ~ age + bmi + children, data = mydata_2, na.action = na.omit)
summary(lm.fit2)



#6
#SouthWest
lm.fit.mydata_1 <- lm(charges ~ age + sex + bmi + children + smoker, data = mydata_1, na.action = na.omit)
summary(lm.fit.mydata_1)


par("mar")

par(mfrow = c(2,2), mar=c(4,4,4,4))

#Residual vs Fitted Plot

plot(lm.fit.mydata_1, which = 1)

#normal Q-Q

plot(lm.fit.mydata_1, which = 2)

#scale-location

plot(lm.fit.mydata_1, which = 3)

#residuals vs leverage plots

plot(lm.fit.mydata_1, which = 5)
