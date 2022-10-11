
#Time Series Plot
library(readr)
DataEmployment_1_ <- read_csv("DataEmployment(1).csv")

View(DataEmployment_1_)


CPI<-ts(data=DataEmployment_1_, start=c(1,1), frequency=4)





print(CPI)

View(CPI)
#
plot(CPI, main= "Consumer Price Index", xlab = "Days", ylab = "CPI")


View(ts)

time <- data.frame(c( 1:length(CPI)))
names(time)[1] <- "time"




#Linear Model

CPIAUSCL<-data.frame(time, CPI)

linear.mod <- lm(CPI~time, data=CPIAUSCL)
print(summary(linear.mod))






#Quadratic Model


quad.mod <- lm(formula = CPI ~ time + time^2, data = CPIAUSCL)
print(summary(quad.mod))

mond <- seasonaldummy(CPI)
logls.seas <- data.frame(cbind(CPI, time, mond))
linear.seas.mod <- lm(CPI~ mond + time, data = logls.seas)
print(summary(linear.seas.mod))



## Log-quadratic trend estimation with seasonal dummies
mond <- seasonaldummy(CPI)
time2 <- time^2
logqls.seas <- data.frame(cbind(CPI,time,time2, mond))
quad.seas.mod <- lm(formula = CPI ~ time + time2 + mond, data = logqls.seas)
print(summary(quad.seas.mod))


AIC <- AIC(linear.mod,quad.mod,quad.seas.mod,linear.seas.mod)
BIC <- BIC(linear.mod,quad.mod,quad.seas.mod,linear.seas.mod)
mod.selection <- data.frame(cbind(AIC,BIC[,2]))
names(mod.selection)[3] <- "AIC"
print(mod.selection)



Residuals_of_Quadratic_Seasonal_Model <- resid(quad.seas.mod)

typeof(Residuals_of_Quadratic_Seasonal_Model)

plot(fitted(quad.seas.mod, "Observations of Quadratic Seasonal Model"), Residuals_of_Quadratic_Seasonal_Model)
#add a horizontal line at 0 
abline(0,0)

library(tseries)

bing <- as.character(resisual_exp_)

View(bing)
typeof(bing)





plot(acf(resisual_exp, plot = F), xlim = c(0,3), ylim = c(-1, 1), main = " Autocorrelation in a quadratic seasonal model under AIC")
plot(acf(resisual_exp, plot = F, type =  "p"), xlim = c(0,3),ylim = c(-1,1), main = "Partial Autocorrelation in a quadratic seasonal model under AIC")
