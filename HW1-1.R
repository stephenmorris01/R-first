# Homework 1-1
# Stephen Morris sam3ce@virginia.edu
# STAT 6021

faithful
attach(faithful)

# a.
# Response (y) <- waiting time
# Predictor (x) <- eruption duration

# b.
plot(waiting~eruptions, xlab='Duration of Eruption', ylab='Wait Time Until Next Eruption', main='Old Faithful Eruption Wait Times')
# refer to 1b_scatter.jpg
# shows a positive linear relationship

# c.
# The longer the duration of an eruption, the longer one would expect to wait
# until the next eruption. The plot appears to show a fairly reliable relationship
# between the predictor (eruptions) and the response (waiting).

# d.
lm(waiting~eruptions)
result <- lm(waiting~eruptions)
summary(result)
# refer to 1b_lm.jpg
#B-hat_1 (estimated slope): 10.7296
#B-hat_0 (estimated intercept): 33.4744
# R^2 (coefficient of determination): 0.8115
# Estimated SD of errors: 0.3148

# e.
# The value of B-hat_0 (estimated intercept) makes sense because there are no
# instances in the data set with a lower than 33.4744 minute wait time.

# f.
anova(result)
# refer to 1f_anova.jpg
# ANOVA F-stat is 1162.1
# H_0: B-hat_1 = 0. There is no linear relationship between eruption and wait times.
# H_a: B-hat_1 != 0. (2-sided) There is a linear relationship between eruption and wait times.
# t squared = F
sqrt(1162.1) # t = 34.08959
qt(.95,2270) # 1.645525 < 34.08959
# We reject the null hypothesis. There is a positive linear relationship between
# the predictor (eruptions) and the response (waiting).

# g.
confint(result,level = 0.95) # 10.10996 to 11.34932
# This is consistent with the conclusion because the calculated slope 10.7296
# is within this confidence interval.

# h.
# estimate wait time, eruption 3.5
# waiting = 33.4744 + (10.7296 * 3.5) = 71.028
newdata<-data.frame(eruptions=3.5)
predict.lm(result,newdata,level=0.95, interval="prediction")
# PI = 59.36332 to 82.69296 predicted waiting time until next eruption

# i.
predict.lm(result,newdata,level=0.95, interval="confidence")
# CI = 70.32211 to 71.73417 confidence interval for the mean (average) waiting time

# j.
par(mfrow=c(2,2))
plot(result$fitted.values,result$residuals, main='Plot of Residuals Against Fitted Values')
abline(h=0,col="blue")
acf(result$residuals, main="Auto Correlation Functions of Residuals")
qqnorm(result$residuals)
qqline(result$residuals, col="green")

# refer to 1j_reg_ACF_QQ.jpg

# Based on these plots, the regression assumptions are not met.
# Residuals have a wide spread, and cluster around 55 and 80
# ACF outside of the blue lines for lag 1
# QQ plot strays from the line between -2 and -3.
# These plots do not really support any of the assumptions for regression analysis.
# This is somewhat surprising given the name and history of "Old Faithful"