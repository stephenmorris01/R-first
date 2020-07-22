# Question 1
data<-read.table("bp.txt", header=TRUE ,sep="")

plot(data$weight,data$BP,xlab='weight',ylab='blood pressure',main='Blood pressure by weight')

# Question 2
n <- lm(data$BP~data$weight)
summary(n)

# Question 3
# BP = Intercept + Slope*weight
69.1043 + (0.41942 * 200) # Predicted BP
# Residual = actual value - predicted
148 - (69.1043 + 0.41942 * 200) # Residual - 148 is "actual BP"

# Question 4
anova(n)

# Question 5
# Multiple R-squared:  0.5983
# This means 59.83% of the variance in BP (y) is due to weight (x).

# Question 6
# Residual mean square is sigma-hat^2
# sigma-hat is standard error of regression
sqrt(75.36) #8.681014

# Question 7
# H0: BP is directly corelated to weight
# Ha: BP is not related to weight

# Question 8
# F-stat of 35.744 is found by dividing MSr (regression) by MSres (residual)
2693.58 / 75.36

# Question 9
# Ha is supported because there is not enough data to support
# a direct relationship between BP and Weight. The variance is
# too great, far beyond the accepted 5% where C = 1-0.05 = 95%
