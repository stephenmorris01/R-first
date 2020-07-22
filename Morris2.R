# read the data file
data<-read.table("bp.txt", header=TRUE ,sep="")
attach(data)

#1 plot bp against weight
plot(BP~weight, xlab='Weight in Pounds', ylab='Systolic BP in mmHg', main='Blood Pressure According to Weight')
n <- lm(BP~weight)
summary(n)

# estimated linear regression equation
# BP = intercept + slope * weight 
# BP = 69.10437 + 0.41942 * weight

# correlate
cor(weight,BP) # 0.7734903

# confidence interval
confint(n,level = 0.95) # weight 0.2746281  0.5642023

# hypothesis test
# H0: B1 = 0
# Ha: B1 != 0
2*(1-pt(5.979,24)) # p-value is  3.587959e-06
# t-value and DOF are given by the lm function already performed
# We reject the null hypothesis because p-value is greater than 1-a/2 or 0.975
# An increase in blood pressure is correlated to increase in weight (slope != 0)

# estimate mean BP, weight 200
# BP = 69.10437 + 0.41942 * weight
69.10437 + 0.41942 * 200  # 152.9884
newdata<-data.frame(weight=200)
predict.lm(n,newdata,level=0.95, interval="confidence")
predict.lm(n,newdata,level=0.95, interval="prediction")

#Question 8
# H0: B1=0.35
# Ha: B1>0.35
# t = (B1 - 0.35) / se(B1)
(0.41942 - 0.35) / 0.07015 # t = 0.9895937 
1 -pt(0.9896, 24) # p-value is 0.1661212
qt(.95,24) # 1.710882 t value was 0.98, so we fail to reject H_0
