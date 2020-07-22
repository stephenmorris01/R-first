data <- read.table("nfl.txt", header=TRUE ,sep="")
attach(data)

## Fit MLR model, using + in between predictors
result<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
##Question 1##
# LR to wins: x1, x8 (neg), x5
# Predictors correlated with each other: x1-x7, x1-x8 (neg), x8-x9
# x1, x8 (neg), x5 seem to have the strongest LR

summary(result)

# pairs() is like plot() for MLR
# lower.panel = NULL removes the redundant plots
pairs(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, lower.panel = NULL)

cor(x1+x2+x3+x4+x5+x6+x7+x8+x9, y) # 0.2124595

##Question 2##
# Equation: y = -1.808 + .004x2 + .194x7 -.005x8
# Fit MLR model with 3 specified predictors
result2<-lm(y~x2+x7+x8)
summary(result2)

##Question 3##
# B1x7 = 0.193960 - for every 1% increase of rush vs. pass, the team is predicted to win .19 more games,
# with all other variables held constant.
# So a ~5% increase in rushing leads to ~1 more win

##Question 4##
# y = B0 + B1x2 + B1x7 + B1x8
# y = -1.80837 + 0.0035982*2000 + 0.19396*48 + -0.004816*2350
-1.80837 + 0.0035982*2000 + 0.19396*48 + -0.004816*2350 #3.38051
newdata <- data.frame(x2=2000, x7=48, x8=2350)
predict.lm(result2, newdata, level=0.95, interval="prediction")
#        fit        lwr      upr
# 1 3.381448 -0.5163727 7.279268

##Question 5##
pairs(y~x2+x7+x8,lower.panel = NULL)
# The plots do not appear to indicate a strong linear relationship.
# H0: B1 (x2, x7, x8) = 0. The model is not useful in predicting wins.
# Ha: At least one of (x2, x7, x8) != 0. The model is useful in predicting wins.
# F-stat is 29.44 on 3 and 24 DF
# P-value is 3.273e-08
# What is the critical value associated with this hypothesis test? Perform the test at 0.05 significance level.
qf(0.05, 3, 24) # Critical Value is 0.1157608. F-stat is bigger, reject null. The model is useful, but unclear which predictor(s) are significant.
##confidence interval
confint(result2,level = 0.95)

##Question 6##
# H0: B1x7 = 0 ... can be dropped from the model with no impact on the linear relationship,
# in the presence of other predictors
# Ha: B1x7 != 0 ... cannot be dropped in the presence of other predictors
# t-stat for x7 = 2.198
# critical value (Pr(>|t|) is 0.037815, so 3.78% - less than 5% so we reject the null hypothesis
qt(0.975, 24) # 2.063899 # since tstat > critical value, we reject the null hypothesis

##Question 7##
par(mfrow=c(2,2))
plot(result2$fitted.values,result2$residuals, main='Residual Values')
abline(h=0,col="green")
acf(result2$residuals, main="ACF of Residuals")
qqnorm(result2$residuals)
qqline(result2$residuals, col="magenta")
# Residuals are pretty evenly distributed, ACF and QQ look good

##Question 8##
result3<-lm(y~x1+x2+x7+x8)
summary(result3)
#t-test is 0.549
# We cannot conclude this soley based on the one predictor - only in the context of
# being combined with the other 3 predictors (family variance, interaction)

### Correlating predictors against each other
cor(cbind(x1, x2, x7, x8))

