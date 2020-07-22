data <- read.table("defects.txt", header=TRUE ,sep="")
attach(data)

result<-lm(defects~weeks)

par(mfrow=c(2,2))

##scatterplot of data, with least squares line overlayed
plot(defects~weeks, main="Scatterplot of Defects Against Weeks", xlab= '# weeks since overhaul', ylab='Avg # defects per 10,000 bottles')
abline(result,col="green")

##residual plot
plot(result$fitted.values,result$residuals, main="Residual Plot: Defects Against Weeks")
abline(h=0,col="red")

acf(result$residuals, main="ACF of Residuals")

##Normal probability or QQ plot of residuals
qqnorm(result$residuals)
qqline(result$residuals, col="magenta")

## install package if not already
## install.packages('MASS')
# the library once installed
library(MASS)

## Box Cox method to see if transformation is required

summary(result)
par(mfrow=c(1,1))
boxcox(result)


##Transform

log.defects <- log(defects)
result.log <- lm(log.defects ~ weeks)

par(mfrow=c(2,2))
plot(log.weeks, defects, main="Scatterplot after Transforming Predictor")
abline(result.log, col="red")
plot(result.log$fitted.values, result.log$residuals, main="Plot of residuals against fits")
abline(h=0, col="red")
acf(result.log$residuals, main="ACF of Residuals")
qqnorm(result.log$residuals)
qqline(result.log$residuals, col="red")

summary(result.log)




par(mfrow=c(1,1))
boxcox(result.inv)
boxcox(result.inv, lambda = seq(0.6, 1.6, 0.01))