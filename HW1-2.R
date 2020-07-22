# Homework 1-2
# Stephen Morris sam3ce@virginia.edu
# STAT 6021

# install and load the faraway package
install.packages('faraway')
library(faraway)

# load the cornit data set

data(cornnit)
attach(cornnit)
names(cornnit)

# The data explore the relationship between corn yield (bushels per acre) and
# nitrogen (pounds per acre) fertilizer application in a study carried out in Wisconsin.

# a.
# Response (y) <- yield
# Predictor (x) <- nitrogen
plot(yield~nitrogen, xlab='Nitrogen (pounds per acre)', ylab='Corn yield (bushels per acre)', main='Corn Yield by Nitrogen Application')
# refer to 2a_scatter.jpg
# abline follows (b) because 'corn' is not yet defined
# There appears to be a positive linear relationship.

# b.
lm(yield~nitrogen)
corn <- lm(yield~nitrogen)
summary(corn)
# refer to 2b_lm.jpg
abline(corn, col='green') # had to wait until 'corn' was defined

par(mfrow=c(2,2)) # viewing multiple plots from here on out

plot(corn$fitted.values,corn$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="blue")
# Based on the residual plot, I would plan to use an inverse square function due to the upside-down 'U' shape

# c.
## For Box-Cox, install MASS package if not already
install.packages('MASS')
# the library once installed
library(MASS)

boxcox(corn, lambda = seq(-1, 6, 1))
# refer to 2c_boxcox.jpg
# lambda is centered near 3.

# d.
## Transforming x based on visual analysis of the plot y = B0 + B1x^2
# refer to 2d_transformation_x_lm.jpg
# refer to 2d_transformation_x_plot.jpg
nitrogen2 <- (nitrogen)^2 # transforming on visual analysis, squaring the predictor 
corn2 <- lm(yield~nitrogen2)
summary(corn2)
plot(yield~nitrogen2, xlab='(Transformed) Nitrogen (pounds per acre)', ylab='Corn yield (bushels per acre)', main='Corn Yield by Nitrogen Application')
abline(corn2, col='green') 
plot(corn2$fitted.values,corn2$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="blue")

boxcox(corn2, lambda = seq(-1, 6, 1))

## Based on Box-Cox method, lambda is ~3 - y^3=B0 + B1x
# refer to 2d_transformation_x_lm.jpg
# refer to 2d_transformation_x_plot.jpg
yield3 <- (yield)^3 # transforming for lambda 3, cubing the response 
corn3 <- lm(yield3~nitrogen2)
summary(corn3)
plot(yield3~nitrogen2, xlab='(Transformed) Nitrogen (pounds per acre)', ylab='(Transformed) Corn yield (bushels per acre)', main='Corn Yield by Nitrogen Application')
abline(corn3, col='green') 
plot(corn3$fitted.values,corn3$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="blue")

boxcox(corn3, lambda = seq(-1, 6, 1))

# Final equation is:
# y* = B0 + B1x* where y* = y^3 and x* = x^2

##Note:  This did not improve the LM - retained for future use
## Based on our class experience in the Module 3 tutorial, inverse helped with curved plot 1/y=B0 + B1
#inv_yield <- 1/yield
#corn_inv<-lm(inv_yield ~ nitrogen)
#summary(corn_inv)
#plot(inv_yield~nitrogen, xlab='Nitrogen (pounds per acre)', ylab='Corn yield (bushels per acre)', main='Corn Yield by Nitrogen Application')
#abline(corn_inv, col='green') 
#plot(corn_inv$fitted.values, corn_inv$residuals, main="Plot of Residuals against Fitted Values")
#abline(h=0,col="blue")

##Note:  This did not improve the LM - retained for future use
## Transforming x based on additional trials y=B0 + x^(1/4)
#nitrogen1_4 <- (nitrogen)^(1/4) # transforming for lambda 3, cubing the predictor 
#corn1_4 <- lm(yield~nitrogen1_4)
#summary(corn1_4)
#(yield~nitrogen1_4, xlab='Nitrogen (pounds per acre)', ylab='Corn yield (bushels per acre)', main='Corn Yield by Nitrogen Application')
#abline(corn1_4, col='green') 
#plot(corn1_4$fitted.values,corn1_4$residuals, main="Plot of Residuals against Fitted Values")
#abline(h=0,col="magenta")


