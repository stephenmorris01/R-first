library(faraway)
data(seatpos)
attach(seatpos)
lm(hipcenter~., data=seatpos)
##This regresses hipcenter against all the other variabes in the dataframe seatpos

### 1

results <- lm(hipcenter~., data=seatpos)
summary(results)
# ANOVA F test indicates very small p-value: 1.306e-05, so there is a linear relationship
# No significant individual t-values for slope, so this implies multicollinearity
# R-squared:  0.6866 so pretty significant (SSR/SST)

### 2

# No individual t-value shows significant LR between predictor and response,
# but F-test p-value is far below .05, so the predictors as a group do have a 
# LR with response.

### 3
# pairs() is like plot() for MLR
# lower.panel = NULL removes the redundant plots
pairs(hipcenter~., data=seatpos, lower.panel = NULL)
# (HtShoes, Ht, Weight, Seated) all have a strong relationship with each other.
# (Thigh, Leg) also an apparent relationship.  (Age) not so much.

### 4
vif(results)
# HtShoes and Ht have very high VIFs, so they must be very multicollinear.
# Seated is also pretty high, but less than 10

### 5
round(cor(seatpos[,3:8]),3) ##notice in your dataframe that the
##6 variables we want pairwise correlations are in the 3rd to 8th
##columns of the dataset. seatpos[,3:8] says we want to use the
##values from all rows and columns 3 to 8 of the dataframe.
# HtShoes, Ht, Seated, Leg are all very high (.9 or higher), but all of these 
# predictors seem to exhibit a strong correlation (.6 or higher)

### 6
# Ht had the highest p-value, and seems to have the highest correlation,
# so it may be the strongest predictor

### 7
lm(hipcenter~Age+Weight+Ht)
reduced <- lm(hipcenter~Age+Weight+Ht)
summary(reduced)

vif(reduced)
pairs(hipcenter~ Age+Weight+Ht, lower.panel = NULL)
# VIFs much smaller, no greater than 3. But the plot still shows correlation between
# Weight and Ht.

### 8

anova(reduced,results)
# still not a significant p-value comparing the reduced list to the total predictors
anova(results)
# sigificant p-value for Weight and HtShoes in presence of all other predictors

### 9
par(mfrow=c(2,2))
plot(reduced$fitted.values,reduced$residuals, main='Residual Values')
abline(h=0,col="green")
acf(reduced$residuals, main="ACF of Residuals")
qqnorm(reduced$residuals)
qqline(reduced$residuals, col="magenta")
# still seem to be some issues on the residual plot WRT error zero mean, errors uncorrelated

### 10
# y = B0 + (0.519504)x1 + (0.004271)x2 - (4.21190)x4
# R-squared:  0.6562 - lower than the first model 0.6866
# Adjusted R-squared:  0.6258 - higher than first model 0.6001 

### 11
hip.error<-hipcenter+10*rnorm(38) ##adds an error term
##that is N(0,10^2) to the 38 observations

results2 <- lm(hip.error~., data=seatpos)
summary(results2)
