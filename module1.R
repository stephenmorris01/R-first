## read data file

read.table("purity.txt", header=TRUE ,sep="")

## name the data file as "data" for easy use

data <- read.table("purity.txt", header=TRUE ,sep="")
data

## check variables
names(data)

## display contents of one variable
data$hydro

## attach the data file for easy use.
## Don't do this if working with multiple data sets!

attach(data)


## make a scatterplot of the data set
## (x,y) format
plot(purity,hydro)
## (y~x)alternate format
plot(hydro~purity)

## change labels - must be done for full credit!!
plot(hydro,purity, xlab="Percent of Hydrocarbons", ylab="Purity of Oxygen", main="Plot of Purity of Oxygen against Percent of Hydrocarbons")

## side by side plots (matrix form - rows,colums)
par(mfrow = c(1,2))
plot(hydro, main="Plot of Percent of Hydrocarbons for each Sample")
plot(purity, main="Plot of Purity of Oxygen for each Sample")

## save the plots - can be in jpeg, pdf, or other format
pdf("joint.pdf")
par(mfrow = c(1,2))
plot(hydro, main="Plot of Percent of Hydrocarbons for each Sample")
plot(purity, main="Plot of Purity of Oxygen for each Sample")
dev.off()

## regression
lm(purity~hydro)
result <- lm(purity~hydro)
result
summary(result)

## ANOVA table
anova(lm(purity~hydro))
