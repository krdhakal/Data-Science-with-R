
#####################################
#   Linear Regression
#####################################
require(stats)
str(cars)

#The data give the speed of cars and the distances taken to stop. Note that the data were recorded in the 1920s.

plot(cars$speed, cars$dist)
#abline(rec.lm, lwd=2, col='red')
cor(cars$speed, cars$dist)

cr <- cor(cars)
#We can use corrplot to visualize
install.packages("corrplot")
library(corrplot)

corrplot(cr, type='upper')

rec.lm <- lm(cars$dist ~ cars$speed, data=cars)

summary(rec.lm)

#Distance = -17.5791 + 3.9324*speed
#If speed (mph) is zero then the stopping distance is 17.5791 fts, more generally intercept doesn't have intutive interpretation
#for every unit more of speed, we estimate distance grows on average by 3.93 fts

#please recall that we are working on sample and not population - hence the point estimate of the
#coefficient is not necessarily (and almost never is) the true value, hence we always needs
#confidence intervals for our estimates and usually in statistics we want 95% CI

#Hence the value of coefficient can be 3.9324 +/- 2*0.4155
# Now the null hypothesis - there is no relationship between speed and distance and slope is zero
# Alternate hypothesis - there exits a relationship between speed and distance

#One rule of thumb - if 0 is ouside our 95% CI, we claim that relationship exists, check p value
# now the value is 1.49e-12 which is near to zero hence the relationship is statistically
# significant at 95% CI

#Model fit - Check R-squared and adjusted R-squared values
# Standard errors and F-statistics - Both standard errors and F-statistic are measures of goodness of fit. The higher the F-Statistic the better it is.

# Let's check the predictive power of the model

# Create Training and Test data

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.7*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data

lmMod <- lm(dist ~ speed, data=trainingData) 
summary(lmMod)

# build the model
distPred <- predict(lmMod, testData)  # predict distance

# Prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

install.packages('Metrics')
library(Metrics)
mse(testData$dist,distPred)

#To compare Predicted value with actual value
plot(testData$dist, type="l", lty=1.8, col="green")
lines(distPred, type = "l", col="blue")

#Very quickly multivariate dataset regression
data ("BostonHousing", package="mlbench")
?BostonHousing

str(BostonHousing)
Boston_cor <- BostonHousing[,-4]
cr <- cor(Boston_cor)
corrplot(cr, type = 'upper')
cor(Boston_cor)

#BostonHousing <- BostonHousing[,-11]
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(BostonHousing), 0.7*nrow(BostonHousing))  # row indices for training data
trainingData <- BostonHousing[trainingRowIndex, ]  # model training data
testData  <- BostonHousing[-trainingRowIndex, ]   # test data

model <- lm(trainingData$medv~., data=trainingData)
summary(model)

library(car)
vif(model)#variance inflation factor

trainingData_1 <- trainingData[,-10]
testData_1 <- testData[,-10]
model_2 <- lm(trainingData_1$medv~., data=trainingData_1)
summary(model_2)
vif(model_2)

hist(BostonHousing$indus)
hist(BostonHousing$nox)
hist(BostonHousing$age)

hist(log(BostonHousing$indus))
trainingData_1$indus <- log(trainingData_1$indus)
testData_1$indus <- log(testData_1$indus)

hist(log(BostonHousing$age))
hist(sqrt(BostonHousing$age))

hist(log(BostonHousing$nox))
trainingData_1$nox <- log(trainingData_1$nox)
testData_1$nox <- log(testData_1$nox)

model_3 <- lm(trainingData_1$medv~., data=trainingData_1)
summary(model_3)
vif(model_3)

trainingData_1 <- trainingData_1[,-5]
testData_1 <- testData_1[,-5]

model_4 <- lm(trainingData_1$medv~., data=trainingData_1)
summary(model_4)
vif(model_4)

trainingData_2 <- trainingData_1
testData_2 <- testData_1
trainingData_2 <- trainingData_2[,-6]
testData_2 <- testData_2[,-6]

model_5 <- lm(trainingData_2$medv~., data=trainingData_2)
summary(model_5)

predic <- predict(model_4, testData_1)
predic

predic_1 <- predict(model_5, testData_2)
predic_1

mse(testData_1$medv,predic) #Model 4 (with age)
mse(testData_2$medv,predic_1) #Model 5 (without age)
