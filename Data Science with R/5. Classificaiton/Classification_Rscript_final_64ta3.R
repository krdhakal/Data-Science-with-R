#####################################
#   Logistics Regression
#####################################

# Y can take values between 0 and 1 whereas betax can take values between -infi to +infi
# Hence logit function becomes - log(p/1-p) = beta0 + beta1*x + error - log odds
#if x changes by one unit how odds changes
#It does not uses OLS (Ordinary Least Square) for parameter estimation. Instead, it uses maximum likelihood estimation (MLE).

diabetic<-read.csv(file.choose())

summary(diabetic)
str(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)
table(diabetic$Is_Diabetic)
#cdplot(diabetic$Is_Diabetic~diabetic$BMI, data=diabetic)

#Build a training and testing set
set.seed(4)
id <- sample(1:nrow(diabetic), 0.7*nrow(diabetic))  # row indices for training data
trainset <- diabetic[id, ]  # model training data
testset  <- diabetic[-id, ]   # test data

#Build logistic regression model
model<-glm(trainset$Is_Diabetic~. ,data = trainset, family='binomial')
summary(model)

diabetic_1 <- diabetic
#Let's check the distributions
hist(diabetic$skin_fold_thickness)
hist(sqrt(diabetic$skin_fold_thickness))
diabetic_1$skin_fold_thickness <- sqrt(diabetic_1$skin_fold_thickness)
summary(diabetic_1$skin_fold_thickness)

hist(diabetic$X2.Hour_serum_insulin)
hist(sqrt(diabetic$X2.Hour_serum_insulin))
diabetic_1$X2.Hour_serum_insulin <- sqrt(diabetic_1$X2.Hour_serum_insulin)
summary(diabetic_1$X2.Hour_serum_insulin)

hist(diabetic$Age)
hist(log(diabetic$Age))
hist(sqrt(diabetic$Age))
diabetic_1$Age <- log(diabetic_1$Age)
summary(diabetic_1$Age)

#Build a training and testing set
set.seed(4)
id_1 <- sample(1:nrow(diabetic_1), 0.7*nrow(diabetic_1))  # row indices for training data
trainset_1 <- diabetic_1[id_1, ]  # model training data
testset_1  <- diabetic_1[-id_1, ]   # test data

model_1<-glm(trainset_1$Is_Diabetic~. ,data = trainset_1,family = "binomial")
summary(model_1)

diabetic_1 <- diabetic_1[,-c(4,5)]
#Build a training and testing set
set.seed(4)
id_2 <- sample(1:nrow(diabetic_1), 0.7*nrow(diabetic_1))  # row indices for training data
trainset_2 <- diabetic_1[id_2, ]  # model training data
testset_2  <- diabetic_1[-id_2, ]   # test data

model_2<-glm(trainset_2$Is_Diabetic~. ,data = trainset_2,family = "binomial")
summary(model_2)

(exp(0.086323)-1)*100 #BMI
(exp(0.034072)-1)*100#Glucose
(exp(-0.014512)-1)*100#Blood pressure
exp(1.104915)

#Null deviance - how well response variable is predicted by model that includes only intercept
#Residual deviance - how well response variable is predicted by model with inclusion of independent variables
#The analogous metric of adjusted R² in logistic regression is AIC. AIC is the measure of fit which penalizes model for the number of model coefficients. Therefore, we always prefer model with minimum AIC value.

predvalues<-predict(model_2,newdata = testset_2,type = "response")
predvalues

table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.5)

Accuracy <- (134+42)/(134+42+37+18)
# Accuracy is 76.19%

table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.3)

Accuracy <- (107+64)/(107+64+45+15)
# Accuracy is 74.02%

table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.7)

Accuracy <- (146+28)/(146+28+6+51)
# Accuracy is 75.3%

install.packages("ROCR")
library(ROCR)
ROCpred <-prediction(predvalues, testset_2$Is_Diabetic)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)

table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.3)
table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.4)
table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.6)
table(Actualvalues = testset_2$Is_Diabetic, Predictedvalues = predvalues>0.5)
Accuracy <- (120+53)/(120+53+26+32)

#####################################
#   Decision Tree
#####################################

diabetic<-read.csv("Diabetes.csv")

summary(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)

install.packages("rpart")
library(rpart)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

#Build decision tree model
model<-rpart(trainset$Is_Diabetic~. ,data = trainset)
summary(model)
plot(model, margin=0.1)
text(model, use.n = TRUE,pretty = TRUE, cex=0.8)

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)
confusionMatrix(table(predvalues, testset$Is_Diabetic))
class(predvalues)
class(testset$Is_Diabetic)
table(predvalues)
table(testset$Is_Diabetic)

a <- cbind.data.frame(predvalues, testset$Is_Diabetic)
write.csv(a, "a.csv")

model_1 <-rpart(trainset$Is_Diabetic~. ,data = trainset, control = rpart.control(cp=0))
summary(model_1)
plot(model_1)
printcp(model_1)

model_2 <-rpart(trainset$Is_Diabetic~. ,data = trainset, control = rpart.control(cp=0.0107527))
summary(model_2)

predvalues_1<-predict(model_1,newdata = testset,type = "class")
confusionMatrix(table(predvalues_1, testset$Is_Diabetic))

predvalues_2<-predict(model_2,newdata = testset,type = "class")
confusionMatrix(table(predvalues_2, testset$Is_Diabetic))


#####################################
#   Random Forest
#####################################

diabetic<-read.csv(file.choose())

summary(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)

install.packages("randomForest")
library(randomForest)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

#Build random forest model
model<-randomForest(trainset$Is_Diabetic~. ,data = trainset, ntree=500)
model
plot(model)
legend("right", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)

model<-randomForest(trainset$Is_Diabetic~. ,data = trainset, ntree=200)
model
plot(model)
legend("right", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)
confusionMatrix(table(predvalues, testset$Is_Diabetic))

varImp(model)
varImpPlot(model,sort=TRUE, type=2)
importance(model)
varUsed(model)

#Build naive bayes model
library(e1071)

diabetic<-read.csv("Diabetes.csv")

summary(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

model<-naiveBayes(trainset$Is_Diabetic~. ,data = trainset)
model

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

confusionMatrix(table(predvalues, testset$Is_Diabetic))

#Build support vector machines

library(e1071)
data(iris)

summary(iris)
str(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(iris),prob = c(0.7,0.3),replace = T)
trainset<-iris[id==1,]
testset<-iris[id==2,]
class(trainset)

#kernel can be linear/polynomial/radial or sigmoid
#cost called as c value - determine the width of the margin, larger the c value, smaller the width
#scale for normalization to avoid bias

model<-svm(Species~. ,data = trainset, kernel = "linear", cost = 0.1)
summary(model)
plot(model, trainset,Petal.Width ~ Petal.Length)
plot(model, trainset,Sepal.Width ~ Sepal.Length)

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

library(caret)
confusionMatrix(table(predvalues, testset$Species))

#Tune for best cost function

set.seed(1)
obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost= c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"), kernel='linear')
summary(obj)
best.model <- obj$best.model
summary(best.model)

predvalues_best <-predict(best.model,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues_best, testset$Species))

#Let's change the kernel scenarios

model_poly<-svm(Species~. ,data = trainset, kernel = "polynomial", cost = 0.1, gamma = 2)
summary(model_poly)

plot(model_poly, trainset,Petal.Width ~ Petal.Length)

predvalues<-predict(model_poly,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues, testset$Species))

set.seed(1)
obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost= c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"), kernel='polynomial')
summary(obj)
best.model <- obj$best.model
summary(best.model)

predvalues_best <-predict(best.model,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues_best, testset$Species))

