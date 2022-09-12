
#Load the Data into R
setwd('/Volumes/GoogleDrive/My Drive/ML_AI/Edureka/DS with R/Class 10')
install.packages('readr')
library(readr)
hrdata = read.csv("338_cert_proj_datasets_v3.0.csv")
head(hrdata)
summary(hrdata)

#Correlation of Data
colnames(hrdata)
cordata = cor(hrdata[,1:8])
library(corrplot)
corrplot(cordata,type = "full")

#Visualizing the characteristics of people who left
leftdata = hrdata[hrdata$left==1,]
#leftdata1 = hrdata[hrdata$left==0,]


par(mfrow = c(2,3))
hist(leftdata$satisfaction_level,col = "#3090C7",main = "", xlab = "Satisfaction level")
hist(leftdata$Work_accident,col = "#3090C7",main = "", xlab = "Work Accidents")
hist(leftdata$average_montly_hours,col = "#3090C7",main = "", xlab = "AVG monthly hours")
hist(leftdata$last_evaluation,col = "#3090C7",main = "", xlab = "Last Evaluation scores")
hist(leftdata$number_project,col = "#3090C7",main = "", xlab = "No.of Projects")
plot(as.factor(leftdata$promotion_last_5years),col = "#3090C7",main = "", xlab = "Promotion last 5 years")

#Plotting a visualiztion for people with average working hrs vs number of projects, with coloring according to left or not
par(mfrow = c(1,1))
plot(hrdata$number_project,hrdata$average_montly_hours,col=as.factor(hrdata$left))


#Lets calculate daily working hour for the employees who left the job
leftdata$average_daily_working_hours<-(leftdata$average_montly_hours/30)
hist(leftdata$average_daily_working_hours)

#Visualizing how many people who have left have met a work accident,Since it's a binary variable convert to factor 
leftdata$Work_accident<-as.factor(leftdata$Work_accident)
plot(leftdata$Work_accident)

#Visualizing how many people who have left have got a promotion in last five years,Since it's a binary variable convert to factor 
leftdata$promotion_last_5years<-as.factor(leftdata$promotion_last_5years)
plot(leftdata$promotion_last_5years)


#Plotting satisfaction level of people who left the job vs who didn't
set.seed(42)
p1 <- hist(hrdata$satisfaction_level[hrdata$left=="1"])
p2 <- hist(hrdata$satisfaction_level[hrdata$left=="0"])
plot( p2, col=rgb(0,0,1,1/4), xlim=c(0,1))  # first histogram
plot( p1, col=rgb(1,0,0,1/4), xlim=c(0,1), add=T)  # second

##Let's see department wise turn out
dat = aggregate(left~department,data = leftdata,FUN = sum)
View(dat)
plot(table(leftdata$department))

#The above graph doesnot take into account the number of employees there in each department
plot(table(leftdata$department)/table(hrdata$department),las =2)

#People who should'nt have left
priorityData = leftdata[leftdata$satisfaction_level>0.7|leftdata$last_evaluation>0.8|leftdata$number_project>5,]
nrow(priorityData)

#Lets check the correlation of people who have already left
corleft = cor(priorityData[,c(1,2,3,4,5,6,8)])
corrplot(corleft,type = "full")

#Now we will predict what are the factors that causes them to leave
hrdata$left = ifelse(hrdata$left==1,"Left","Not Left")
hrdata$left = as.factor(hrdata$left)
library(caTools)
id = sample.split(hrdata,SplitRatio = 0.7)
#id = sample(2,nrow(hrdata), prob = c(0.8,0.2),replace = T)
traindata = hrdata[id,]
testdata = hrdata[!id,]

#Decision Tree________________________________________________
library(rpart)
treeModel = rpart(left~.,data = traindata)
treeModel


plot(treeModel,margin  = 0.2)
text(treeModel,cex = 0.7)

predtree = predict(treeModel,testdata,type = "class")
predtree
library(caret)
confusionMatrix(table(predtree,testdata$left))

#RandomForest_________________________________________________
library(randomForest)

forestModel = randomForest(left~.,data = traindata)
forestModel

predforest = predict(forestModel,testdata,type = "class")
confusionMatrix(table(predforest,testdata$left))

#Naive-Bayes__________________________________________________
library(e1071)

bayesModel = naiveBayes(left~.,data = traindata)
bayesModel

predBayes = predict(bayesModel,testdata,type = "class")
confusionMatrix(table(predBayes,testdata$left))

#SVM__________________________________________

svmModel = svm(left~.,data = traindata)
svmModel

predsvm = predict(svmModel,testdata)
confusionMatrix(table(predsvm,testdata$left))


