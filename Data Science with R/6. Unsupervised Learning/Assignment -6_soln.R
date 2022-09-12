
##Clustering another example
getwd()
setwd('/Volumes/GoogleDrive/My Drive/ML_AI/Edureka/DS with R/Class 6')
getwd()
install.packages("readxl")
library(readxl)
Insurance_data <- read_excel("InsuranceData.xlsx")
summary(Insurance_data)
#CustomerData <- read.csv(file.choose())
Insurance_data <- data.frame(Insurance_data)
summary(Insurance_data)
sum(is.na(Insurance_data))
data <-Insurance_data[,-c(1,3,4)]
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(data,k, nstart = 1)$tot.withinss})
plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')
set.seed(123)
install.packages("cluster")
library(cluster)
install.packages(fpc)
library(fpc)
clus <- kmeans(data, centers=3)
# Fig 01
plotcluster(data, clus$cluster)
# More complex
clusplot(data, clus$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
data_1 <- cbind(data, clus)
##C-means clustering
library(e1071)
c2 <- cmeans(mycustomer, 4)
c2

###W

