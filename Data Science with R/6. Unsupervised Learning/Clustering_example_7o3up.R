
##Clustering another example

CustomerData<- read.csv("Wholesale customers data.csv", header=TRUE)

CustomerData <- read.csv(file.choose())

Customerdata <- data.frame(CustomerData)

summary(Customerdata)

sum(is.na(Customerdata))

mycustomer <- Customerdata[,-c(1,2)]

k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(mycustomer,k, nstart = 1)$tot.withinss})
plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')

set.seed(123)
grocery <- kmeans(mycustomer, 4) #apply k means clustering
grocery
grocery$cluster
clust <- grocery$cluster
grocery_1 <- cbind(Customerdata, clust)

##C-means clustering
library(e1071)
c2 <- cmeans(mycustomer, 4)
c2

###Wine dataset clustering

library(HDclassif)
data(wine)

colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
summary(wine)
mywine <- wine[,-c(1)]
set.seed(123)
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(mywine,k, nstart = 1)$tot.withinss})
plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')

km_wine <- kmeans(mywine, 3) #apply k means clustering
km_wine

km_wine <- kmeans(mywine, 2) #apply k means clustering
km_wine

#Hierarachical clustering
# Facebook likes

mov <- read.csv("movie_metadata.csv", header=TRUE)
mov <- na.omit(mov)
colnames(mov)
facebook <- mov[,c(5,6,8,14,25,28)] #take facebook like column
submov <- facebook[1:50,]
rownames(submov) <- mov$movie_title[1:50]

d <- dist(as.matrix(submov))
hc <- hclust(d)
plot(hc,cex=0.8,las=1)
hc


