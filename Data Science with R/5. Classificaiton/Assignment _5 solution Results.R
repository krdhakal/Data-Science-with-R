# Assignment Soulution 6 for Extramaritial affaris
getwd()
install.packages("AER")
library(AER)
head(AER)
data("Affairs")
head(Affairs)
df1 <- Affairs
library(rpart)
library(rpart.plot)
library(caret)
library(RColorBrewer)
hist(df1$affairs, freq=FALSE, main="Density plot")
curve(dnorm(x, mean=mean(df1$affairs), sd=sd(df1$affairs)), add = TRUE, col='darkblue', lwd=2)
df1$past.affairs <- ifelse(df1$affairs == 0, "No", "Yes")
df1$past.affairs <-as.factor(df1$past.affairs)
df1$gender <- as.factor(df1$gender)
df1$children <- as.factor(df1$children)
inTrain <- createDataPartition(y = df1$past.affairs,
                               p = 0.7,
                               list = FALSE)

training <- df1[ inTrain,]
testing <- df1[-inTrain,]
set.seed(1)
tree <- rpart(past.affairs ~ gender + children + age + rating  + religion + education, data = training, method='class')
printcp(tree)
#Output as follow
## Classification tree:
## rpart(formula = past.affairs ~ sex.f + child.f + age.bins + occ.f + 
##     rating.f + ym + religion.f + edu.f, data = training, method = "class")
## 
## Variables actually used in tree construction:
## [1] age.bins   child.f    edu.f      occ.f      rating.f   religion.f
## 
## Root node error: 105/421 = 0.24941
## 
## n= 421 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.066667      0   1.00000 1.00000 0.084549
## 2 0.019048      2   0.86667 0.92381 0.082286
## 3 0.014286      4   0.82857 0.88571 0.081068
## 4 0.011905      6   0.80000 1.00952 0.084816
## 5 0.010000     10   0.75238 1.00952 0.08481
tree.pred <- predict(tree, testing, type = "class")