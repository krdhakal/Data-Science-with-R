
#Let's see now iris dataset
data(iris)
summary(iris)
str(iris)

hist(iris$Petal.Length)
hist(log(iris$Petal.Length))
hist(sqrt(iris$Petal.Length))

summary(iris$Petal.Length)
summary(log(iris$Petal.Length))
summary(sqrt(iris$Petal.Length))

summary(iris$Sepal.Length)
hist(iris$Sepal.Length)

var(iris$Sepal.Length)
sd(iris$Sepal.Length)
mean(iris$Sepal.Length)

hist(iris$Sepal.Width)
write.csv(iris, "example.csv")

iris$Sepal.Width
sort(iris$Sepal.Width)

sd(iris$Sepal.Length)
iris[,]
iris[1,1]
iris[,1]
iris[1,]
iris[c(2,4,6),]
iris[,c(2,4,5)]
iris[iris$Species=="setosa",]
iris[iris$Sepal.Length>4.0,]
iris[c(2:5),c(1:4)]

iris[iris$Sepal.Length>4.0 | iris$Species=="setosa",]
iris[iris$Sepal.Length>4.0 & iris$Species=="setosa",]

#iris[row index,column index]

iris[,]
iris[1,]
iris[,1]
iris[c(1,6,8), c(2,5)]
iris[1,"Sepal.Length"]
iris[iris$Species=="setosa",]
iris[c(2,5),c(1,3)]
iris[1:5,c(1,2)]
iris_1 <- iris[iris$Species=="setosa",]

median(iris$Sepal.Length)


# taking multiple inputs
# using braces 
{
  var1 = readline("Enter 1st number : ");
  var2 = readline("Enter 2nd number : ");
  var3 = readline("Enter 3rd number : ");
  var4 = readline("Enter 4th number : ");
}

# converting each value
var1 = as.integer(var1);
var2 = as.integer(var2);
var3 = as.integer(var3);
var4 = as.integer(var4);

# print the sum of the 4 number
print(var1 + var2 + var3 + var4)

iris[,]
iris[1,]
iris[,1]
iris[c(2,5,6),c(2,4)]
iris[iris$Species=='setosa',]






