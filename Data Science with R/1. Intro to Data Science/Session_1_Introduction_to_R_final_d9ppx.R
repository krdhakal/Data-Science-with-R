#Introduction to R

#High Level Programming language
# Widely used in academia and research
# Very good collection of packages and tools for statistical analysis and 
# machine learning tools
# Good functionality for textual analysis tools
#Can be used both in interactive mode and the development mode

#R is an eco-system with basic programming constructs at the core. However, bulk of popularity for R comes not from 
# language itself but the vast number of tools and packages available for performing analyses. For example, in order 
# to use an exotic machine learning algorithm such as Random Forests, all that you need to do is call a function.

#R is a case sensitive language
#we are learning R

#get current work directory
getwd()

#set a different working directory
setwd('C:/Personal/Module 2/')
setwd("C:/Personal/Module 2/")
getwd()

#getting help
#The simplest way to get help in R is to click on the Help button on the toolbar 
#if you know the name of the function you want help with, you just type a question mark ? at the command line prompt followed by the
#name of the function.
help(solve)
?solve

#Sometimes you cannot remember the precise name of the function, but you know the subject on which
#you want help (e.g. data input in this case). Use the help.search function (without a question mark) with
#your query in double quotes like this
help.search("data input")

#Packages are one of the most important eco-system of R. You would be using 
# packages continuously throughout the course and in your professional lives
#Installing a package: You can install a package by using install.packages command
install.packages("e1071")

#In order to view contents of the package, type:

library(help=e1071)
library(e1071)

#Language essentials

#Simple calculations: You can simply use R as a calculator
log(50)
5+3
#Multiple expressions can be placed in single line but have to be 
# separated by semi-colons
log(20); 3*35; 5+2

#Assignment operations
#Variable assignment is a very easy task in R. There are three important things 
# to keep in mind
# Variable names are case sensitive
# Variable names cannot begin with numbers
# Variable names cannot contain blank spaces

x <- 5.6
x = 5
y = 5

x+y
x + y
x*y
z = x+y
z
x
print(x)
print(y)
x

#[1] indicates that x and y are vectors and 5 is the first element of the vector
x <- 1:50
x
sum(x)

#Difference between <- and = operator?

sum(z=1:50)
sum(ab <- 1:50)

#Sequences
0:20

20:6

seq(0,8,0.2)
seq(-8,0,0.2)
seq(0,20,2)
sum(a<-seq(0,20,3))


#Five basic data types: Character, Numeric, Complex, Logical, Boolean
#Vector is most basic object. Can only contain objects of same class

class(str)
class(y)

a <- 4.5
a
class(a)
typeof(a)
str(a)

b <- '4a'
class(b)
str(b)

str <- "abc"
str
class(str)
boolean <- TRUE
boolean
class(boolean)
a <- 3.2
class(a)

#Type conversion
x <- 2.5
class(x)
x <- "2.5"
class(x)
y <- as.numeric(x)
class(y)
y
z <- 'ab45'
z <- as.numeric(z)
a <- as.numeric(z)

# Many R operations can be performed, or performed more efficiently, on vectors or
# matrices. Vectors are strings of objects; matrices are two-dimensional collections of
# objects, usually numbers. The c operator, which means concatenate, creates simple
# vectors, while the colon (:) operator generates simple sequences. To construct matrices,
# one simply passes a vector of data, the dimensions of the matrix to be created, and
# whether to input the data by row or by column (the default behavior is to input data by row).                                                by row).

#c() is concatenate function

x <- c(1, 0.5, 4)
x
class(x)
y <- c("a","b","c")
y
y <- c(2.4,"c")
y
class(y)

z <- c('abc', 'prq')
z
class(z)

r <- c('abc', 4, 5,6, 'pqr')
r
class(r)

#To find the class of a vector, use class function
class(y)
#Length of a vector
length(y)
x <- c(5,6,7,8)
x

A <- 'kabir'
B <- 'Bhushan'
C <- c(A, B)
C

#This representation of data in a vector allows you to ask mathematical questions easily
mean(x)
max(x)
quantile(x)

x <- c(TRUE,1,0)
x
y <- c(TRUE,'abc')
y
class(x)
x[1]
class(x[1])

#Generating repeat
rep(4,9)
rep(1:7,10)
rep(1:7,each=3)

#Vector Arithematic. You can perform the arithematic operations on vectors in a 
# manner similar to variable operations. Here, the operations are performed on 
#each corresponding elements
x <- c(1, 0.5, 4)
x
y <- c(5,3,2)
y
x+y

#What happens in this case?
x <- c(1, 0.5, 4)
x
y <- c(5,3,2,4)
y
x+y

#What about this case?
x <- c(1, 0.5, 4, 3)
x
y <- c(5,3,2,6,7,8,9)
y
x+y

#Apply print with paste0 to get string and values together
x <- 5
print(paste0("Value of x is", x))
