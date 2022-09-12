
install.packages("arules")
library(arules)

datagr <- read.transactions("C:/Personal_Merck/Module 7/groceries.csv", sep = ",")
class(datagr)
summary(datagr)

#Inspect function is used to view the association rules
#inspect(datagr)
inspect(datagr[1:3])

#Item frequency helps in getting support characteristics
itemFrequency(datagr[,1])
itemFrequency(datagr[,1:169])

itemFrequencyPlot(datagr, support=0.08)
itemFrequencyPlot(datagr, topN =10)

#generating association rules

m1 <- apriori(datagr)
summary(m1)

m2 <- apriori(datagr, parameter = list(support=0.007,confidence=0.5))
summary(m2)
inspect(m2[1:10])
inspect(sort(m2,by="lift")[1:10])

m3 <- apriori(datagr, parameter = list(support=0.007,confidence=0.2),
              appearance = list(default="rhs",lhs=c("whole milk","soda")))
summary(m3)
inspect(m3[1:9])

install.packages("arulesViz")
library(arulesViz)
plot(m3)

plot(m3, measure=c("confidence", "support"), shading="lift", interactive=TRUE)

#Building Recommendation Engine

library("recommenderlab")
library(ggplot2)
library(data.table)
library(reshape2)
library(magrittr) 
library(usethis)
library("devtools")

data(MovieLense, package = "recommenderlab")

movielense = MovieLense
nrow(movielense)
ncol(movielense)
image(MovieLense[1:100,1:100])

set.seed(101)
which_train = sample(x = c(TRUE, FALSE), size = nrow(movielense), replace = TRUE, 
                     prob = c(0.8, 0.2))

recc_data_train = movielense[which_train, ]
recc_data_test = movielense[!which_train, ]

recc_model1 = Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 25, 
                                                                                    method = "Cosine"))
recc_model1

num_rec =10 

recc_predicted1 <- predict(object = recc_model1, newdata = recc_data_test, n = num_rec)
recc_predicted1

recdf = data.frame(user = sort(rep(1:length(recc_predicted1@items), recc_predicted1@n)), 
                   rating = unlist(recc_predicted1@ratings), index = unlist(recc_predicted1@items))


recdf$title = recc_predicted1@itemLabels[recdf$index]
#recdf$year = moviemeta$year[recdf$index]
recdf

#UBCF
data("MovieLense")
MovieLense

ml10 = MovieLense[c(1:10),]
ml10 = ml10[,c(1:10)]
as(ml10, "matrix")
image(MovieLense[1:100,1:100])

train  = MovieLense
our_model = Recommender(train, method = "UBCF")
our_model
User = 115
pre = predict(our_model, MovieLense[User], n = 10)
pre
user_ratings = train[User]
as(user_ratings, "list")
as(pre,"list")

recc_model2 =Recommender(data = recc_data_train, method = "UBCF", parameter = list(k = 10, 
                                                                                   method = "Cosine"))
num_rec = 10 

recc_predicted2 = predict(object = recc_model2, newdata = recc_data_test, n = num_rec)
recc_predicted2

recdfub =data.frame(user = sort(rep(1:length(recc_predicted2@items), recc_predicted2@n)), 
                    rating = unlist(recc_predicted2@ratings), index = unlist(recc_predicted2@items))

recdfub$title =recc_predicted2@itemLabels[recdfub$index]
#recdfub$year = moviemeta$year[recdfub$index]

recdfub








