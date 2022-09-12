#to clear the console use control+L for Mac os
getwd()
setwd("/Volumes/GoogleDrive/My Drive/ML_AI/Edureka/DS with R/Class 3/Datasets_Module3")
list(getwd())
movie_df <- read.csv("m.csv")

#Let's check the summary and structure of the dataset
str(movie_df)
summary(movie_df)
#write summary to CSV
write.csv(summary(movie_df),"Movie_df summary.csv")
class(movie_df)
nrow(movie_df)
ncol(movie_df()
     #Top 6 rows along with header name
     head(movie_df)
     tail(movie_df)
     
     movie_df[1:4,]
     
     movie <- movie_df
     class(movie$aspect_ratio)
     
     #Let's play with subset and data points in dataframe
     #Fetch 4th column from 1st Row ; df[RowIndex, ColIndex]
     #dataframe[row index,column index]
     movie[1,4]
     movie[1, "duration"]
     movie[1, c("movie_title", "duration")]
     movie[,c('duration','movie_title','gross')]
     movie[1, 1:5] #first 5 columns
     movie[1:5, 1:5] #first 5 rows and 5 cols
     movie_list <- movie[,c(2,12)]
     
     movie_list_2 <- movie[movie$color=='Color'& movie$duration >100,]
     movie_list
     
     movie_color <- subset(movie, movie$color=="Color")
     color_movies <- subset(movie$movie_title, movie$color == "Color" & movie$duration >200 | movie$imdb_score >8)
     color_movies <- subset(movie, movie$color == "Color" & movie$duration >200 | movie$imdb_score >8)
     
     color_movies_sub <- color_movies[,c('movie_title','duration', 'imdb_score')]
     color_movies <- subset(c(movie$movie_title,movie$director_name), movie$color == "Color" & movie$duration >200 | movie$imdb_score >8)
     
     class(movie$color)
     summary(movie$color)
     movie$color <- as.factor(movie$color)
     class(movie$color)
     summary(movie$color)
     
     #Replacement of particular entry in the dataset
     levels(movie$color)
     levels(movie$color)[levels(movie$color)==""] <- "Not mentioned"
     levels(movie$color)
     movie$duration[which(movie$duration==178)] <- 178.0
     table(movie$duration)
     
     movie$movie_title[which(movie$movie_title =='The Avengers')]
     class(movie$duration)
     unique(movie$color)
     unique(movie$duration)
     table(movie$movie_title)
     
     #Replacement of NA values with some value
     mean(movie$duration)
     summary(movie$duration)
     mean(movie$duration, na.rm = TRUE)
     sum(is.na(movie$duration),0)
     summary(movie$duration)
     summary(movie)
     sum(is.na(movie),0)
     
     #Imputation with mean, median and mode
     install.packages("Hmisc")
     library("Hmisc")
     hist(movie$duration)
     movie_backup <- movie
     movie_backup$duration <- impute(movie_backup$duration, mean)# replace with mean
     summary(movie_backup$duration)
     hist(movie_backup$duration)
     
     table(is.na(movie_backup$duration))
     sum(is.na(movie_backup$duration),0)
     hist(movie_backup$duration)
     summary(movie$duration)
     
     #'NA' values or records are removed
     movie_new <- na.omit(movie)
     table(movie$language)
     
     #let's use the table command here to check the entries by categories
     table(movie$color)
     table(movie$director_name)
     table(movie$language)
     max(table(movie$language))
     sort(table(movie$language), decreasing = TRUE)
     
     #Text Manipulation -- To remove special char "Â"
     movie$movie_title <- gsub(pattern = " ", replacement = "", movie$movie_title)
     View(movie)
     
     #Generation of new column and cbind feature
     movie$newgross <- movie$gross/10000
     a <- as.data.frame(cbind(movie$color, movie$imdb_score))
     movie$newgross <- NULL
     
     #All movies which duration > 200 mins
     movie[movie$duration > 200, "movie_title"]
     
     #Top 10 movies based on gross
     gross_trial <- as.data.frame(head(movie[order(movie$gross, decreasing = TRUE), c("movie_title", "gross")], 10))
     
     #Top 10 movies with highest profits
     movie$profit <- movie$gross - movie$budget
     head(movie[order(movie$profit, decreasing = TRUE), c("movie_title", "gross", "profit")], 10)
     
     #Know top rated movies in the list and also average IMDB Score
     head(movie[order(movie$imdb_score, decreasing = TRUE), c("movie_title", "imdb_score")], 10)
     mean(movie$imdb_score)
     #deletion of columns
     #movie$profit <- NULL
     
     #Spliting of dataset and random sampling operations
     trainingRowIndex <- sample(1:nrow(movie), 0.7*nrow(movie))  # row indices for training data
     trainingData <- movie[trainingRowIndex, ]  # model training data
     testData  <- movie[-trainingRowIndex, ]   # test data
     seed(123)
     
     #Some visualization on the datasets and the concept of normalization
     
     hist(movie$num_critic_for_reviews)
     summary(movie$num_critic_for_reviews)
     
     hist(log(movie$num_critic_for_reviews))
     summary(log(movie$num_critic_for_reviews))
     
     hist(sqrt(movie$num_critic_for_reviews))
     summary(sqrt(movie$num_critic_for_reviews))
     
     #Let us create multiple histograms on one plot
     install.packages('gridExtra')
     library(gridExtra)
     
     library(ggplot2)
     a<-qplot(x=movie$num_critic_for_reviews,data=movie)
     b<-qplot(x=log(movie$num_critic_for_reviews),data=movie)
     c<-qplot(x=sqrt(movie$num_critic_for_reviews),data=movie)
     
     grid.arrange(a,b,c)
     
     #time related concepts and strptime functionality
     time <- read.csv('Time_dataset.csv')
     str_time <- strptime(time$time,format = '%H%M')
     head(str_time)
     date <- '2000-04-20'
     str_time_re <- paste(date,time$time)
     head(str_time_re)
     
     str_time_again <- strptime(str_time_re,format='%Y-%m-%d %H%M')
     head(str_time_again)
     time$Year <- str_time_again
     
     #Interesting visuals on exploration 
     
     #1 Histogram - Plots the frequency
     hist(movie_df$title_year)
     summary(movie_df$title_year)
     table(movie_df$title_year)
     
     #2 Box Plot
     boxplot(movie$imdb_score)
     summary(movie$imdb_score)
     hist(movie$imdb_score)
     hist(sqrt(movie$imdb_score))
     
     outlie <- boxplot(movie$imdb_score, plot=FALSE)$out
     outlie
     movie <- movie[!(movie$imdb_score %in% outlie),]
     boxplot(movie$imdb_score)
     
     #3 Scatter Plot
     plot(movie$title_year, movie$director_facebook_likes)
     plot(movie$title_year, movie$director_facebook_likes,type="p", main='movie_data')
     plot(movie_df$title_year, movie_df$director_facebook_likes)
     table(movie$director_facebook_likes)
     
     #Some more visualization on the datasets
     
     installed.packages("ggplot2")
     library("ggplot2")
     
     # Use data from data.frame
     qplot(movie$imdb_score,data = movie)
     hist(movie$imdb_score)
     qplot(movie$gross,data = movie)
     plot(density(movie$imdb_score))
     
     qplot(movie$imdb_score,movie$gross,data = movie)
     qplot(movie$budget,movie$gross,data = movie)
     qplot(movie$imdb_score,movie$gross,data = movie, colour = I("blue"))
     qplot(movie$imdb_score,movie$gross,data = movie, colour = movie$color)
     qplot(movie$imdb_score,movie$gross,data = movie, size = movie$color)
     hist(movie$title_year)
     
     movie$title_year[which(movie$title_year>=2010)] <- 4
     movie$title_year[which(movie$title_year>=2000)] <- 3
     movie$title_year[which(movie$title_year>=1980)] <- 2
     movie$title_year[which(movie$title_year>=1950)] <- 1
     movie$title_year[which(movie$title_year>=1910)] <- 0
     table(movie$title_year)
     movie$title_year <- as.factor(movie$title_year)
     table(movie$title_year)
     
     qplot(movie$imdb_score,movie$gross,data = movie, facets = movie$color ~ movie$title_year)
     qplot(movie$imdb_score,movie$gross,data = movie, facets = movie$title_year ~ movie$color)
     
     table(movie_df$title_year)
     
     
     
     
     