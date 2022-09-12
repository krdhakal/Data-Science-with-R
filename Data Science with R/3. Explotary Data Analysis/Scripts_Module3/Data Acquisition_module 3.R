
#####################################
#   Data Acquisition
#####################################

# Reading dataset from directory

getwd()
setwd("C:/Personal/Module 3/")

# Reading dataset using read.csv command

movie_df <- read.csv("movie_metadata.csv")
movie_df <- read.csv(file.choose())

#####################################
#   Download Raw data from Website
#####################################

LinkAddress = "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=50620&Year=2018&Month=10&Day=1&timeframe=2&submit=Download+Data%22"
#dest should be file name and not a directory
download.file(LinkAddress, dest="C:/Personal/Module 3/webdata/test3.csv")
list.files("C://Personal/Module 3/webdata")

#####################################
#   Webscrapping - Reading from website
#####################################

#let's try the package rvest and IMDB website

install.packages('rvest')
library(rvest)
#Knowledge of selector gadget to understand HTML and CSS Tags

#Specifying the url for desired website to be scraped
url <- 'https://www.imdb.com/search/title/?title_type=feature&release_date=2016-01-01,2016-12-31&count=100&ref_=adv_prv'

#Reading the HTML code from the website
webpage <- read_html(url)
#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)

try <- cbind(rank_data,title_data,description_data)
try_1 <- as.data.frame(try)
write.csv(try_1, "C:/Personal/Module 3/webdata/try.csv")

#####################################
#   Reading Excel File
#####################################
install.packages("xlsx")
library(xlsx)

InsuranceData <- read.xlsx("M3_Insurance_Data.xlsx", sheetIndex=1)
#view(InsuranceData)

col <- c(2,4)
row <- c(2:5)
InsuranceData_subset <- read.xlsx("M3_Insurance_Data.xlsx", sheetIndex = 1, colIndex = col, rowIndex = row, header=TRUE)
InsuranceData_subset

#####################################
#   Reading from MSSQL
#####################################
install.packages("RMySQL")
library("RMySQL")
#Connect to a database

hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")

allTables <- dbListTables(hg19)
dbGetQuery(hg19, 'select count(*) from affyU133Plus2')

#####################################
#   Reading XML File
#####################################
install.packages("XML")
library(XML)
library("methods")

# Give the input file name to the function.
result <- xmlParse(file="trial.xml")

a <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/ID")),"ID")    
b <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/NAME")),"Name") 
c <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/SALARY")),"Salary") 
d <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/STARTDATE")),"StartDate")
e <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/DEPT")),"Department") 

employee_df <- cbind(a,b,c,d,e)
employee_df[,1]
employee_df[c(1:5),c(2:5)]
employee_df[c(2,4), c(1,5)]

#####################################
#   Reading from API
#####################################
install.packages("twitteR")
library(twitteR)

consumer_key <-
consumer_secret <- 
access_token <- 
access_secret <- 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Use searchTwitter to search Twitter based on the supplied search string and return a list. The “lang” parameter is used below to restrict tweets to the “English” language. 
tweets <- searchTwitter(searchString = "omicron",500, lang="en")
tweets[[1]]

install.packages("rjson")
# Load the package required to read JSON files.
library("rjson")
mydata <- fromJSON(file="http://makemeanalyst.com/wp-content/uploads/2017/06/sample_json.txt")
mydata

data<- data.frame(mydata)
data
