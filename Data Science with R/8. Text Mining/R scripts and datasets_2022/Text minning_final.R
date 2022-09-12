install.packages("tm")
install.packages("dplyr")
install.packages("SnowballC")

library("tm")
library("dplyr")
library("SnowballC")

dell_script <- readLines("C:/Personal/Module 8/R scripts and datasets_2022/Dell earning call Q32022.txt")
lenovo_script <- readLines("C:/Personal/Module 8/R scripts and datasets_2022/Lenovo earning call Q32022.txt")

#To break into words we would require Corpus
Corpusdell <- Corpus(VectorSource(dell_script))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords,stopwords("english"))%>%
  tm_map(stripWhitespace)%>%
  tm_map(removeWords, c("will", "new", "also")) # Text stemming
  #tm_map(stemDocument)
  
#Document term matrix
dtmdell <- DocumentTermMatrix(Corpusdell)
dtmdell
inspect(dtmdell[1:10,1:10])

#calculating word frequencies
word.freqdell <- sort(colSums(as.matrix(dtmdell)),decreasing = T)
head(word.freqdell)

#to get relative frequencies
tabledell <- data.frame(word=names(word.freqdell),absolute.frequency=word.freqdell,relative.frequency = word.freqdell/length(word.freqdell))
head(tabledell)
rownames(tabledell)<- NULL
head(tabledell)
write.csv(tabledell[1:1000,],"dell_1000.csv")  

#Let's start with next dataset

#To break into words we would require Corpus
Corpuslenovo <- Corpus(VectorSource(lenovo_script))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords,stopwords("english"))%>%
  tm_map(stripWhitespace)%>%
  tm_map(removeWords, c("will", "new", "also")) # Text stemming
  #tm_map(stemDocument)

#Document term matrix
dtmlenovo <- DocumentTermMatrix(Corpuslenovo)
dtmlenovo
inspect(dtmlenovo[1:10,1:10])

#calculating word frequencies
word.freqlenovo <- sort(colSums(as.matrix(dtmlenovo)),decreasing = T)
head(word.freqlenovo)

#to get relative frequencies
tablelenovo <- data.frame(word=names(word.freqlenovo),absolute.frequency=word.freqlenovo,relative.frequency = word.freqlenovo/length(word.freqlenovo))
head(tablelenovo)
rownames(tablelenovo)<- NULL
head(tablelenovo)
write.csv(tablelenovo[1:1000,],"lenovo_1000.csv") 

#Merging both the datasets and for two earning calls comparison

finaltable <- tabledell %>%
  merge(tablelenovo,by="word")%>%
  mutate(dProp=relative.frequency.x-relative.frequency.y,dAbs=abs(dProp))%>%
  arrange(desc(dAbs))
  #rename(HP6.freq=absolute.frequency.x, HP6.prop = relative.frequency.x,
         #HP7.freq=absolute.frequency.y, HP7.prop = relative.frequency.y)

head(finaltable)

#More visualizations

dtm <- TermDocumentMatrix(Corpusdell)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate word cloud

install.packages("wordcloud")
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=400, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#Bigram cloud generations

install.packages("RWeka")
library(RWeka)

# Bigrams 
minfreq_bigram<-2
token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(Corpusdell, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,min.freq = minfreq_bigram,scale = c(1.4,0.7),colors = brewer.pal(8,"Dark2"),max.words=150)

# Trigrams 

minfreq_trigram <- 2
token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(Corpusdell, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.4,0.5),colors = brewer.pal(8,"Dark2"),max.words=150)

#Visualization with dell earning call now

dtm <- TermDocumentMatrix(Corpuslenovo)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate word cloud

install.packages("wordcloud")
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=400, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

barplot(d[1:50,]$freq, las = 2, names.arg = d[1:50,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#Bigram cloud generations

install.packages("RWeka")
library(RWeka)

# Bigrams 
minfreq_bigram<-2
token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(Corpuslenovo, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,min.freq = minfreq_bigram,scale = c(1.4,0.7),colors = brewer.pal(8,"Dark2"),max.words=150)

# Trigrams 

minfreq_trigram <- 2
token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(Corpuslenovo, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.4,0.5),colors = brewer.pal(8,"Dark2"),max.words=150)

#Sentiment analysis

install.packages("twitteR")
library(twitteR)

consumer_key <- 
consumer_secret <- 
access_token <- 
access_secret <- 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#load 1000 tweets of bjp and samajwadi

bjp_tweets <- searchTwitter("bjp", n=2000, lang="en")
samajwadi_tweets <- searchTwitter("samajwadi", n=2000, lang="en")

#Extract text from the tweets

bjp.text <- lapply(bjp_tweets,function(x) x$getText())
samajwadi.text <- lapply(samajwadi_tweets,function(x) x$getText())
class(bjp.text)
samajwadi.text[[1]]
bjp.text[[1]]

pos = scan('positive-words.txt', what='character',comment.char = ";")
neg = scan('negative-words.txt', what='character',comment.char = ";")

#let's create the function for analyzing text

sentence <- bjp.text

install.packages("stringr")
library(stringr)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}

#Using above function let's calculate the score of bjp tweets
analysisbjp = score.sentiment(bjp.text, pos,neg,.progress='none')
analysisbjp <- as.data.frame(analysisbjp)
table(analysisbjp$score)
mean(analysisbjp$score)
median(analysisbjp$score)
hist(analysisbjp$score)

analysissamajwadi <- score.sentiment(samajwadi.text, pos,neg,.progress='none')
table(analysissamajwadi$score)
mean(analysissamajwadi$score)
median(analysissamajwadi$score)
hist(analysissamajwadi$score)

#comparing sentiment score
par(mfrow=c(2,2))

plot1 <- hist(analysisbjp$score)
plot2 <- hist(analysissamajwadi$score)
plot(plot1, col=rgb(0,1,0,0.25),main="modi vs samajwadi", xlab = "scores") #Brown
plot(plot2, col=rgb(1,0,0,0.25),add = T) #Brown

#kejriwal has more positive tweets as compared to modi


  

