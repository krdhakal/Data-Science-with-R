
#Example of word cloud with Dell earning call transcript

earningcall <- readLines("C:/Personal/Module 8/Dell_earning_ call.txt")

#To break into words we would require Corpus
earning_call_Dell <- Corpus(VectorSource(earningcall))%>%
  tm_map(removePunctuation)%>% # Remove punctuations
  tm_map(removeNumbers)%>% # Remove numbers
  tm_map(content_transformer(tolower))%>% # Convert the text to lower case
  tm_map(removeWords,stopwords("english"))%>% # Remove english common stopwords
  tm_map(stripWhitespace) # Eliminate extra white spaces
#tm_map(stemDocument)%>% # Text stemming
#tm_map(removeWords, c("harri", "said", "hermion", "ron", "dumbledoor", "'s")) # Text stemming
# specify your stopwords as a character vector
#earning_call_Del <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

#Document term matrix
dtmHP7 <- DocumentTermMatrix(earning_call_Dell)
dtmHP7
inspect(dtmHP7[1:10,1:10])

#calculating word frequencies
word.freqHP7 <- sort(colSums(as.matrix(dtmHP7)),decreasing = T)
head(word.freqHP7)

#to get relative frequencies
tableHP7 <- data.frame(word=names(word.freqHP7),absolute.frequency=word.freqHP7,relative.frequency = word.freqHP7/length(word.freqHP7))
head(tableHP7)
rownames(tableHP7)<- NULL
head(tableHP7)

#More visualizations

dtm <- TermDocumentMatrix(earning_call_Dell)
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

#Bigram cloud generations

install.packages("RWeka")
library(RWeka)

# Bigrams 
minfreq_bigram<-2
token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(earning_call_Dell, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,min.freq = minfreq_bigram,scale = c(1.4,0.7),colors = brewer.pal(8,"Dark2"),max.words=150)

# Trigrams 

minfreq_trigram <- 1
token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(earning_call_Dell, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.4,0.5),colors = brewer.pal(8,"Dark2"),max.words=150)

minfreq_trigram <- 1
token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(earning_call_Dell, Weka_control(min=4,max=4, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.4,0.5),colors = brewer.pal(8,"Dark2"),max.words=150)
