library(dplyr)
library(tidytext)
library(tm)
library(stringr)
library(SnowballC)
library(RWeka)

#sizes converted to MB
setwd("/Users/kathrynh/Documents/FurtherLearning/CourseProjects/capstone/final/en_US")
sizes <- file.info(dir())[1]
sizes[,1]<-sizes[,1]/(1000000)

#load files
text_t <- readLines("en_US.twitter.txt")
text_twitter <- data_frame(line = 1:length(text_t), text = text_t)

text_b <- readLines("en_US.blogs.txt")
text_blog <- data_frame(line = 1:length(text_b),text = text_b)

text_n <-readLines("en_US.news.txt")
text_news <- data_frame(line = 1:length(text_n), text = text_n)


#summary of each table
sizes['num_lines'] = c(length(text_b),length(text_n),length(text_t)) 
sizes['num_words'] = c(sum(str_count(text_b)),sum(str_count(text_n)),sum(str_count(text_t)))


#do sample of words
selectSample <- 0.01
sample_b <- sample(text_b, length(text_b)*selectSample)
sample_n <- sample(text_n, length(text_n)*selectSample)
sample_t <- sample(text_t, length(text_t)*selectSample)

text_Fullsample  <- c(sample_b,sample_n,sample_t)
docs <- VCorpus(VectorSource(text_Fullsample))

#tidy data
docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, PlainTextDocument)  

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

#
dtm_s <- removeSparseTerms(dtm, 0.999) 
tdm_s <- removeSparseTerms(tdm, 0.999) 
#most frequent words

freq <- colSums(as.matrix(dtm))  
freq2 <- freq[tail(order(freq),15)]
wf <- data.frame(word=names(freq2), freq=freq2)


p <- ggplot(wf, aes(x=reorder(word,freq), y=freq, fill=freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


#bigram
bigram <- NGramTokenizer(docs, Weka_control(min=2, max=2))

TrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tri <- TrigramTokenizer(docs)

#trigram

NGramTokenizer(docs, Weka_control(min=3, max=3))

NGramTokenizer(x, Weka_control(min=4, max=4))

