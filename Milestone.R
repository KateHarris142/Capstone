library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tidyr)

#sizes converted to MB
setwd("/Users/kathrynh/Documents/FurtherLearning/CourseProjects/capstone/final/en_US")
sizes <- file.info(dir())[1]
sizes[,1]<-sizes[,1]/(1000000)

#load files
text_t <- readLines("en_US.twitter.txt")
text_twitter <- data_frame(text = text_t)

text_b <- readLines("en_US.blogs.txt")
text_blog <- data_frame(text = text_b)

text_n <-readLines("en_US.news.txt")
text_news <- data_frame(text = text_n)


#summary of each table
sizes['num_lines'] = c(length(text_b),length(text_n),length(text_t)) 
sizes['num_words'] = c(sum(str_count(text_b)),sum(str_count(text_n)),sum(str_count(text_t)))


#do sample of words
selectSample <- 0.01
sample_t <- text_twitter[sample(nrow(text_twitter), dim(text_twitter)[1]*selectSample), ]
sample_n <- text_twitter[sample(nrow(text_news), dim(text_news)[1]*selectSample), ]
sample_b <- text_twitter[sample(nrow(text_blog), dim(text_blog)[1]*selectSample), ]

text_Fullsample  <- rbind(sample_b,sample_n,sample_t)

text_df <- text_Fullsample %>%
    unnest_tokens(word, text)

data(stop_words)

text_df2 <- text_df %>%
    anti_join(stop_words) 


text_df3 <- text_df2 %>%
    count(word, sort = TRUE) %>%
    filter(n>600) %>%
    mutate(word = reorder(word, n))

p <- ggplot(text_df3, aes(word, n)) + geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) 
p

text_df %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))


#bigram
text_bigrams <- text_Fullsample %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    count(bigram, sort = TRUE)


bigrams_separated <- text_Fullsample %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)

bigrams_separated

bigrams_united <- bigrams_separated %>%
    unite(bigram, word1, word2, sep = " ") %>%
    filter(n>30)

p2 <- ggplot(bigrams_united, aes(bigram, n)) + geom_bar(stat="identity",fill="blue") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) 
p2


bigrams_separated2 <- text_Fullsample %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word |
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)

bigrams_separated2




#TriGram
trigrams_separated <- text_Fullsample %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = TRUE)

trigrams_separated

trigrams_united <- trigrams_separated %>%
    unite(trigram, word1, word2, word3, sep = " ") %>%
    filter(n>4)

p3 <- ggplot(trigrams_united, aes(trigram, n)) + geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) 
p3



fourgrams_separated <- text_Fullsample %>%
    unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
    count(fourgram, sort = TRUE)

fourgrams_separated

trigrams_united <- trigrams_separated %>%
    unite(trigram, word1, word2, word3, sep = " ") %>%
    filter(n>4)

p3 <- ggplot(trigrams_united, aes(trigram, n)) + geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) 
p3



