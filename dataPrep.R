library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(data.table)



#sizes converted to MB
setwd("/Users/kathrynh/Documents/FurtherLearning/CourseProjects/capstone/final/en_US")
#load files
text_t <- readLines("en_US.twitter.txt")
text_twitter <- data_frame(text = text_t)

text_b <- readLines("en_US.blogs.txt")
text_blog <- data_frame(text = text_b)

text_n <-readLines("en_US.news.txt")
text_news <- data_frame(text = text_n)


#do sample of words
selectSample <- 0.20
sample_t <- text_twitter[sample(nrow(text_twitter), dim(text_twitter)[1]*selectSample), ]
sample_n <- text_twitter[sample(nrow(text_news), dim(text_news)[1]*selectSample), ]
sample_b <- text_twitter[sample(nrow(text_blog), dim(text_blog)[1]*selectSample), ]
# 
text_Fullsample  <- rbind(sample_b,sample_n,sample_t)
write.csv(text_Fullsample,"sampleTextlarge.csv")
write.csv(text_Fullsample,"sampleTextMedium.csv")

text_Fullsample <- read.csv("sampleTextlarge.csv")
text_Fullsample <- read.csv("sampleTextMedium.csv")


sample_t <- text_twitter[sample(nrow(text_twitter), dim(text_twitter)[1]*0.2), ]
sample_t <- text_news[sample(nrow(text_news), dim(text_news)[1]*0.5), ]


write.csv(sample_t,"sampleText_twitter.csv")
text_Fullsample <- read.csv("sampleText_twitter.csv")


#calculate the 4word ngram
data(stop_words)
fourgrams <- text_news %>%
    unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
    separate(fourgram, c("word1", "word2","word3","word4"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word,
           !word4 %in% stop_words$word)

#we don't want any matches in to trigrams etc so select unique combinations of the last 3 words and later remove from the trigram
#to_compare <- data.frame(paste(fourgrams$word2, fourgrams$word3,fourgrams$word4,sep=" "))
#to_compare3 <- unique(to_compare2)
#to_compare2 <- unname(unlist(to_compare[,1]))

fourgrams_separated <- fourgrams %>%
    count(word1,word2,word3,word4,sort = TRUE)

fourgrams_separated

newTab <- data.frame(paste(fourgrams_separated$word1, fourgrams_separated$word2,fourgrams_separated$word3,sep=" "),
                     fourgrams_separated$word4, fourgrams_separated$n)

colnames(newTab) <- c("start","predict","n")

write.csv(newTab,"fourgram.csv")

##### Trigram prep

data(stop_words)
trigrams <- text_news %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2","word3"), sep = " ")  %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word)
    
    
#tri_list <- unname(unlist(trigrams[,1]))

#remove matches
#trigrams_new <- trigrams[!(tri_list %in% to_compare3),]

trigrams_separated <- trigrams %>%
    count(word1,word2,word3,sort = TRUE)

trigrams_separated
newTabtri <- data.frame(paste(trigrams_separated$word1, trigrams_separated$word2,sep=" "),
                        trigrams_separated$word3, trigrams_separated$n)

colnames(newTabtri) <- c("start","predict","n")

write.csv(newTabtri,"trigram.csv")


#5 gram

fivegrams <- sample_t %>%
    unnest_tokens(fivegram, text, token = "ngrams", n = 5) %>%
    separate(fivegram, c("word1", "word2","word3","word4","word5"), sep = " ") 

#we don't want any matches in to trigrams etc so select unique combinations of the last 3 words and later remove from the trigram

fivegrams_separated <- fivegrams %>%
    count(word1,word2,word3,word4,word5,sort = TRUE)
newTab5 <- data.frame(paste(fivegrams_separated$word1, fivegrams_separated$word2,
                            fivegrams_separated$word3, fivegrams_separated$word4, sep=" "),
                     fivegrams_separated$word5, fivegrams_separated$n)

colnames(newTab5) <- c("start","predict","n")

trigram <- newTabtri


