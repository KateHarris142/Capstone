library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(data.table)

fourgram <- read.csv("fourgram.csv")
trigram <- read.csv("trigram.csv")

phrase <- "faith during"

fourgram <- newTab

t <- fourgram[grep(phrase,fourgram$start),]

#3word ngram
trigram <- newTabtri
phrase2 <- paste(strsplit(phrase," ")[[1]][2], strsplit(phrase," ")[[1]][3],sep=" ")

t_tri <- trigram[grep(phrase,trigram$start),]


phrase5 <- "still struggling but the"

newTab5[grep(phrase5,newTab5$start),]

#######################
#start using the fourgram:
#create a table of the frequency of the frequencies
FoF <- data.frame(table(t$n))
t2 <- merge(t,FoF,by.x="n",by.y="Var1")

# the probability of there being that frequency
Nr <-t2$n/(sum(t2$n))  #currN
t2 <- cbind(t2,Nr)

#Using the Simple Good-Turing estimate, anything with n>5 is fine. Anything else is estimated.
# using linear regression of the log transform to get prediction for n+1
fit <- lm(log(t2$Nr)~log(t2$n))

a = fit$coefficients[[1]]
b = fit$coefficients[[2]]

# Loop around to get the adjusted frequency. 
Nr1 = vector()   #nextN
discount = vector()
for(i in 1:dim(t2)[1]){
    
    if(t2$n[i] > 5){
        Nr1[i] <- t2$Nr[i]
        discount[i] <- 1
    }else{
        currRTimes <- t2$n[i]
        nextRTimes <- t2$n[i] +1 
        Nr1[i] <- 10^(a + b*log(currRTimes))
        discount[i] <- (nextRTimes/currRTimes) * (Nr1[i]/t2$Nr[i])
    }
}

t3 <- cbind(t2,Nr1,discount)

prob <- t3$n*t3$discount/sum(t3$n)

t3 <- cbind(t3,prob)

colnames(t3) <- c("n","start","predict","FoF","ProbOfFreq","ProbOfFreq+1","discount","probaility")

t3 <- t3[order(-t3$probaility),]

#left-over probability mass for the (n − 1)-gram
leftover = 1-sum(t3$n*t3$discount/sum(t3$n))

######
#Then go to 3gram
FoF2 <- data.frame(table(t_tri$n))
t_tri2 <- merge(t_tri,FoF2,by.x="n",by.y="Var1")

# the probability of there being that frequency
Nr_tri <-t_tri2$n/(sum(t_tri2$n))
t_tri2 <- cbind(t_tri2,Nr_tri)

#Using the Simple Good-Turing estimate, anything with n>5 is fine. Anything else is estimated.
# using linear regression of the log transform to get prediction for n+1
fit_tri <- lm(log(t_tri2$Nr)~log(t_tri2$n))

a_tri = fit_tri$coefficients[[1]]
b_tri = fit_tri$coefficients[[2]]

# Loop around to get the adjusted frequency. 
Nr1_tri = vector()
discount_tri = vector()
for(i in 1:dim(t_tri2)[1]){
    
    if(t_tri2$n[i] > 5){
        Nr1_tri[i] <- t_tri2$Nr[i]
        discount_tri[i] <- 1
    }else{
        currRTimes <- t2$n[i]
        nextRTimes <- t2$n[i] +1 
        Nr1_tri[i] <- 10^(a + b*log(currRTimes))
        discount_tri[i] <- (nextRTimes/currRTimes) * (Nr1_tri[i]/t2$Nr[i])
    }
}

t_tri3 <- cbind(t_tri2,Nr1_tri,discount_tri)

prob2 <- t_tri3$n*t_tri3$discount_tri * leftover/(sum(t_tri3$n*t_tri3$discount_tri))

t_tri3 <- cbind(t_tri3,prob2)

colnames(t_tri3) <- c("n","start","predict","FoF","ProbOfFreq","ProbOfFreq+1","discount","probaility")

t_tri3 <- t_tri3[order(-t_tri3$probaility),]

