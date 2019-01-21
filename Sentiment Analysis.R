rm(list=ls())
library(twitteR)
library(purrr)
library(plyr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(stringr)
library(ggplot2)
library(syuzhet)
library(tm)
library(wordcloud)
library(wordcloud2)

setwd("C:/Users/Lenovo/Documents/R Projects/Sentiment Analysis")

#Function for calculating Score Sentiments
score.sentiment <- function(sentences, pos.words, neg.words, .progress = "none")
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('//d+', "", sentence)
    sentence <- gsub("http[^[:space:]]*", "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, " ")
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress = .progress)
  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}

#Lexicon of positive and negative words
neg.words = scan('C:/Users/Lenovo/Documents/R Projects/Sentiment Analysis/negativewords.txt', what = 'character', comment.char = ';')
pos.words = scan('C:/Users/Lenovo/Documents/R Projects/Sentiment Analysis/positivewords.txt', what = 'character', comment.char = ';')

#Import tweets from Twitter API to R
consumerKey <- "6WETbpJQbjLg4CRB3zzAMWWDO"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "o12NhGDotbDCI3pCAXQyPwy1zi4rJJ2G99T0LMxpq0v0oQvbbK"
accessToken <- "1085499509105455107-86VC0rlm3ZySJL4w0wSJrF58c4oXMH"
accessTokenSecret <- "5il77ykUXeKG0RhVlJX2Q6vhq8MHIwC7fjqFKOiHF1oAO"
twitCred <- OAuthFactory$new(consumerKey = consumerKey, 
                             consumerSecret = consumerSecret,
                             requestURL = reqURL,
                             accessURL = accessURL,
                             authURL = authURL)
twitCred$handshake()
  setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
  tweet <- searchTwitter("@gillette", n = 10000, lang = "en", since = "2019-01-13")
  tweet_df <- tbl_df(map_df(tweet, as.data.frame))

gscore <- score.sentiment(tweet_df$text, pos.words, neg.words, .progress = 'text')

#Vizualize results for Sentiment Analysis I
scores <- table(gscore$score)
barplot(scores, main = "Barplot for Gillette's Sentiment Scores", 
     xlab="Scores", 
     border="darkblue", 
     col="turquoise")
text(x = 11, y = 4000, adj = 0, label = "Score < 0: Negative\nScore = 0: Neutral\nScore > 0: Positive",
       col = 1, cex = 1.3)
#####################################################################

#Sentiment Analysis II
corpus <- iconv(tweet_df$text)
corpus <- Corpus(VectorSource(corpus))
tun <- function(x) gsub("@[a-z,A-Z]*","",x)
corpus <- tm_map(corpus, tun)
tt <- function(x) gsub("RT |via", "", x)
corpus <- tm_map(corpus, tt)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
ss <- function(x) gsub("[^0-9A-Za-z///' ]", '', x,ignore.case = TRUE)
corpus <- tm_map(corpus, ss)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c('isnt', 'gillette', 'terrence', 'thewizrd'))
#inspect(corpus[1:5])

####################################################

tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)

#Vizualize results
w <- rowSums(tdm)
w <- subset(w, w >= 400)
barplot(w, las = 2, col = rainbow(50))

#1st Wordcloud
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud (words = names(w), freq = w,
           min.freq = 60, random.order = F, colors = brewer.pal(3, 'Dark2'))

#2nd wordcloud
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w, size = 0.5, shape = 'circle')

#Barplots
corpus <- iconv(tweet_df$text)
s <- get_nrc_sentiment(corpus)
barplot(colSums(s[, 1:8]), col = rainbow(10), ylab = 'Count', 
        main = 'Sentiment Scores')

barplot(
  sort(colSums(prop.table(s[, 1:8]))),
  cex.names = 1, 
  las = 1, col = rainbow(10),
  main = "Emotion Percentages in Gillette Tweets", xlab = "Emotions", ylab = "Percent"
)