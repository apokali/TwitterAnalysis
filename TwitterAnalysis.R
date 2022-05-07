
# Reference links
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/

# Load required libraries
require(rtweet)
require(ggplot2)
require(dplyr)
require(tm)
require(wordcloud)

# Twitter authentication
get_token()

#_______________________________________________________________________________
#_______________________________________________________________________________
## search for 1000 tweets using by finding apple
tweet.df <- search_tweets(q = '#Heartstopper', n = 1000, lang = 'en', include_rts = FALSE)
head(tweet.df$text)
tweet.df <- data.frame(tweet.df['text'])

# Data cleaning, remove urls 
tweet.df$text <- gsub("http.*", "", tweet.df$text)
tweet.df$text <- gsub("https.*", "", tweet.df$text)

# Remove all non-ASCII characters
# reference: https://stackoverflow.com/questions/44893354/remove-emoticons-in-r-using-tm-package
tweet.df$text <- gsub("[^\x01-\x7F]", "", tweet.df$text)
tweet.df$text <- str_replace_all(tweet.df$text, "[\r\n]", "")

# Clean the punctuation
tweet.df$text =str_replace_all(tweet.df$text,"[\\.\\,\\;]+", " ")
tweet.df$text =str_replace_all(tweet.df$text,"http\\w+", "")
tweet.df$text =str_replace_all(tweet.df$text,"@\\w+", " ")
tweet.df$text =str_replace_all(tweet.df$text,"[[:punct:]]", " ")
tweet.df$text =str_replace_all(tweet.df$text,"[[:digit:]]", " ")
tweet.df$text =str_replace_all(tweet.df$text,"^ ", " ")
tweet.df$text =str_replace_all(tweet.df$text,"[<].*[>]", " ")
tweet.df$text <- str_trim(tweet.df$text)

tweet.df$text


#_______________________________________________________________________________
#_______________________________________________________________________________
# Dictionary based scoring
library(sentimentr, quietly = TRUE)
sentiment.score <- sentimentr::sentiment(tweet.df$text)
head(sentiment.score)


# Calculate the average value of sentiment.scores for each tweet
library(dplyr, quietly = TRUE)
sentiment.score <- sentiment.score %>% group_by(element_id) %>% 
  summarise(sentiment = mean(sentiment))
head(sentiment.score)


# Add the sentiment to our original tweet.df
tweet.df$polarity <- sentiment.score$sentiment
tweet.final <- tweet.df[,c('text', 'polarity')]

# remove all records with a polarity value of 0
tweet.final <- tweet.final[tweet.final$polarity != 0, ]
tweet.final$sentiment <- ifelse(tweet.final$polarity < 0, "Negative", "Positive")
tweet.final$sentiment <- as.factor(tweet.final$sentiment)
table(tweet.final$sentiment)


# Leverage the function upSample in the caret package to create some records 
# in the minority class.
# This should produce better results in the classification models
# install.packages("caret")
library(caret, quietly = TRUE)
tweet.balanced <- caret::upSample(x = tweet.final$text, y = tweet.final$sentiment)
names(tweet.balanced) <- c('text', 'sentiment')
table(tweet.balanced$sentiment)

# Note, the upSample function looks at the class distribution and repeatedly
# samples from the Positive class to balance. The final table command shows the
# new class distribution

# Finally, we add an doc_id column to our dataset
tweet.final$doc_id <- seq(1, nrow(tweet.final))

# Note, what does the sentiment function do?


#_______________________________________________________________________________
#_______________________________________________________________________________
# Text pre-processing
# Reference https://cache.one/read/15943846

library(tm)

# helper function of readTabular, since this is removed from the new tm package
readTabular<- function (mapping){
  stopifnot(is.list(mapping))
  function(elem, language, id) {
    meta <- lapply(mapping[setdiff(names(mapping), "content")], 
                   function(m) elem$content[, m])
    if (is.null(meta$id)) 
      meta$id <- as.character(id)
    if (is.null(meta$language)) 
      meta$language <- as.character(language)
    PlainTextDocument(elem$content[, mapping$content], meta = meta)
  }
}

# Create a document term matrix
get.dtm <- function(text.col, id.col, input.df, weighting) {
  title.reader <- readTabular(mapping = list(content = text.col, id = id.col))
  corpus <- Corpus(DataframeSource(input.df), readerControl = list(reader = title.reader))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weighting))
  return (dtm)
}

dtm <- get.dtm('text', 'doc_id', tweet.final, "weightTfIdf")
dtm.mat <- as.matrix(dtm)

#_______________________________________________________________________________
#_______________________________________________________________________________
# Term frequency inverse document frequency
dtm.mat[1:2, 10:15]


#_______________________________________________________________________________
#_______________________________________________________________________________
# Delta TFIDF
# Calculate Delta TFIDF in R
dtm <- get.dtm('text', 'doc_id', tweet.final, 'weightTf')
dtm

# Throw away some terms and try to reduce the sparsity of our document term matrix
dtm <- removeSparseTerms(dtm, 0.98)
dtm

dtm.mat <- as.matrix(dtm)

# Split our data into a positive tweets dataset and a negaitve tweets dataset
# and get their respective document term matricies
dtm.pos <- get.dtm('text', 'doc_id', tweet.final[tweet.final$sentiment == 'Positive',], "weightBin")
dtm.neg <- get.dtm('text', 'doc_id', tweet.final[tweet.final$sentiment == 'Negative',], "weightBin")

dtm.pos.mat <- as.matrix(dtm.pos)
dtm.neg.mat <- as.matrix(dtm.neg)

# Find the document frequencies of words in both the corpuses
pos.words.df <- colSums(dtm.pos.mat)
neg.words.df <- colSums(dtm.neg.mat)

# Get all the unique words and the document ids
tot.features <- colnames(dtm.mat)
doc.ids <- rownames(dtm.mat)

# Calculate the Dealta TFIDF
c.dtm.mat <- dtm.mat

for( i in 1:length(tot.features)) {
  for ( j in 1:length(doc.ids)) {
    # Number of times the term has occurred in the document
    ctd <- dtm.mat[doc.ids[j], tot.features[i]]
    # Number for documents in pos data with the term
    pt <- pos.words.df[tot.features[i]]
    # Number for documents in pos data with the term
    nt <- neg.words.df[tot.features[i]]
    score <- ctd * log( nt / pt)
    if(is.na(score)){
      score <- 0 }
  }
  c.dtm.mat[doc.ids[j], tot.features[i]] <- score
}


#_______________________________________________________________________________
#_______________________________________________________________________________
# Building a sentiment classifier
# install.packages("naivebayes")
library(naivebayes)

# Build our classifier
model <- naive_bayes(x = dtm.mat, y = tweet.final$sentiment, usekernel = TRUE)
str(model)

# Predictions using our models
preds <- predict(model, newdata = dtm.mat, type = "class")
library(caret)
confusionMatrix(preds, tweet.final$sentiment)
plot(model)

