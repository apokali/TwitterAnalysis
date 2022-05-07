# word clouds
# Reference links: https://mikeyharper.uk/creating-twitter-wordclouds/

# Load required libraries
require(rtweet)
require(tm)
require(qdapRegex)
require(wordcloud2)

# Twitter authentication
get_token()

#_______________________________________________________________________________
#_______________________________________________________________________________
## search for 1000 tweets
tweet.df <- search_tweets(q = '#taylorswift', n = 1000, lang = 'en', include_rts = FALSE)
head(tweet.df$text)
tweet.df <- data.frame(tweet.df['text'])

library(qdapRegex)
# Data cleaning
tweet.df$text <- str_c(tweet.df$text, collapse = "") %>%
  str_remove("\\n") %>%   # remove line breakers
  rm_twitter_url() %>%
  rm_url() %>%
  str_remove_all("#\\S+") %>%
  str_remove_all("@\\S+") %>%
  removeWords(stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))


#_______________________________________________________________________________
#_______________________________________________________________________________
## word cloud
# Convert the data into a summary table
textCorpus <-
  Corpus(VectorSource(tweet.df$text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

# save in to wordcloud
library(wordcloud2)
textCorpus <- sort(rowSums(textCorpus), decreasing = TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)

wordcloud
