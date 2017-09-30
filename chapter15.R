install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)

data <- read.csv("JEOPARDY_CSV.csv", stringsAsFactors = F, nrows=10000)
glimpse(data)

data_corpus <- Corpus(VectorSource(data$Question))
data_corpus

data_corpus <- tm_map(data_corpus, content_transformer(tolower))
data_corpus <- tm_map(data_corpus, removePunctuation)
data_corpus <- tm_map(data_corpus, removeWords, stopwords('english'))

data_corpus <- tm_map(data_corpus, stemDocument)

wordcloud(data_corpus, max.words=100, random.order=F, 
          colors=brewer.pal(8, "Dark2"))
