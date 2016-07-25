
# Install required packages
install.packages("twitteR")
install.packages("httpuv")
install.packages("RWeka")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("stringi")
install.packages("slam")



# Load Libraries
library(twitteR)
library(RWeka)
library(tm)
library(wordcloud)
library(SnowballC)
library(stringi)
library(slam)

access_token_secret <- '<access token secret>'
api_key <- '<api key>'
api_secret <- '<api secret>'
access_token <- '<access token>'

hash_tag <- 'SundayFunday'
number <- 600

setup_twitter_oauth(api_key,api_secret)

twitter_search <- searchTwitter(hash_tag, number)
twitter_text <- strip_retweets(twitter_search)
twitter_text<- lapply(twitter_search,function(t)t$getText())

removewords2 <- c(stopwords("english"), tolower(hash_tag), "tiffanypresley", "amp", "gt gt","don t", "doesn t")

corpus <- VCorpus(VectorSource(twitter_text))
corpus <- tm_map(corpus, function(x) iconv(x, to ="UTF-8"))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
# remove urls
corpus <- tm_map(corpus, function(x) gsub('http.* *', '', x))
# remove all non-alphabetic characters
corpus <- tm_map(corpus, function(x) gsub('@*', '', x))
# corpus <- tm_map(corpus, function(x) gsub('rt*', '', x))
corpus <- tm_map(corpus, function(x) gsub("rt * [a-z]+", "", x))
# corpus <- tm_map(corpus, function(x) gsub('[^[:alnum:]]', '', x))
corpus <- tm_map(corpus, function(x) gsub("[^A-z]", " ", x))
corpus <- tm_map(corpus, function(x) stri_replace_all_regex(as.character(x), "<.+?>", " "))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, removewords2)
corpus <- tm_map(corpus, stemDocument, language="english")
corpus <- tm_map(corpus, PlainTextDocument)


wordcloud(corpus, scale = c(5,.5), min.freq = 10, max.words = 100, random.order = TRUE, rot.per = .35, colors= brewer.pal(8, "Dark2"))

unigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
unigram_text <- TermDocumentMatrix(corpus, control = list(tokenizer = unigram_tokenizer))

bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))
bigaram_text <- TermDocumentMatrix(corpus, control = list(tokenizer =bigram_tokenizer))



frequency <- function(x){
              word_freq <- sort(row_sums(x), decreasing = TRUE)
              data.frame(Word=names(word_freq), count = word_freq)
}

unigram_df <- frequency(unigram_text)
bigram_df <- frequency(bigram_text)

wordcloud(unigram_df$Word, unigram_df$count,min.freq = 14, random.order = TRUE, rot.per = .35, max.words = 75, colors = brewer.pal(8, "Dark2"))
wordcloud(bigram_df$Word, bigram_df$count, scale = c(5,.5),min.freq = 57, random.order = TRUE, max.words = 75, colors = brewer.pal(8, "Dark2"))

