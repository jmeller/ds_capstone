# We are doing LANGUAGE MODELING

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import dependencies
library(tm)
library(quanteda)
library(dplyr)
source('../week-2/02_analysis.R')

# implement tokenizer
x <- "I can't"
Boost_tokenizer(x)
unigrams <- tokens(x) %>% tokens_tolower
bigrams <- unigrams %>% tokens_ngrams(2)

bf <- bigram_frequencies[unlist(bigrams)]
wf <- word_frequencies[unigrams %>% unlist() %>% tail(1)]
prob <- bf/wf
prob

# get the term frequencies of ngrams
new_corpus <- corpus(us_texts)
set.seed(2608)
new_sample <- corpus_sample(new_corpus, size=length(new_corpus)*0.1)

word_frequencies <- new_sample %>% dfm %>% featfreq
bigram_frequencies <- new_sample %>% tokens %>% tokens_ngrams(2) %>% dfm %>% featfreq

bigram_dfm <- new_sample %>% tokens %>% tokens_ngrams(2) %>% dfm
bigram_dfm

first <- unigrams %>% unlist %>% head(1)
search_string <- paste0('^', unigrams %>% unlist %>% head(1), '_')
search_string
bigram_dfm %>% dfm_select(search_string, valuetype = 'regex')



tokens_ngrams(new_sample, 2L)
bigrams <- tokens(new_sample) %>% tokens_ngrams(2)
dfm <- dfm(bigrams)
frequencies <- featfreq(dfm)

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

BigramTokenizer(content(us_preprocessed_subsample[[1]]))

dtm <- DocumentTermMatrix(us_preprocessed_subsample[1:10])
inspect(dtm)
termFreq(us_preprocessed_subsample[[1]])

Boost_tokenizer(x) %>% ngrams(n=2)


test <- Boost_tokenizer(words(us_preprocessed_subsample[1:10]))
test

ngrams(words(us_preprocessed_subsample[[1]]), 2)
library(dplyr)

get_bigrams <- function(x){ngrams(x, 2)}
bigrams <- tm_map(us_preprocessed_subsample[1:10], FUN=get_bigrams)

content(us_preprocessed_subsample[[2]])
content(bigrams[[2]])




# implement stupid backoff algorithm
S(w_i) = count(w_i)/N

# smoothing via lambdas


# reduce size: pruning