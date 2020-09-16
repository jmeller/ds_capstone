# We are doing LANGUAGE MODELING

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import dependencies
library(tm)
library(quanteda)
library(dplyr)
library(tidyverse)
library(data.table)
#source('../week-2/02_analysis.R')

# get the term frequencies of ngrams
new_corpus <- corpus(us_texts)
set.seed(2608)
new_sample <- corpus_sample(new_corpus, size=length(new_corpus)*0.1)

word_frequencies <- new_sample %>% dfm %>% featfreq
bigram_frequencies <- new_sample %>% tokens %>% tokens_ngrams(2) %>% dfm %>% featfreq
trigram_frequencies <- new_sample %>% tokens %>% tokens_ngrams(3) %>% dfm %>% featfreq
fourgram_frequencies <- new_sample %>% tokens %>% tokens_ngrams(4) %>% dfm %>% featfreq

word_dt = data.table(term=names(word_frequencies), freq=word_frequencies)
bigram_dt = data.table(term=names(bigram_frequencies), freq=bigram_frequencies)
trigram_dt = data.table(term=names(trigram_frequencies), freq=trigram_frequencies)
fourgram_dt = data.table(term=names(fourgram_frequencies), freq=fourgram_frequencies)

# TO-DO: need input tokenizer + logic first

base_freq <- bigram_dt %>% filter(str_detect(term, "^i_can't$")) %>% select(freq) %>% unlist
bigram_dt %>% filter(str_starts(term, "i_")) %>% arrange(desc(freq))
test <- trigram_dt %>% filter(str_starts(term, "i_can't_")) %>% arrange(desc(freq)) %>% head(3) %>% data.table %>% .[,prob := freq/base_freq]
print(test)



# implement tokenizer
x <- "I can't"
Boost_tokenizer(x)
unigrams <- tokens(x) %>% tokens_tolower
bigrams <- unigrams %>% tokens_ngrams(2)

bf <- bigram_frequencies[unlist(bigrams)]
wf <- word_frequencies[unigrams %>% unlist() %>% tail(1)]
prob <- bf/wf
prob



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