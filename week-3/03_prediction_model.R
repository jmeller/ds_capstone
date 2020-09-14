# We are doing LANGUAGE MODELING

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import dependencies
library(tm)
source('../week-2/02_analysis.R')

# implement tokenizer
x <- "I can't"
Boost_tokenizer(x)


# get the term frequencies of ngrams
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

BigramTokenizer(content(us_preprocessed_subsample[[1]]))

dtm <- DocumentTermMatrix(us_preprocessed_subsample[1:10], control = list(tokenize = BigramTokenizer))
inspect(dtm)
findMostFreqTerms(dtm)

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