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
fivegram_frequencies <- new_sample %>% tokens %>% tokens_ngrams(5) %>% dfm %>% featfreq
sixgram_frequencies <- new_sample %>% tokens %>% tokens_ngrams(6) %>% dfm %>% featfreq

word_dt = data.table(term=names(word_frequencies), freq=word_frequencies)
bigram_dt = data.table(term=names(bigram_frequencies), freq=bigram_frequencies)
trigram_dt = data.table(term=names(trigram_frequencies), freq=trigram_frequencies)
fourgram_dt = data.table(term=names(fourgram_frequencies), freq=fourgram_frequencies)
fivegram_dt = data.table(term=names(fivegram_frequencies), freq=fivegram_frequencies)
sixgram_dt = data.table(term=names(sixgram_frequencies), freq=sixgram_frequencies)

# TO-DO: need input tokenizer + logic first

input_string <- "must be"
# The guy in front of me just bought a pound of bacon, a bouquet, and a case of
# You're the reason why I smile everyday. Can you follow me please? It would mean the
# Hey sunshine, can you follow me and make me the
# Very early observations on the Bills game: Offense still struggling but the
# Go on a romantic date at the
# Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my
# Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some
# After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little
# Be grateful for the good times and keep the faith during the
# If this isn't the cutest thing you've ever seen, then you must be
input_tokens <- tokens(input_string) %>% tokens_tolower %>% unlist
n_tokens <- input_tokens %>% length %>% min(5) # trim number of tokens due to maximum of fivegrams pre-saved

search_tokens <- input_tokens %>% tail(n_tokens) %>% list %>% tokens %>% tokens_ngrams(n_tokens) %>% unlist
search_string <- paste0("^", search_tokens, "$")

# TO-DO choose ngram depending on token length
if (n_tokens == 5) {
  base_ngram <- fivegram_dt
  pred_ngram <- sixgram_dt
} else if (n_tokens == 4) {
  base_ngram <- fourgram_dt
  pred_ngram <- fivegram_dt
} else if (n_tokens == 3) {
  base_ngram <- trigram_dt
  pred_ngram <- fourgram_dt
} else if (n_tokens == 2) {
  base_ngram <- bigram_dt
  pred_ngram <- trigram_dt
} else {
  base_ngram <- word_dt
  pred_ngram <- bigram_dt
}

# no of top predictions
n_words <- 3
base_freq <- base_ngram %>% filter(str_detect(term, search_string)) %>% select(freq) %>% unlist
pred_freq <- pred_ngram %>% filter(str_starts(term, paste0("^", search_tokens, "_"))) %>% arrange(desc(freq)) %>% head(n_words) %>% data.table %>% .[,prob := freq/base_freq]
print(pred_freq)




##### WIP ####
input_string <- "must be a"


input_tokens <- tokens(input_string) %>% tokens_tolower %>% unlist
max_tokens <- input_tokens %>% length %>% min(5) # trim number of tokens due to maximum of fivegrams pre-saved

dcf <- 1
n_tokens <- max_tokens
stop = FALSE
while (stop != TRUE) {
  print(n_tokens)
  switch(n_tokens,
         "5" = {
           print("hello5")
           n_tokens <<- n_tokens - 1
           dcf <- dcf*0.4
           print(dcf)
         },
         "4" = {
           print("hello4")
           n_tokens <<- n_tokens - 1
           dcf <- dcf*0.4
           print(dcf)
         },
         "3" = {
           print("hello3")
           n_tokens <<- n_tokens - 1
           print(n_tokens)
           dcf <- dcf*0.4
           print(dcf)
         },
         "2" = {
           print("hello2")
           n_tokens <<- n_tokens - 1
           dcf <- dcf*0.4
           print(dcf)
         },
         "1" = {
           print("hello1")
           n_tokens <<- n_tokens - 1
           dcf <- dcf*0.4
           print(dcf)
           stop <<- TRUE
           print(stop)
         }
  )
  Sys.sleep(10)
  print(get('stop'))
  print(stop)
}

search_tokens <- input_tokens %>% tail(n_tokens) %>% list %>% tokens %>% tokens_ngrams(n_tokens) %>% unlist
search_string <- paste0("^", search_tokens, "$")










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