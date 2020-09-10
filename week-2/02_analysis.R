# Import dependencies -----------------------------------------------------
library(tm)
library(dplyr)
library(parallel)
library(SnowballC)
library(ngram)
library(tidyverse)
library(wordcloud)

# Data loading ------------------------------------------------------------

# path to content
fnames <- list.files("../Coursera-SwiftKey/final/en_US", full.names = TRUE)

fn_readfile <- function(fname, sampling=FALSE, n=1000){
  con <- file(fname, "r") 
  
  result_list <- list()
  
  while(length(result_list) < n | !sampling) {
    
    if (sampling){
      if(rbinom(1, 1, 0.5)){ # sample whether or not to consider this line for the text sample
        nextline <-  readLines(con, 1, encoding="UTF-8", warn=F)
        if(length(nextline) > 0){
          result_list[length(result_list) + 1] <- nextline
        } else {
          break
        }
      }
    } else {
      nextline <-  readLines(con, 1, encoding="UTF-8", warn=F)
      if(length(nextline) > 0){
        result_list[length(result_list) + 1] <- nextline
      } else {
        break
      }
    }
  }
  
  close(con) # close connection
  return(result_list)
}

# subsampling
us_texts_subsample <- lapply(fnames, fn_readfile, sampling=T, n=100000) %>% 
  Reduce(f=append, x=.) %>% 
  unlist %>%
  VectorSource %>%
  SimpleCorpus

# full texts
us_texts_list <- lapply(fnames, fn_readfile, sampling=FALSE)
us_texts_vector <- us_texts_list %>% Reduce(f=append, x=.) %>% unlist
us_texts <- SimpleCorpus(VectorSource(us_texts_vector))

# Preprocessing -----------------------------------------------------------

# define stop words
word_removal <- function(x) removeWords(x, stopwords("english"))

preprocess_fns <- list(
  stripWhitespace,
  removeNumbers,
  removePunctuation #,
  # word_removal
)

us_preprocessed_subsample <- tm_map(us_texts_subsample, content_transformer(tolower)) %>%
  tm_map(FUN = tm_reduce, tmFuns = preprocess_fns)

# us_preprocessed <- tm_map(us_texts, content_transformer(tolower)) %>%
#   tm_map(FUN = tm_reduce, tmFuns = preprocess_fns)

# document-term matrix
dtm <- DocumentTermMatrix(us_preprocessed_subsample, control=list(wordLengths=c(-1, 20)))

sparsity_threshold <- 0.999
freq <- colSums(as.matrix(removeSparseTerms(dtm, sparsity_threshold)))
ord <- order(freq, decreasing=TRUE)


# Exploratory data analysis -----------------------------------------------

# no. of total documents
doc_overview <- data.frame(file=fnames, no_docs=lengths(us_texts_list))

# histograms
plot_data <- data.frame(term=names(freq),occurrences=freq)
plot_frequencies <- data.frame(term=names(freq),occurrences=freq) %>% filter(occurrences > 15000) %>% 
  ggplot(aes(x=reorder(term, -occurrences), y=occurrences)) + geom_bar(stat='identity') + theme_bw() + theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) + labs(x='term', y='# of occurrences')
plot_frequencies

least_frequent <- freq[tail(ord, n = 10)]

# n-grams
strings <- concatenate(lapply(us_preprocessed_subsample, "[", 1))

fg <- ngram(strings, n=4)
fg_phrases <- get.phrasetable(fg)

tg <- ngram(strings, n=3)
tg_phrases <- get.phrasetable(tg)

bigram <- ngram(strings, n=2)
bg_phrases <- get.phrasetable(bigram)

unigram <- ngram(strings, n=1)
ug_phrases <- get.phrasetable(unigram)


# Playground --------------------------------------------------------------


