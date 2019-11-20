# libraries
library(tidyverse)

# config
fn_en_twitter <- paste0("C:/Users/jam19vl/Documents/private/coursera_capstone/",
                   "Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
fn_en_blogs <- paste0("C:/Users/jam19vl/Documents/private/coursera_capstone/",
                     "Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
fn_en_news <- paste0("C:/Users/jam19vl/Documents/private/coursera_capstone/",
                     "Coursera-SwiftKey/final/en_US/en_US.news.txt")
con <- file(fn_en_twitter, "r") 
sample_list <- list()
n_sample = 1000

# data loading - sampling
while(length(sample_list) < n_sample) {
  if(rbinom(1, 1, 0.5)){ # sample whether or not to consider this line for the text sample
    sample_list[length(sample_list) + 1] <- readLines(con, 1)
  }
}

all_twitter <- list()
# data loading - all
while(TRUE) {
  nextline <-  readLines(con, 1)
  if(length(nextline) > 0){ # sample whether or not to consider this line for the text sample
    all_twitter[length(all_twitter) + 1] <- nextline
  } else {
    break
  }
}

close(con) # close connection

# get max nchar of data set
get_max_length <- function(filename){
  con <- file(filename, 'r')
  
  max = 0
  while(TRUE) {
    nextline <-  readLines(con, 1)
    if(length(nextline) > 0){
      max <- max(nchar(nextline), max)
    } else {
      break
    }
  }
  close(con) # close connection
  
  return(max)
}

# get max lenghts
max_length_twitter <- get_max_length(fn_en_twitter)
max_length_blogs <- get_max_length(fn_en_blogs)
max_length_news <- get_max_length(fn_en_news)

# get regex matches
get_regex <- function(filename, regex){
  con <- file(filename, 'r')
  
  result_list <- list()
  while(TRUE) {
    nextline <-  readLines(con, 1)
    if(length(nextline) > 0){
      extract <- grep(regex, nextline, value = TRUE)
      if(length(extract) > 0)
        result_list[length(result_list) + 1] <- extract
    } else {
      break
    }
  }
  close(con) # close connection
  
  return(result_list)
  
}

n_love_twitter <- get_regex(fn_en_twitter, "love")
n_hate_twitter <- get_regex(fn_en_twitter, "hate")
biostats_twitter <- get_regex(fn_en_twitter, "biostats")

length(n_love_twitter)/length(n_hate_twitter)

saying_twitter <- get_regex(fn_en_twitter, "^A computer once beat me at chess, but it was no match for me at kickboxing$")
