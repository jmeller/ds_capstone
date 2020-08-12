# imports
library(tm)
library(dplyr)
library(parallel)
library(SnowballC)

# data loading
fnames <- list.files("Coursera-SwiftKey/final/en_US", full.names = TRUE)

fn_readfile <- function(fname, sampling=FALSE, n=1000){
  con <- file(fname, "r") 
  
  result_list <- list()
  
  while(length(result_list) < n | !sampling) {
    
    if (sampling){
      if(rbinom(1, 1, 0.5)){ # sample whether or not to consider this line for the text sample
        result_list[length(result_list) + 1] <- readLines(con, 1)
      }
    } else {
      nextline <-  readLines(con, 1)
      if(length(nextline) > 0){ # sample whether or not to consider this line for the text sample
        result_list[length(result_list) + 1] <- nextline
      } else {
        break
      }
    }
  }
  
  close(con) # close connection
  return(result_list)
}

# full texts
us_texts_vector <- lapply(fnames, fn_readfile, sampling=FALSE) %>% Reduce(f=append, x=.) %>% unlist
us_texts <- SimpleCorpus(VectorSource(us_texts_vector))

# subsampling
us_texts_subsample_vector <- lapply(fnames, fn_readfile, sampling=T, n=10000) %>% Reduce(f=append, x=.) %>% unlist
us_texts_subsample <- SimpleCorpus(VectorSource(us_texts_subsample_vector))
