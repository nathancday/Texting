#!/usr/bin/env Rscript

library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(tidytext)
library(doMC)
library(foreach)
registerDoMC(7)
getDoParWorkers()

setwd("~/Text/")
set.seed(1)
options(encoding = "UTF-8")

### Ins -----------------------------------------------------------------------
news <- readLines("en_US.news.txt")
twitter <- readLines("en_US.twitter.txt")
blogs <- readLines("en_US.blogs.txt")

fulls <- list(news, blogs, twitter) %>% set_names(c("n", "b", "t"))
fulls %<>% map_df(~ tibble(text = ., entry = 1:length(.)), .id = "source")
smalls <- sample_frac(fulls, .1)

rm(fulls, twitter, news, blogs)

ngrams1<- foreach (i = 2:5) %dopar% {
  unnest_tokens(smalls, gram, text, token = "ngrams", n = i) %>%
    mutate(next_word = gsub(".*( .*$)", " \\1", gram),
           gram = gsub("(.*) .*$", "\\1", gram) ) %>%
    group_by(gram) %>%
    count(next_word, sort = T) %>%
    mutate(total_next = sum(n)) %>%
    filter(n > 2) %>%
    slice(1:3)
}

ngrams1 <- readRDS("ngrams1.RDS")

### Guess ---------------------------------------------------------------------
best_guessN <- function(st) {
  
  st %<>% tolower() %>% str_extract_all("[a-z]+'?[a-z]*") %>% unlist()
  num_words <- length(st)
  
  if (num_words > 4) {
    st <- st[-(1:(num_words - 4))]
    num_words <- 4
  }
  
  st_guess <- paste(st, collapse = " ")
  
  guess <- ngrams1[[num_words]] %>% filter(gram == st_guess) %>%
    ungroup() %>%
    mutate(prop = n / total_next) %>%
    select(gram, next_word, prop)
  
  while (nrow(guess) < 1) {
    num_words <- num_words - 1
    st <- st[-1]
    st_guess <- paste(st, collapse = " ")
    guess <- ngrams1[[num_words]] %>% filter(gram == st_guess) %>%
      ungroup() %>%
      mutate(prop = n / total_next) %>%
      select(gram, next_word, prop)
  }
  return(guess)
}

### Building a sentence --------------------------------------------------------
# using the top result as the next word
best_guessN("I")
best_guessN("I have")
best_guessN("I have to")
best_guessN("I have to say")
best_guessN("I have to say that")
best_guessN("I have to say that I")
best_guessN("I have to say that I am")
best_guessN("I have to say that I am a")
best_guessN("I have to say that I am a little")
best_guessN("I have to say that I am a little late") # how did it know?

