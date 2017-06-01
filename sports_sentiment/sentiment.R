library(stringr)
library(tidyverse)
library(magrittr)
library(rvest)
library(tidytext)
library(syllable)
library(readability)
library(sentimentr)
source("asap_scraper.R")

setwd("~/future/Text/sports_sentiment/")

## Ins ------------------------------------------------------------------------

# sourced from : http://www.asapsports.com/
# 4 sports 
sources <- c("http://www.asapsports.com/show_event.php?category=5&date=2017-5-30&title=NHL+STANLEY+CUP+FINAL%3A+PREDATORS+VS+PENGUINS",
           "http://www.asapsports.com/show_event.php?category=11&date=2017-5-25&title=NBA+EASTERN+CONFERENCE+FINALS%3A+CELTICS+VS+CAVALIERS",
           "http://www.asapsports.com/show_event.php?category=4&date=2017-5-28&title=BMW+PGA+CHAMPIONSHIP",
           "http://www.asapsports.com/show_event.php?category=3&date=2017-5-28&title=MONSTER+ENERGY+NASCAR+CUP+SERIES%3A+COCA-COLA+600") %>%
               set_names(c("nhl", "nba", "pga", "nascar"))

sports <- map_df(sources, asap_scraper, .id = "sport")

table(sports$speaker, useNA = "always")

sports %<>% filter(speaker != "THE MODERATOR")

## Readability ----------------------------------------------------------------

read_scores <- with(sports, readability(text, list(sport, speaker))) %>%
    gather(method, score, -(sport:speaker))
    
ggplot(read_scores, aes(sport, score, color = sport, fill = sport)) +
    stat_summary(fun.data = mean_cl_normal, geom = "crossbar", alpha = .25) +
    geom_point(size = 4, alpha = .5) +
    facet_wrap(~ method) +
    coord_flip()

# Kyle Busch nice job
# JR Smith no comment

word_scores <- with(sports, readability_word_stats_by(text, list(sport, speaker)))
# oh they only have a few comments ...

filter(sports, speaker %in% c("J.R. SMITH", "KYLE BUSCH"))
# ahhh just sentences with high and low syllable words

# drop them
word_scores %<>% filter(!(speaker %in% c("J.R. SMITH", "KYLE BUSCH")))

ggplot(word_scores, aes(sport, n.sents, color = sport)) +
    stat_summary(fun.data = mean_cl_normal, geom = "crossbar") +
    geom_point()
# looks like a coupel of inteview have more weight

## Sentiment -------------------------------------------------------------------
sports %<>% filter(!(speaker %in% c("J.R. SMITH", "KYLE BUSCH")))

sents <- with(sports, sentiment_by(text) ) %>%
    select(words = word_count,
           sent = ave_sentiment) %>%
    bind_cols(sports, .)
    
# make initials for axis
sents %<>% mutate(initials = gsub("(^\\D).* (\\D).*", "\\1\\2", speaker))

ggplot(sents,
       aes(initials, sent, color = sport)) +
    geom_point() +
    facet_grid(. ~ sport,
               scales = "free_x")

filter(sents, sent < -0.5) %>%
    select(text) %>%
    unlist()

sents %<>% filter(sent > -0.5)

# another library
nrc <- get_sentiments("nrc")

# unnest_tokens to words
words <- unnest_tokens(select(sents,sport:speaker, initials), word, text) %>%
    inner_join(nrc) %>%
    group_by(sport, speaker, initials) %>%
    count(sentiment)

ggplot(words, aes(sentiment, y = n, fill = sentiment)) +
    geom_col() +
    facet_wrap(sport ~ speaker, scales = "free_y") +
    scale_fill_d3()

# collaps to sport level and use proportions
sport_words <- group_by(words, sport, sentiment) %>%
    summarise(n = sum(n)) %>%
    mutate(prop = n / sum(n))

ggplot(sport_words, aes(sentiment, prop, fill = sentiment)) +
    geom_col() +
    facet_grid(~ sport) +
    scale_fill_d3()

ggplot(sport_words, aes(sport, prop, fill = sport)) +
    geom_col() +
    facet_wrap(~ sentiment, scales = "free") +
    scale_fill_d3()
