## Globals --------------------------------------------------------------------
library(stringr)
library(tidyverse)
# ggplot
theme_set(theme_classic() +
              theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
                    strip.background = element_rect(linetype = "blank"),
                    panel.grid.minor = element_line(colour="grey90", size=0.5),
                    panel.grid.major = element_line(colour="grey90", size=0.5),
                    legend.position = "none"))

library(magrittr)
library(rvest)
# colors
library(ggsci)
source("asap_scraper.R")

setwd("~/future/Text/sports_sentiment/")

## Ins ------------------------------------------------------------------------
# ty tidyverse team and R-Studio

# sourced from : http://www.asapsports.com/
# 4 sports 
sources <- c("http://www.asapsports.com/show_event.php?category=5&date=2017-5-30&title=NHL+STANLEY+CUP+FINAL%3A+PREDATORS+VS+PENGUINS",
           "http://www.asapsports.com/show_event.php?category=11&date=2017-5-25&title=NBA+EASTERN+CONFERENCE+FINALS%3A+CELTICS+VS+CAVALIERS",
           "http://www.asapsports.com/show_event.php?category=4&date=2017-5-28&title=BMW+PGA+CHAMPIONSHIP",
           "http://www.asapsports.com/show_event.php?category=3&date=2017-5-28&title=MONSTER+ENERGY+NASCAR+CUP+SERIES%3A+COCA-COLA+600") %>%
               set_names(c("nhl", "nba", "pga", "nascar"))

sports <- map_df(sources, asap_scraper, .id = "sport")

table(sports$speaker, useNA = "always")

# get that non-athlete outa' here
sports %<>% filter(speaker != "THE MODERATOR")

## Readability ----------------------------------------------------------------
# ty Tyler Rinker
library(syllable)
library(readability)

read_scores <- with(sports, readability(text, list(sport, speaker))) %>%
    gather(method, score, -(sport:speaker))
    
ggplot(read_scores, aes(sport, score, color = sport)) +
    stat_summary(fun.data = mean_cl_normal, geom = "crossbar", width = .5) +
    geom_point(size = 4, alpha = .5) +
    facet_wrap(~ method) +
    coord_flip() + 
    scale_color_d3() +
    scale_fill_d3()

# Kyle Busch nice job
# JR Smith no comment

# why is that
word_scores <- with(sports, readability_word_stats_by(text, list(sport, speaker)))

filter(sports, speaker %in% c("J.R. SMITH", "KYLE BUSCH"))
# ahhh just sentences with some high & low syllable words

# get 'em outta here
word_scores %<>% filter(!(speaker %in% c("J.R. SMITH", "KYLE BUSCH")))

# check high-freq-occurance influence
ggplot(word_scores, aes(sport, n.sents, color = sport)) +
    stat_summary(fun.data = mean_cl_normal, geom = "crossbar", width = .5) +
    geom_point(size = 4, alpha = .5) +
    coord_flip() + 
    scale_color_d3() +
    scale_fill_d3()


## Sentiment -------------------------------------------------------------------
# ty again Tyler Rinker Stackgod
library(sentimentr)

sents <- with(sports, sentiment_by(text) ) %>%
    select(words = word_count,
           sent = ave_sentiment) %>%
    bind_cols(sports, .)
    
# make initials to de-clutter axis
sents %<>% mutate(initials = gsub("(^\\D).* (\\D).*", "\\1\\2", speaker))

ggplot(sents,aes(initials, sent, color = sport)) +
    stat_summary(fun.data = mean_se, geom = "crossbar") +
    geom_point(size = 4, alpha = .5) +
    facet_grid(. ~ sport, scales = "free_x") +
    scale_color_d3() +
    scale_fill_d3()

# why is NICOLAS COLSAERTS having a bad day
filter(sents, sent < -0.5) %>%
    select(text) %>%
    unlist()

# drop that statment
sents %<>% filter(sent > -0.5)

# and drop if n == 1
sents %<>% group_by(initials) %>%
    filter(n() != 1) %>%
    ungroup()


## Another package, another method --------------------------------------------
# ty Julia Silge & David Robinson 
library(tidytext)

nrc <- get_sentiments("nrc")

# unnest_tokens to words
words <- unnest_tokens( select(sents, sport:speaker, initials), word, text) %>%
    inner_join(nrc) %>%
    group_by(sport, speaker, initials) %>%
    count(sentiment)

ggplot(words, aes(sentiment, y = n, fill = sentiment)) +
    geom_col() +
    facet_wrap(sport ~ speaker, scales = "free_y") +
    scale_fill_d3()

# collapse to sport level and use proportions
sport_words <- group_by(words, sport, sentiment) %>%
    summarise(n = sum(n)) %>%
    mutate(prop = n / sum(n))

ggplot(sport_words, aes(sentiment, prop, fill = sentiment)) +
    geom_col() +
    facet_grid(~ sport) +
    coord_flip() +
    scale_fill_d3()
    
ggplot(sport_words, aes(sport, prop, fill = sport)) +
    geom_col() +
    facet_wrap(~ sentiment, scales = "free") +
    scale_fill_rickandmorty() # it really exists
