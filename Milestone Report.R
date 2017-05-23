library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(tidytext)
library(wordcloud)
library(widyr)
library(igraph)
library(ggraph)

# i'm type-lazy
setwd("~/future/Text_Prediction/")

# this doesn't help with the ' fiasco
# encoding is scary
getOption("encoding")
options(encoding = "UTF-8")

# # readLines() for character vectors and speed
# news <- readLines("data/en_US/en_US.news.txt")
# twitter <- readLines("data/en_US/en_US.twitter.txt")
# blogs <- readLines("data/en_US/en_US.blogs.txt")
# 
# # store them in a named list
# sources <- list(news = news, twitter = twitter, blogs = blogs) # saves a ton of typing for batch processes
# 
# # sample down to 10%
# set.seed(1)
# small_sources <- map(sources, ~ sample(., length(.) * .10))
# 
# # convert to a tibble structure from character; also let's index entries
# tidy_sources <- map(small_sources, ~ tibble(text = ., entry = 1:length(.)))
# 
# saveRDS(tidy_sources, "data/tidy_sources.RDS")

tidy_sources <- readRDS("data/tidy_sources.rds")
# unnest_tokens as single words
word_sources <- map(tidy_sources, ~ unnest_tokens(., word, text))

# tweaked drob's method for apotrophes
word_sources %<>% map(~ mutate(., word = str_extract(word, "[a-z]+'?[a-z]+")) %>%
                          filter(!is.na(word)))

# plot the top 20
plot_df <- word_sources %>%
    map_df(~ count(., word, sort = T) %>%
               top_n(20), .id = "source") %>% 
    mutate(word = reorder(word, n))

ggplot(plot_df, aes(x = word, y = n, fill = source)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap(~ source, scales = "free")

# drop some stop words (blahs; low to no impact words; like the top 20 (the, to, and, ...))
# according to 3 lexicons
data(stop_words)

# clean stop_words just like above
stop_words %<>% mutate(word = str_extract(word, "[a-z]+'?[a-z]+")) %>% # solved the apostrophe fiasco
      filter(!is.na(word))

# do the work with dplyr::anti_join()
word_sources %<>% map(~ anti_join(., stop_words))

# drop chopped up apostrophe words
stop_words %<>% full_join(tibble(word = c("i", "m", "ve", "t", "don", "can", "st"), lexicon = "cust"))
word_sources %<>% map(~ anti_join(., stop_words))

# now re-plot the top 20
plot_df2 <- word_sources %>%
    map_df(~ count(., word, sort = T) %>%
               top_n(20), .id = "source") %>% 
    mutate(word = reorder(word, n)) # for plotting pretty ;)

ggplot(plot_df2, aes(x = word, y = n, fill = source)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap(~ source, scales = "free")

# takes care of 'rt', 'im', 'follow'; probably not true text, really it's part of twitter's structure ReTweet and InstantMessage

# lets full_join so when we come back alter we don't have to rerun everything :)
stop_words %<>% full_join(tibble(word = c("rt", "im", "follow"), lexicon = "cust"))
# saver
saveRDS(stop_words, "data/stop_words.RDS")
word_sources %<>% map(~ anti_join(., stop_words))

## Sentiment Analysis ---------------------------------------------------------
nrc_feels <- get_sentiments("nrc") %>% 
    mutate(word = str_extract(word, "[a-z]+'?[a-z]+")) %>% 
    filter(sentiment %in% c("positive", "negative"), !is.na(word))
    
# inner_join to do the work
feels <- map(word_sources, ~ inner_join(., nrc_feels))


## Word Clouds -----------------------------------------------------------------
# watch out this is base graphics :()
par(mfrow = c(1,3), mar = rep(0,4))
pal <- brewer.pal(7, "Dark2")


# make a list ready to pipe
feels_plot <- feels %>%
    map(~ count(., word, sentiment, sort = T) %>%
            top_n(100)) %>%
    map(~ with(., wordcloud(word, n, max.words = 100, colors = pal)))
# need to work out titling still
# maybe look at tm:comparison.cloud as a alt solution

## N-Grams ---------------------------------------------------------------------

## Bigrams

# unnest
g2_sources <- map(tidy_sources, ~ unnest_tokens(., gram, text, token = "ngrams", n = 2))

# clean with tidyr::separate()
g2_sources_clean <- g2_sources %>% map(~ separate(., gram, c("w1", "w2"), sep = " ") %>% # now use our strategy form above
                        mutate_at(vars(starts_with("w")), funs(str_extract(., "[a-z]+'?[a-z]+"))) %>%
                        filter(!is.na(w1),
                               !is.na(w2),
                               !w1 %in% stop_words$word,
                               !w2 %in% stop_words$word) %>%
                        unite(gram, w1, w2, sep = " "))

# count
plot_df <- g2_sources_clean %>%
    map_df(~ count(., gram, sort = T) %>%
               top_n(20), .id = "source") %>% 
    mutate(gram = reorder(gram, n))
# takes a little longer; remember grams doubles up size, for full coverage

# plot
ggplot(plot_df, aes(gram, n, fill = source)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap(~ source, scales = "free")

## Trigrams

# unnest
g3_sources <- map(tidy_sources, ~ unnest_tokens(., gram, text, token = "ngrams", n = 2))

# clean with tidyr::separate()
g3_sources_clean <- g3_sources %>% map(~ separate(., gram, c("w1", "w2", "w3"), sep = " ") %>% # now use our strategy form above
                                           mutate_at(vars(starts_with("w")), funs(str_extract(., "[a-z]+'?[a-z]+"))) %>%
                                           filter(!is.na(w1),
                                                  !is.na(w2),
                                                  !is.na(w3),
                                                  !w1 %in% stop_words$word,
                                                  !w2 %in% stop_words$word,
                                                  !w3 %in% stop_words$word) %>%
                                           unite(gram, w1, w2, w3, sep = " "))

# count
plot_df <- g3_sources_clean %>%
    map_df(~ count(., gram, sort = T) %>%
               top_n(20), .id = "source") %>% 
    mutate(gram = reorder(gram, n))
# takes a little longer; remember grams doubles up size, for full coverage

# plot
ggplot(plot_df, aes(gram, n, fill = source)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap(~ source, scales = "free")


## Word Pairs ------------------------------------------------------------------
# twitter is the most conversationally relevant data set
twitter <- word_sources[["twitter"]]

pairs_count <- twitter %>% 
    pairwise_count(word, entry, sort = T)

pairs_cors <- twitter %>%
    group_by(word) %>% # filter for relatively common words first
    filter(n() > 20) %>%
    pairwise_cor(word, entry, sort = T)

cor_net <- pairs_cors %>%
    filter(correlation > .30) %>% # 202 rows qualify
    graph_from_data_frame()

ggraph(cor_net, layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()

## Model next word ------------------------------------------------------------
tidy_sources <- readRDS("data/tidy_sources.RDS")
stop_words <- readRDS("data/stop_words.RDS")


# want to use bigrams
g2_twitter <- tidy_sources[["twitter"]] %>%
    unnest_tokens(gram, text, token = "ngrams", n = 2)

# separate again to clean but leave separated for pairwise work
g2_twitter %<>% separate(gram, c("w1", "w2"), sep = " ") %>%
                       mutate_at(vars(starts_with("w")), funs(str_extract(., "[a-z]+'?[a-z]+"))) %>%
                       filter(!is.na(w1),
                              !is.na(w2),
                              !w1 %in% stop_words$word,
                              !w2 %in% stop_words$word) # consider adding  "w1 != w2"

g2_counts <- g2_twitter %>%
    group_by(w1) %>%
    count(w2, sort = T) %>%
    filter(n > 2)
    
# make a large network
tnet <- graph_from_data_frame(g2_counts[,1:2])
# weight the edges
E(tnet)$weight <- g2_counts$n

# get value for node by name
v <- grep("happy", V(tnet)$name)[1]

small_net <- make_ego_graph(tnet, order = 1, nodes = v)[[1]]

ggraph(small_net, layout = "nicely") + 
    geom_edge_link(aes(width = log(weight),
                       alpha = log(weight)),
                   edge_colour = "purple",
                   show.legend = F) +
        geom_node_label(aes(label = name)) +
        theme_void()


## topn worflow ----------------------------------------------------------------
my_n <- 10

# grab the top3 for each w1
topn_list <- g2_counts %>%
    split(.$w1) %>%
    map(~ top_n(., my_n)) # these are the trade-offs we make

# looks good, but some groups don't have 3 preditions

# # let's fill this gap with the top overal entries
# words_twitter <- tidy_sources[["twitter"]] %>%
#     unnest_tokens(word, text, token = "words")
# 
# words_twitter %<>% anti_join(stop_words)
# words_twitter %<>% mutate(word = str_extract(word, "[a-z]+'?[a-z]+")) %>%
#     filter(!is.na(word))
# # takes a while but will not run frequently
# 
# topn_overall <- count(words_twitter, word, sort = T) %>%
#     top_n(my_n) %>%
#     select(word) %>%
#     unlist()

# long term we could do this within the specific og document (i.e. chapter, post, etc...)

# anon() for map(.f)
anon <- function(df) {
    if (nrow(df) < my_n) {
        df %<>% bind_rows(tibble(w1 = unique(df$w1),
                                w2 = topn_overall[1:(my_n - nrow(df))],
                                n = 1) ) }
    return(df) }

topn_list %<>% map(anon)

# lets ungroup and lean it out
topn_lean <- map(topn_list, ~ ungroup(.) %>%
                     select(pred = w2, n) %>%
                     mutate(p = n / sum(n)))

topn_lean[["fast"]]
system.time( topn_lean[["fast"]] )
