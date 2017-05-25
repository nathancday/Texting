
library(stringr)
library(tidytext)
library(tidyverse)
library(magrittr)
library(wordcloud)

setwd("~/future/Text/sports_sentiment/")

# documents source from : http://www.asapsports.com/
# used most recent/first document based on date and page order with player quotes (not coaches of execs)
# possibly expand from this to more observations

nhl <- dir(".", "nhl") %>%
    map(readLines) %>%
    set_names(rep("nhl", length(.)))
pga <- dir(".", "pga") %>%
    map(readLines) %>%
    set_names(rep("pga", length(.)))
indy <- dir(".", "indy") %>%
    map(readLines) %>%
    set_names(rep("indy", length(.)))
nba <- dir(".", "nba") %>%
    map(readLines) %>%
    set_names(rep("nba", length(.)))

athletes <- c(nhl, pga, indy, nba)

rm(nhl, pga, indy, nba)
## Clean ----------------------------------------------------------------------

# tidy first
tidy <- map(athletes, ~tibble(text = .))

rm(athletes)
# want to clean header lines
map(tidy, ~head(., 15))

# look at line length
map(tidy, ~ str_count(.$text))

# use line great than 100 as start of actual conversation
start_lines <- map(tidy, ~ str_count(.$text)) %>%
    map(~ . > 100) %>%
    map(~ grep(T, .)[1])

# cut out headers from transcripts
tidy %<>% map2(start_lines, ~ .x[-(1:.y),])

# drop empty lines
empty_lines <- map(tidy, ~ str_count(.$text)) %>%
    map(~ . == 0)

#cut out empty lines
tidy %<>% map2(empty_lines, ~ .x[!(.y),])

# drop questions from reporters/moderators
tidy %<>% map(~ filter(., !grepl("^Q|^THE|^MICHAEL GIBBONS", text))) # c'mon Mike ;)

# remove answerer id prefix from text
tidy %<>% map(~ mutate(., text = gsub("^.*:", "", text)))

# unnest_tokens and reduce to tbl_df
words <- map_df(tidy, ~ unnest_tokens(., word, text), .id = "sport")

# drop stop words
data("stop_words")
words %<>% anti_join(stop_words)

# top100 viz per sport
counts <- words %>% split(.$sport) %>%
    map_df(~ count(., word, sort = T), .id = "sport")

library(ggsci) # coolest of color pals, D3!!!
pal <- pal_d3("category10")(10)

layout(matrix(c(1:8), ncol=2), heights=c(1, 4, 1, 4))
par(mar=c(1, 0, 1, 0), mfcol= c(2,2))
for (sport in unique(counts$sport)) {
    datf <- filter(counts, sport == sport)
    wordcloud(datf$word, datf$n, colors = pal, min.freq = 2)
    title(sport, cex.main = 1.5)
}


term_mat <- counts %>% acast(word ~ sport, value.var = "n", fill = 0) 

par(mfrow = c(1,1))
comparison.cloud(term_mat) 

wordcloud(counts[[1]]$word, counts[[1]]$n)
title("my plot")
map(counts, nrow)

