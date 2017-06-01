## Build-a-scraper
library(stringr)
library(tidyverse)
library(magrittr)
library(rvest)

## Cust fxns
na_filler <- function(vector, reverse = F) {
    if (reverse) {
        seq <- length(vector):1
    }
    if (!reverse) {
        seq <- 1:length(vector)
    }
    for (i in seq) {
        if (!is.na(vector[i])) {
            j <- vector[i]
        }
        if (is.na(vector[i])) {
            vector[i] <- j
        }
    }
    return(vector)
}

setwd("~/future/Text/sports_sentiment/")

# documents source from : http://www.asapsports.com/
# 4 sports 
pages <- "http://www.asapsports.com/show_event.php?category=5&date=2017-5-30&title=NHL+STANLEY+CUP+FINAL%3A+PREDATORS+VS+PENGUINS"
pages <- "http://www.asapsports.com/show_event.php?category=11&date=2017-5-25&title=NBA+EASTERN+CONFERENCE+FINALS%3A+CELTICS+VS+CAVALIERS"
pages <- "http://www.asapsports.com/show_event.php?category=4&date=2017-5-28&title=BMW+PGA+CHAMPIONSHIP"
pages <- "http://www.asapsports.com/show_event.php?category=3&date=2017-5-28&title=MONSTER+ENERGY+NASCAR+CUP+SERIES%3A+COCA-COLA+600"

asap_scraper <- function(link_page) {
    pages <- read_html(link_page)
    
    pages %<>% html_nodes("td a") %>% # selector gadget
        html_attr("href") %>% # get links out
        Filter(function(x) { grepl("id=\\d*", x) }, .) # filter for interviews
    
    # web-scraping is always gross
    sport <- map_df(pages, ~ read_html(.) %>%
                        html_nodes("td") %>%
                        html_text() %>% 
                        .[14] %>%
                        gsub(".*\n\t\t", "", .) %>%
                        str_split("\n") %>%
                        unlist() %>%
                        tibble(text = .) %>%
                        mutate(text = trimws(text),
                               text = gsub("Q\\..*", "", text),
                               text = gsub("FastScripts.*", "", text),
                               speaker = gsub("(^.*): .*", "\\1", text),
                               text = gsub("^.*: ", "", text),
                               speaker = ifelse(speaker == text, NA, speaker),
                               text = ifelse(str_count(speaker, " ") > 3, paste(speaker, text), text),
                               speaker = ifelse(str_count(speaker, " ") > 3, NA, speaker)) %>%
                        .[-1,] %>%
                        filter(!is.na(text)))
    
    sport$speaker %<>% na_filler()   
    
    return(sport)
    
}

asap_scraper(pages)


