

library(tidytext)
library(tidyverse)
library(magrittr)

setwd("~/future/Text/")

# documents source from : http://www.asapsports.com/
# used most recent/first document based on date and page order with player quotes (not coaches of execs)
# possibly expand from this to more observations

nhl <- readLines("mark_streit_20170522")
pga <- readLines("chris_wood_20170523")
indy <- readLines("indy_car_20170522")
nba <- readLines("jr_smith_20170522")

athletes <- list(nhl, pga, indy, nba) %>%
    set_names(c("nhl", "pga", "indy", "nba"))

## Clean ----------------------------------------------------------------------

# tidy first
tidy <- map(athletes, ~tibble(text = .))

# clean header lines
map(tidy, ~head(., 10))
# consistent formatting due to source
tidy %<>% map(~ .[-(1:8), ])

# drop questions
tidy %<>% map(~ filter(., !grepl("^Q|^THE|^MICHAEL GIBBONS", text)))
# drop answerer id prefix
tidy %<>% map(~ mutate(., text = gsub("^.*:", "", text)))

# drop empty rows
tidy %<>% map_df(~ filter(., grepl(" ", text)))
