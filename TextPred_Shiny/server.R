library(tidyverse)
library(magrittr)
library(stringr)
library(viridis)
library(forcats)
library(shiny)

# load lookup RDS from ngrams.R
ngrams <- readRDS("ngrams.RDS") # large list 49.3 Mb

# load helper functions
best_guessN <- function(st) {
    
    st %<>% tolower() %>% str_extract_all("[a-z]+'?[a-z]*") %>% unlist()
    num_words <- length(st)
    
    if (num_words > 4) {
        st <- st[-(1:(num_words - 4))]
        num_words <- 4
    }
    
    st_guess <- paste(st, collapse = " ")
    
    guess <- ngrams[[num_words]] %>% filter(gram == st_guess) %>%
        ungroup() %>%
        mutate(prop = n / total_next,
               coverage = cumsum(prop)) %>%
        select(gram, next_word, prop) %>%
        mutate(next_word = fct_inorder(next_word),
               next_word = factor(next_word, levels = rev(levels(next_word))))
    
    while (nrow(guess) < 1) {
        num_words <- num_words - 1
        st <- st[-1]
        st_guess <- paste(st, collapse = " ")
        guess <- ngrams[[num_words]] %>% filter(gram == st_guess) %>%
            ungroup() %>%
            mutate(prop = n / total_next) %>%
            select(gram, next_word, prop) %>%
            mutate(next_word = fct_inorder(next_word),
                   next_word = factor(next_word, levels = rev(levels(next_word))))
    }
    return(list(guess, st))
}

# server
shinyServer(function(input, output) {
    
    datf <- reactive({ 
        req(input$prompt)
        best_guessN(input$prompt)
        })
    limits <- reactive({
        c(0, .05*(ceiling(datf()[[1]]$prop[1] / .05)))
    })
    
    output$parsed <- renderText({ datf()[[2]] })
    
    output$test <- renderPrint({ datf() })
    
    output$barplot <- renderPlot({
        ggplot(datf()[[1]], aes(next_word, prop, fill = prop)) +
            geom_col() +
            geom_label(aes(y = prop, color = prop,
                           label = paste0(next_word,"\n",round(prop*100,0), "%")),
                       fill = "#f5f5f5", size = 10, label.size = 2, label.padding = unit(.5, "lines"),
                       hjust = .5, vjust = .5) +
            coord_flip() +
            scale_y_continuous(labels = scales::percent, limits = limits()) +
            scale_fill_gradient(low = "#772953", high = "#e95420") +
            scale_color_gradient(low = "#772953", high = "#e95420") +
            labs(title = "Corpus Frequency",
                 x = NULL, y = NULL) +
            theme(legend.position = "none",
                  panel.background = element_rect(fill = "#f5f5f5"),
                  panel.border = element_rect(color = "#e1e1e1", fill = NA, size = 2),
                  panel.grid.major = element_line(color = "#e1e1e1"),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text.x = element_text(size = rel(2), angle = 0, color = "black"),
                  axis.text.y = element_blank(),
                  axis.title.x = element_text(size = rel(2.5), angle = 0, color = "black"),
                  axis.ticks.y = element_blank(),
                  plot.title = element_text(size = rel(2.5)))
    })
  
})
