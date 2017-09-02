library(tidyverse)
library(magrittr)
library(stringr)
library(viridis)
library(forcats)
library(shiny)

# load lookup RDS from ngrams.R
ngrams_tib <- readRDS("top3_tib.RDS") # large list 49.3 Mb

best_guess <- function(st) {
    
    top_overall <- count(ngrams_tib, next_word, wt = n, sort = T) %>%
        mutate(gram = "???",
               coverage = (nn / sum(nn))) %>%
        slice(1:3) %>%
        rename(n = nn)
    
    st %<>% tolower() %>% str_extract_all("[a-z]+'?[a-z]*") %>% unlist()
    
    n <- 4 # ngrams become over-fitted as n increases
    st %<>% tail(n)
    
    guess <- filter(ngrams_tib, gram ==  paste(st, collapse = " "))
    
    while (nrow(guess) < 1) {
        if (n == 1) {
            guess <- top_overall
        }
        else {
            n %<>% -1
            st %<>% tail(n)
            guess <- filter(ngrams_tib, gram == paste(st, collapse = " "))
        }
    }
    # back fill with top overall words if less than 3 options
    if (nrow(guess) < 3) {
        guess %<>% bind_rows(top_overall[1:(3 - nrow(guess)), ])
        guess %<>% group_by(next_word) %>%
            summarise(coverage = mean(coverage))
    }
    return(guess)
}


# server
shinyServer(function(input, output) {
    
    datf <- reactive({ 
        req(input$prompt)
        best_guess(input$prompt) %>%
            mutate(next_word = fct_inorder(next_word),
                   next_word = fct_rev(next_word))
        })
    
    limits <- reactive({
        c(0, 1.1*(datf()$coverage[1]))
    })
    
    output$parsed <- renderText({ datf()$gram[1] })
    
    output$test <- renderPrint({ datf() })
    
    output$barplot <- renderPlot({
        ggplot(datf(), aes(next_word, coverage, fill = coverage)) +
            geom_col() +
            geom_label(aes(y = coverage, color = coverage,
                           label = paste0(next_word,"\n",
                                          round(coverage*100,0), "%")),
                       fill = "#f5f5f5", size = 10, label.size = 2,
                       label.padding = unit(.5, "lines"),
                       hjust = .5, vjust = .5) +
            coord_flip() +
            scale_y_continuous(labels = scales::percent, limits = limits()) +
            scale_fill_gradient(low = "#772953", high = "#e95420") +
            scale_color_gradient(low = "#772953", high = "#e95420") +
            labs(title = "Corpus Frequency",
                 x = NULL, y = NULL) +
            theme(legend.position = "none",
                  panel.background = element_rect(fill = "#f5f5f5"),
                  panel.border = element_rect(color = "#e1e1e1", fill = NA,
                                              size = 2),
                  panel.grid.major = element_line(color = "#e1e1e1"),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text.x = element_text(size = rel(2), angle = 0,
                                             color = "black"),
                  axis.text.y = element_blank(),
                  axis.title.x = element_text(size = rel(2.5), angle = 0,
                                              color = "black"),
                  axis.ticks.y = element_blank(),
                  plot.title = element_text(size = rel(2.5)))
    })
  
})
