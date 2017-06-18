library(shiny)
library(shinythemes)

shinyUI(navbarPage("Text Prediction", inverse = T,
                   theme = shinytheme("united"),
                   tabPanel("App",
                            sidebarLayout(
                                sidebarPanel(
                                    textInput("prompt", "Prompt", placeholder = "Start typing..."),
                                    br(),
                                    strong("Parsed"),
                                    verbatimTextOutput("parsed")
                                ),
                                
                                mainPanel(
                                    plotOutput("barplot")
                                )
                              )
                            )
))
