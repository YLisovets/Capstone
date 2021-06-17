library(shiny)
library(shinythemes)
library(markdown)
library(dplyr)
library(stringr)

source("global.R")
source('prediction.R')
source('clean_text.R')

# Define UI for application that draws a histogram
ui <- navbarPage("Next Word Predictor",
                 theme = shinytheme("readable"),
                 tabPanel("Prediction",
                          fluidPage(includeCSS("css/styles.css"),
                              sidebarLayout(
                                  sidebarPanel(
                                      textInput("text",
                                                "Enter a phrase:",
                                                value =  "",
                                                placeholder = "Enter text here"),
                                      br(),
                                      sliderInput("numPredictions", "Number of Predictions:",
                                                  value = 3, min = 1, max = 5, step = 1)
                                  ),
                                  mainPanel(
                                      h3("Prediction:"),
                                      uiOutput(outputId = "user_phrase"),
                                      br(),
                                      h3("Predicted words:"),
                                      tableOutput("pred_table")
                                  )
                              )
                          )
                 ),
                 tabPanel("About",
                          h3("About Next Word Predict"),
                          br(),
                          div(h4("Next Word Predict is a Shiny app that uses a n-gram
                            prediction algorithm to predict the next word(s)
                            based on text entered by a user."),
                              br(),
                              h4("After one or more words have been entered, the server will
                             predict the most likely next word using Stuped Backoff."),
                              br(),
                              h4("Use the slider tool to select up to five next word predictions.
                             In the main panel the top next-word prediction will be displayed
                             in bold typeface along with top predicted words and their scores
                             (relative frequencies) and used n-grams."),
                              br(),
                              h4("If the input mostly contains unintelligible content, and the
                             prediction cannot make any “useful” prediction, the application
                             will simply return a list of its highest frequency unigrams.
                             So, if you see “the” “to” “and” “a” as predictions and those are
                             unlikely to be correct, please check the spelling of the last
                             word(s) you have just typed in."),
                              br(),
                              br(),
                              h4("Reference"),
                              br(),
                              h4("“Large language models in machine translation” by Thorsten Brants
                             et al, in EMNLP/CoNLL 2007"),
                              a(target = "_blank", href="http://www.aclweb.org/anthology/D07-1090.pdf",
                                "Ref")
                          )
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    predict =  reactive({
        
        predict = prediction(input$text)
    })
    
    output$user_phrase <- renderText(
        HTML(paste(input$text, "<b>", predict()[1,1]))
            )
    
    output$pred_table <- renderTable(
        predict()[1:input$numPredictions],
        rownames = TRUE, digits = 3
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
