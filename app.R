library(shiny)
library(tidyverse)
library(data.table)

# Load Data once when app starts
bigrams <- readRDS("app_data/bigrams.rds")
trigrams <- readRDS("app_data/trigrams.rds")
quadgrams <- readRDS("app_data/quadgrams.rds")

# --- The Prediction Function ---
predict_next_word <- function(input_text) {
  # Clean input
  input_text <- tolower(input_text)
  input_text <- str_replace_all(input_text, "[^a-z\\s]", "")
  words <- strsplit(input_text, " ")[[1]]
  n <- length(words)
  
  # Logic: Check Quadgram -> Trigram -> Bigram
  
  # 1. Check Quadgram (needs last 3 words)
  if (n >= 3) {
    w1 <- words[n-2]; w2 <- words[n-1]; w3 <- words[n]
    match <- quadgrams %>% filter(word1 == w1, word2 == w2, word3 == w3) %>% head(1)
    if (nrow(match) > 0) return(match$word4)
  }
  
  # 2. Check Trigram (needs last 2 words)
  if (n >= 2) {
    w1 <- words[n-1]; w2 <- words[n]
    match <- trigrams %>% filter(word1 == w1, word2 == w2) %>% head(1)
    if (nrow(match) > 0) return(match$word3)
  }
  
  # 3. Check Bigram (needs last 1 word)
  if (n >= 1) {
    w1 <- words[n]
    match <- bigrams %>% filter(word1 == w1) %>% head(1)
    if (nrow(match) > 0) return(match$word2)
  }
  
  # 4. Default
  return("the")
}

# --- Shiny UI ---
ui <- fluidPage(
  titlePanel("Next Word Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      p("1. Type a phrase in the box below."),
      p("2. Press the 'Predict' button."),
      p("3. The model will guess the next word."),
      br(),
      textInput("user_input", "Enter Text Here:", value = ""),
      actionButton("predict_btn", "Predict Next Word")
    ),
    
    mainPanel(
      h3("Predicted Word:"),
      verbatimTextOutput("prediction_output"),
      br(),
      h4("You entered:"),
      textOutput("original_text")
    )
  )
)

# --- Shiny Server ---
server <- function(input, output) {
  
  # Use eventReactive to wait for button press
  prediction <- eventReactive(input$predict_btn, {
    req(input$user_input) # Ensure input exists
    predict_next_word(input$user_input)
  })
  
  output$prediction_output <- renderText({
    prediction()
  })
  
  output$original_text <- renderText({
    input$user_input
  })
}

shinyApp(ui = ui, server = server)