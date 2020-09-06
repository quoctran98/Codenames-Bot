library(shiny)
library(WikipediR)
library(stringr)

source("clueFunctions.R")
source("gameFunctions.R")

shinyServer(function(input, output) {
  
  observeEvent(input$startGame, {
    
    print("start game")
    
    output$loading <- renderText({
      "loading"
    })
    
    gameState <<- startGame()
    
    print(gameState$allWords[gameState$displayOrder])
    
    output$clue <- renderText({
      gameState$currentClue
    })
    
    output$allWords <- renderText({
      paste(gameState$allWords, collapse = "\n")
    })
    
    output$loading <- renderText({
      "done"
    })
    
  })
  
  observeEvent(input$submitGuess, {
    
    print(paste(input$guess))
    
    output$loading <- renderText({
      "loading"
    })
    
    gameState <<- gameLoop(gameState, input$guess)
    
    print(gameState)
    
    output$clue <- renderText({
      gameState$currentClue
    })
    
    output$allWords <- renderText({
      paste(gameState$allWords, collapse = "\n")
    })
    
    output$blueWords <- renderText({
      paste("Blue Words", 
              gameState$blueWords[gameState$blueWords %in% gameState$uncoveredWords], collapse = "\n")
    })
    
    output$redWords <- renderText({
      paste("Red Words", 
              gameState$redWords[gameState$redWords %in% gameState$uncoveredWords], collapse = "\n")
    })
    
    output$neutralWords <- renderText({
      paste("Neutral Words", 
              gameState$neutralWords[gameState$neutralWords %in% gameState$uncoveredWords], collapse = "\n")
    })
    
    output$loading <- renderText({
      "done"
    })
  })
  
})
