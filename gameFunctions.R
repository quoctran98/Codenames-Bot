startGame <- function (wordList = "words/classic.txt") {
  
  #start these outside because apparently you can't refer to a list being built, which makes sense
  allWords <-  sample(read.table(wordList)[,1], 25)
  blueWords <-  allWords[1:9]
  redWords <-  allWords[10:18]
  neutralWords <- allWords[19:24]
  assassinWord <- allWords[25]
  
gameState <- list(
    turnNum = 1, #blue takes odd turns and red takes even ones -- increments AFTER play
    
    allWords = allWords,
    displayOrder = sample(1:25, 25),
    uncoveredWords = c(), #char array -- don't use indices
    
    blueWords = blueWords,
    redWords = redWords,
    neutralWords = neutralWords,
    assassinWord = assassinWord,
    
    currentClue = giveClue(blueWords, exclude = c(redWords, assassinWord)),
    needClue = F,
    
    winner = NA
  )
  
  return(gameState)
}

gameLoop <- function (gameState, wordChosen) {
  
  gameState <- gameState
  blueTurn <- gameState$turnNum %% 2
  gameState$uncoveredWords <- c(gameState$uncoveredWords, wordChosen)
  
  blueChosen <- F
  redChosen <- F
  neutralChosen <- F
  
  if (wordChosen %in% gameState$blueWords) {
    print("blue word")
    blueChosen <- T
  } else if (wordChosen %in% gameState$redWords) {
    print("red word")
    redChosen <- T
  } else if (wordChosen %in% gameState$neutralWords) {
    print("neutral word")
    neutralChosen <- T
  } else {
    print("uh oh")
  }
  
  
  #check for round ending (+1 to turnNumber will trigger)
  #if the right color word is chosen, nothing will be triggered (except maybe game win)
  #NO HANDLING FOR MAX NUM OF CLUES GUESSED YET
  if (blueChosen && !blueTurn) {
    print("blue chosen on red turn")
    gameState$turnNum <- gameState$turnNum + 1
    blueTurn <- gameState$turnNum %% 2
    gameState$needClue <- T
    
  } else if (redChosen && blueTurn) {
    print("red chosen on blue turn")
    gameState$turnNum <- gameState$turnNum + 1
    blueTurn <- gameState$turnNum %% 2
    gameState$needClue <- T
    
  } else if (neutralChosen) {
    print("neutral chosen")
    gameState$turnNum <- gameState$turnNum + 1
    blueTurn <- gameState$turnNum %% 2
    gameState$needClue <- T
    
  } else if (wordChosen == gameState$assassinWord) {
    #end game immediately
    if (blueTurn) {
      gameState$winner <- "red"
      return(gameState)
    } else {
      gameState$winner <- "blue"
      return(gameState)
    }
  }
  
  #check for a winnter
  if (!(F %in% (gameState$blueWords %in% gameState$uncoveredWords))) {
    gameState$winner <- "blue"
    return(gameState)
  } else if (!(F %in% (gameState$redWords %in% gameState$uncoveredWords))) {
    gameState$winner <- "red"
    return(gameState)
  }
  
  #handle clues AFTER round and game end logic, so the clue will be for whoever's turn is NEXT
  if (gameState$needClue) {
    blueRemaining <- gameState$blueWords[!gameState$blueWords %in% gameState$uncoveredWords]
    redRemaining <- gameState$redWords[!gameState$redWords %in% gameState$uncoveredWords]
    if (blueTurn) {
      gameState$currentClue <- giveClue(blueRemaining, exclude = c(redRemaining, gameState$assassinWord))
    } else {
      gameState$currentClue <- giveClue(redRemaining, exclude = c(blueRemaining, gameState$assassinWord))
    }
    gameState$needClue <- F
  }
  
  return(gameState)
}
