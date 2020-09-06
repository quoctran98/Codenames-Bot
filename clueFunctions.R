#this is basically a wrapper for page_backlinks
getLinks <- function(page, recursionLevel = 0, maxDisambRecursion = 3) {
  linkList <- page_backlinks(language = "en", 
                             project = "wikipedia", 
                             page = page, 
                             namespaces = 0, #only main articles
                             limit = 500)$query$backlinks #the limit is actually 500 from wikipedia -- the library is wrong
  linkTitles <- c()
  
  #page_backlinks returns a list -- this is just to resolve that list into a vector of page titles
  for (link in linkList) {
    if (str_detect(link$title, "(disambiguation)") && recursionLevel < maxDisambRecursion) {
      #resolve disambugation pages for more primary clues -- up to `maxDisambRecursion` times
      linkTitles <- c(linkTitles, getLinks(link$title, recursionLevel = recursionLevel + 1))
    } else {
      linkTitles <- c(linkTitles, link$title)
    }
  }
  
  #this returns everthing that ISN'T duplicated -- I DON'T KNOW WHY? shouldn't I want just the unique ones?
  #linkTitles <- linkTitles[!(linkTitles %in% linkTitles[duplicated(linkTitles)])] 
  linkTitles <- unique(linkTitles)
  return(linkTitles)
}

#using getLinks of getLinks (etc...) to force a clue between two words
#very slow, so limit max steps
forceClue <- function(word1, word2, maxSteps = 3) {
  #initial links for each word
  w1Tree <- getLinks(word1)
  w2Tree <- getLinks(word2)
  
  #gets links of links for one tree first, searches, then if it doesn't find anything, gets links of links for the other tree, etc.
  currentTree <- sample(c("w1", "w2"), 1) #there's no reason one tree should start first
  
  for (i in 1:maxSteps) {
    #finds matches between the current trees first
    treeMatch <- match(w1Tree, w2Tree)
    
    #match() will return all NA's if no matches are found
    if (F %in% is.na(treeMatch)) {
      return(w2Tree[treeMatch[!is.na(treeMatch)]]) #returns all matches and exits function
      
      #if no match then expand the tree that didn't get expanded last time
    } else {
      if (currentTree == "w1") {
        #tree expansion is literally just adding all the links for each link into the same vector
        for (link in w1Tree) {
          w1Tree <- c(w1Tree, getLinks(link))
        }
        currentTree <- "w2"
      } else {
        for (link in w2Tree) {
          w2Tree <- c(w2Tree, getLinks(link))
        }
        currentTree <- "w1"
      }
    }
  }
  
  #if no other matches are found in `maxSteps` steps, it returns NA
  #(probably should change since nothing downstream can handle this)
  return(NA)
}

findClues <- function(words, exclude = NA) {
  
  #primary bad backlinks to avoid
  badClues <- c()
  if (FALSE %in% is.na(exclude)){
    for (badWord in exclude) {
      badClues <- c(badClues, getLinks(badWord))
    }
  }
  
  #get the primary backlinks for each word
  wordLinks <- list()
  for (w in words) {
    allLinks <- getLinks(w)
    filteredLinks <- allLinks[!(allLinks %in% badClues)] #filter out clues to bad word
    wordLinks[[length(wordLinks) + 1]] <- list(word = w, links = filteredLinks, clues = NULL)
  }
  
  #get counts of how many times the primary backlinks appear overall (>1 is a clue)
  allLinks <- unlist(lapply(wordLinks, function(list) {return(list$links)}))
  possibleClues <- table(allLinks[duplicated(allLinks)]) + 1
  
  #adds the clues and the linked words to the main list
  for (clue in names(possibleClues)) {
    hasClueLog <- sapply(wordLinks, 
                         function(list) {
                           return(clue %in% list$links)
                         })
    hasClueWord <- c() 
    for (i in 1:length(wordLinks)) { #for all the words that the clue refers to
      if (hasClueLog[i]) {
        hasClueWord <- c(hasClueWord, wordLinks[[i]]$word)
      }
    }
    for (i in 1:length(wordLinks)) { #add those words to main list
      if (hasClueLog[i]) {
        wordLinks[[i]][["clues"]][[clue]] <- hasClueWord[hasClueWord != wordLinks[[i]]$word]
      }
    }
  }
  
  return(wordLinks)
}

#EXCLUDE DOESN'T WORK FOR FORCE CLUE OR GET LINKS
giveClue <- function (words, exclude = NA) {
  #only one word left -- choose a random backlink
  if (length(words) == 1) { 
    return(c(sample(getLinks(words), 1), 1))
  } else if (length(words) == 2) { #only two words left -- force a clue
    return(c(sample(forceClue(words[1], words[2]), 1), 2))
  }
  
  #go through all clues now
  wordList <- findClues(words, exclude = exclude)
  
  #no primary clues -- force a link now
  if (is.null(unlist(lapply(wordList, function(list) {return(list$clues)})))) { 
    return(c(sample(forceClue(wordList[[1]]$word, wordList[[1]]$word), 1), 2))
  }
  
  #choose one of the top clues
  allCluesRaw <- lapply(wordList, function(list) {return(list$clues)})
  allClues <- list()
  for (clueList in allCluesRaw) {
    allClues <- append(allClues, clueList)
  }
  bestClue <- c()
  bestClueNum <- 0
  for (clueIndex in 1:length(allClues)) { #go through every clue
    clueNum <- length(unlist(allClues[clueIndex]))
    clue <- names(allClues[clueIndex])
    if (clueNum > bestClueNum) { #if greater completely replace list of clues
      bestClue <- clue
      bestClueNum <- clueNum
    } else if (clueNum == bestClueNum) { #if equal add to list of clues
      bestClue <- c(bestClue, clue)
    }
  }
  bestClue <- unique(bestClue) #I don't think this is actually necessary
  bestClue <- sample(bestClue, 1)
  return(c(bestClue, bestClueNum + 1)) #add one to include self-referral for word
}