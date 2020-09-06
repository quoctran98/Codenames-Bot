library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                textInput("guess", "Guess:"),
                actionButton("submitGuess", "Submit Guess")
            ),
            actionButton("startGame", "New Game"),
            verbatimTextOutput("loading")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput("clue"),
            verbatimTextOutput("allWords"),
            fluidRow(
                verbatimTextOutput("blueWords"),
                verbatimTextOutput("redWords"),
                verbatimTextOutput("neutralWords") 
            )
        )
    )
))
