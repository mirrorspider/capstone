#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        useShinyjs(),
  # Application title
  titlePanel("Next Word Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("sentenceStub",
                 "Type your sentence to predict the next word:",
                 "predict the"),
       sliderInput("no.predictions",
                   "Number of suggestions:",
                   min = 1,
                   max = 6,
                   value = 6),
       checkboxInput("showMarkov", 
                     HTML(paste0("Display a visualisation of the Markov chain?",
                        "<br/>",
                        "(turn off for faster prediction performance)")))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            plotOutput("markov"),
            tableOutput("predictions"),
            plotOutput("confidence")
    )
  )
))
