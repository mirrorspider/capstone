#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(data.table)
require(dplyr)
library(shinyjs)
source("./lib/backoff.R")
source("./lib/nextword.R")
source("./lib/plotting.R")
source("./lib/markov.R")

labels <- c("1st", "2nd", "3rd", "4th", "5th", "6th")


shinyServer(function(input, output) {
        ngram.data.loaded <- FALSE
        ngramList <- data_frame()
        
        
        doPrediction <- reactive({
                if(!ngram.data.loaded){
                        withProgress(message = "loading data", value = 0.5, {
                                # global variable update
                                ngramList <<- fread("./data/ngram_008.csv")
                                ngramList <<- dplyr::as_data_frame(ngramList)
                                setProgress(value = 0.9, message = "data loaded")
                                setProgress(value = 1, message = "done")
                        })
                        # global variable update
                        ngram.data.loaded <<- TRUE
                }
                s <- input$sentenceStub
                nrw <- getNextWordSlim(s, ngramList)
                
                pred <- calcProbability(nrw)

                n <- input$no.predictions
                
                n <- ifelse(length(pred$outcome) > n, n, length(pred$outcome))
                
                pred <- pred %>% head(n) %>%
                        mutate(prob = round(prob,3)) 
                pred
                
        })
        

        output$predictions <- renderTable({
                pred <- doPrediction() %>%
                        select(outcome, prob, ngram.match)
                
                a <- pred %>% t()

                n <- length(pred$outcome)
                colnames(a) <- labels[1:n]
                rownames(a) <- c("prediction", "confidence", "words matched")
                a
                
                
        }, striped = TRUE, colnames = TRUE, bordered = TRUE, digits = 3,
        rownames = TRUE)
        
        output$confidence <- renderPlot({
                #values <- doPredictions()
                pred <- doPrediction() %>%
                        select(outcome, prob, ngram.match)
                plotting <- data_frame(choice = 1:length(pred$outcome),
                                       confidence = pred$prob,
                                       label = pred$outcome)
                g <- plotConfidence(plotting)
                suppressWarnings(print(g))
        }, height = 150)
        
        observeEvent(input$showMarkov, { toggle("markov") })
        
        output$markov <- renderPlot({
                if(!input$showMarkov) return(NULL)
                show("markov")
                pred <- doPrediction()
                m <- assembleChainData(pred, ngramList)
                g <- plotMarkov(m)
                g
        })
        

})
