#'This Shiny app is written by Ali Pourkhesalian
#'This the final assignment in Coursera's data science course
#'02/12/2019
#'
#'
#'_____________________________________________________________________________________________________
#'In this first section I define functions to use in the shiny app server 

#_______________________________________________________________________________________________________
# loading required packages
library(tm)
library(stringr)

#_______________________________________________________________________________________________________
# reading the N-grams to build the model
bigramsDataFrame <- readRDS("bigrams.rds")
trigramsDataFrame <- readRDS("trigrams.rds")
quadgramsDataFrame <- readRDS("fourgrams.rds")

#_______________________________________________________________________________________________________
# let's now clean the input text


textClean <- function(input) {
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- gsub("\\s+"," ",input)
  input <- tolower(input) 
}


#_______________________________________________________________________________________________________
#Defining a function to extract n last words from an input phrase

lastNWords <- function(textLastWords, nLastWords) {
          x <- as.data.frame(strsplit(textLastWords, " "))
          textLength <- nrow(x)
          if(textLength<nLastWords) nLastWords<-textLength 
          textLastWords <- paste(x[ (textLength-(nLastWords-1)) : textLength ,], collapse=" ")
          return(textLastWords)
}

#_______________________________________________________________________________________________________
# Let's now modify those shortened one-letter words such as 's or 'n etc

apostropheFixed <- function(apostropheToFixed) {
          apostropheToFixed <- gsub("^s\\s","\'s ",apostropheToFixed)
          apostropheToFixed <- gsub("^n\\s","\'n ",apostropheToFixed)
          apostropheToFixed <- gsub("^d\\s","\'d ",apostropheToFixed)
          apostropheToFixed <- gsub("^t\\s","\'t ",apostropheToFixed)
          apostropheToFixed <- gsub("^ve\\s","\'ve ",apostropheToFixed)
          apostropheToFixed <- gsub("^ll\\s","\'ll ",apostropheToFixed)
          apostropheToFixed <- gsub("^re\\s","\'re ",apostropheToFixed)
          return(apostropheToFixed)
}

#_______________________________________________________________________________________________________
#Most frequently occuring words in the unigram dataframe
maxNGram = 3
highestFrequency = 5


mostFrequentUniGram <- function(nHighestFrequentWords=highestFrequency) {
          dfUnigrams <- readRDS("unigrams.rds")
          nrows <- nrow(dfUnigrams)
          highFreqUnigramVector <- vector("list", length=25)
          for(counter in seq_len(25))
            highFreqUnigramVector[counter] <- as.character(dfUnigrams[counter,1])
          return(head(highFreqUnigramVector,nHighestFrequentWords))
}

#_______________________________________________________________________________________________________
# now let's define a fuction that searches in the ngram data frames and finds the last n words of the sentence in the them

nGramsSeek <- function(lastNWords, numberOfGrams) {
  lastNWords <- paste('^',lastNWords,' ', sep = "")
  if(numberOfGrams==3)
            subsetDataframe <- subset(quadgramsDataFrame, grepl(lastNWords, quadgramsDataFrame$Word))
  else
    if(numberOfGrams==2)
            subsetDataframe <- subset(trigramsDataFrame, grepl(lastNWords, trigramsDataFrame$Word))
  else
    if(numberOfGrams==1)
            subsetDataframe <- subset(bigramsDataFrame, grepl(lastNWords, bigramsDataFrame$Word))
  if(nrow(subsetDataframe) > 0) {
            fiveMostFreqWords <- head(apostropheFixed(gsub(lastNWords,"",subsetDataframe$Word)),highestFrequency)
            return( gsub("[[:space:]]"," ",fiveMostFreqWords) )
  }
  else{
            numberOfGrams <- numberOfGrams-1;
                    if(numberOfGrams > 0) { 
                              lastNWords <- substr(lastNWords,2,nchar(lastNWords))
                              nGramsSeek( lastNWords(lastNWords,2), numberOfGrams )
    }
            else {
                      lastNWords <- substr(lastNWords,2,nchar(lastNWords))
                      return(mostFrequentUniGram(highestFrequency))
    }
  }
}

#_______________________________________________________________________________________________________
#The predictor model

nextWordPredictorModel <- function(user_input) {
          return( nGramsSeek( lastNWords(user_input, maxNGram), n=maxNGram) )
}


#_____________________________________________________________________________________________________
#the shiny app: 

library(shiny)
#source("functions.R")

#_____________________________________________________________________________________________________
#server

server <- function(input, output, session) {
        
        output$text <- renderText({paste(input$text)})
         observe({
            textCleaned <- textClean(input$text)
            if(textCleaned != " ") 
            {
              output$textCleaned <- renderText({
                paste0("Cleansed text: [",textCleaned,"]")
              })
              textCleaned <- gsub(" \\* "," ",textCleaned)  
              predictWords <- nextWordPredictorModel(textCleaned)
              updateSelectInput(session = session, inputId = "predicts", choices = predictWords)
            }  
         })

 }

#_____________________________________________________________________________________________________
#User interface

ui <- fluidPage(
        	# Application title
          	titlePanel("Coursera Data Science Capstone Project"),
          	    
          # User interface controls1
            sidebarLayout(
        	    sidebarPanel(
        			p("Please enter a sequence of words or a part of a sentence:"),	
        			textInput(inputId="text", label = ""),
        			submitButton("Next")),
                		mainPanel(
                        		   	p(
                        		   	  selectInput("predicts","The prediction for the next word in the order of their probabilities:",choices=c(""))
                              	    
                        		   	  )
                			        )
        		
                          )
                )


shinyApp(ui = ui, server = server)


