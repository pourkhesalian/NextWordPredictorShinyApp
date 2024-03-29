The "Next Word Predictor" Shiny App
=============================================================================
author: Ali Pourkhesalian
date: 15/12/2019
transition: fade
transition-speed: fast


Summary
========================================================
This document presents description on the Coursera Data Science Specialist final assingment named the Capstone. The objective is to put together a shiny app whose job is to predict the next word in a sentence or a phrase.  
The app uses the data from Coursera Swiftkey to build a model. The original dataset is very large and thus sampling methods were implemented to reduce the size of the dataset in order to reduce computational resources of the machine.   
The dataset included some text form blogs, twitter and news in English. 
Initially, some exloratoy analyses were carried out on the dataset. Next, the dataset were cleaned, meaning all numbers, punctuations, profanity words were remved and then all the text was turned into lower case. 



How to use the app?
========================================================

Type in the phrase or sentence in the designated field and press "Enter" or "Next" and the app will predict the next word(s) based on the predictive model.

The algorythm
========================================================

There app first load four datasets containing 1-, 2-, 3-, and 4-gram models which are put to gether previously.  
Then, the app will go through the user input and performs some cleaning procedure on the input text.  
The model uses Stupid Backoff algorythm to predict the next word.
The app will search the quad-gram dataset and tries to find the next most probable words. If the search is not successfull it goes through the next n-gram model down and if it could not find the next most commonly used word(s), it returns the most commonly used word in the modelled corpus.

Check it out! 
========================================================


Please check out the shiny app by following the below link:  
<https://pourkhesalian.shinyapps.io/NextWordPredictorShinyApp/>  

Also, all the files and codes related to this project can be found in the GitHub repository below:  

<https://github.com/pourkhesalian/NextWordPredictorShinyApp>
