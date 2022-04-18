
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rgl)


trainFiles <- list.files("TrainingFiles")
corpusData <- list.files("CorpusData")



navbarPage("Classifier", id="nav",
           tags$head(
             # Include our custom CSS
             includeCSS("styles.css"),
             includeScript("gotable.js"),
           ),
           tabPanel("Home",
                    h1("Abui Verb Clustering")            
                    # Sidebar with a slider input for number of bins
                    , p("This is a simple tool to visualize Abui verbs (using Principal Component Analysis) and their classification using K-means classifier. Different clusters have different colours - size corresponds to cluster size. The analysis of principal components can benefit the linguistic description by indicating which
morphological properties are most significant for the clustering and should be documented with priority.")
                    ,p("There are two datasets. The so-caller Currated data set and Olga's data set. Mostly, they contain the same verbs. Nevertheless, their representations differ. The data from currated data set is classified using K-means classifier. 
                       Then, a Naive Bayes classifier is trained on this classification. Using this Naive classifier, the data from Olga's data set were classified using Naive Bayes classified.")
                    ,p("The data from currated set are very unbalanced. It contains 351 verbs but 86 unique representations. More than 60% of verbs correspond just to two different representations.")  
                    
           ),
           tabPanel("Cluster Visualisation", 
                    # Application title
                    fluidRow(
                      column(6
                             , selectInput("learningSet", "Learning set", 
                                           choices = trainFiles)
                             
                             
                      ) ,
                      column(6, 
                             selectInput("corpusData", "Corpus data", 
                                         choices = corpusData)
                             
                      )
                    ),
                    fluidRow(
                      column(12
                             , p(
                               sliderInput("nCluster",
                                           "Number of clusters:",
                                           min = 1,
                                           max = 86,
                                           value = 25)
                             )
                             
                             # Show a plot of the generated distribution
                             
                             , p("Scroll to zoom the 3D picture, use mouse to rotate it.")
                             
                             , span(rglwidgetOutput("myWebGL", width = "800", height = 800))
                             
                             
                             , plotOutput("plotDistances")
                      )
                    )
           ),
           tabPanel("Cluster membership"
                    , p("If there are more verbs in the centre of the cluster,
                        the first in the alphabetical ordering is selected as cluster representative")
                    , DT::dataTableOutput("tblCluster")
                    
           ),
           tabPanel("Data",
                    fluidRow(
                      column(4 
                             , selectInput("dataFile", "Data", 
                                           choices = c("Learning data", "Corpus")))
                      , column(4
                               , textOutput("txtWord"))
                      ,column(4
                              , actionButton("btnClear", "Clear selection"))
                    )
                    , DT::dataTableOutput("tblData")),
           tabPanel("Bayesian network analysis")
           
)
