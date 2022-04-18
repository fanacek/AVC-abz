
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rgl)
library(e1071)
library(plyr)
library(data.table)

# converts factor variable into binary
factor2bin <- function(df) {
  for (name in names(df)) {
    f = factor(df[,name])
    l = levels(f)
    if(length(l) > 2) {
      newcols = sapply(l, function(x) as.integer(x==df[,name]))
      # new columns name
      colnames(newcols) = paste(name,l,sep=".")
      df = cbind(df[,setdiff(names(df), name)], newcols)
    }
  } 
  df
}


# convert columns to binary
#if (all(colnames(cs) == colnames(olga))) {
#  res = factor2bin(rbind(cs,olga)) 
#  cs = res[1:nrow(cs),]
#  olga = res[(nrow(cs)+1):nrow(res),]
#  rownames(olga) <- gsub("1","", rownames(olga))
#} else {
#  error("Columns are different.")
#}

# pca
#cs.pca  <- prcomp(cs, retx = TRUE, scale. = TRUE)

# Determine number of clusters
#



shinyServer(function(input, output, session) {
  
  open3d()
  data = reactiveValues(
    word = "",
    tblClusters = data.frame(),
    training = data.frame(),
    corpus = data.frame(),
    clusters = list(), # K-means clustering method result
    scene1 = scene3d(),
    class = list() # NaiveBayes class result
  )
  rgl.close()
  
  
  # Loading  Training files ####
  observeEvent(input$learningSet, {
    training <- read.csv2(file=paste("TrainingFiles/",input$learningSet, sep=""))
    rownames(training) <- training[,"Verb"]
    # remove all columns named Notes, ID, Class, Aspectual.Class
    training <- training[,setdiff(colnames(training), c("Notes", "en", "abui","Class", "Aspectual.Class", "ID", "Notes", "Verb"))]
    
    
    training <- training
    
    # add columns loc/ben and rec/goal as in olga file
    training[,"loc/ben"] <- pmax(training[,"loc"], training[,"ben"])
    training[,"rec/goal"] <- pmax(training[,"rec"], training[,"goal"])
    data$training <- training
    
    
  })
  
  # Loading  Corpus Data ####
  observeEvent(input$corpusData, {
    corpus <- read.csv2(file=paste("CorpusData/",input$corpusData, sep=""))
    rownames(corpus) <- corpus[,"Verb"]
    # remove all columns named Notes, ID, Class, Aspectual.Class
    corpus <- corpus[,setdiff(colnames(corpus), c("Notes", "En", "Abui","Class", "Aspectual.Class", "ID", "Notes", "Verb"))]
    
    
    
    # add columns loc/ben and rec/goal as in olga file
    if(("loc" %in% colnames(corpus)) & ("ben" %in% colnames(corpus))) {
      corpus[,"loc/ben"] <- pmax(corpus[,"loc"], corpus[,"ben"])
    }
    if(("rec" %in% colnames(corpus)) & ("goal" %in% colnames(corpus))) {
      corpus[,"rec/goal"] <- pmax(corpus[,"rec"], corpus[,"goal"])
    }
    data$corpus <- corpus
    
    
  })
  
  # Number of Clusters  - main routine####
  
  observeEvent({input$nCluster
    # data$training
    # data$cluster
  }, {
    data$clusters <- kmeans(data$training, centers = input$nCluster)
    
    cs.model = naiveBayes(data$training[, intersect(colnames(data$training), colnames(data$corpus))], 
                          as.factor(data$clusters$cluster))
    data$class = predict(cs.model, data$corpus)
    
    data$clusters$representat = c();
    for (i in 1:input$nCluster){
      # rowsum <- rowSums(abs(cs[which(data$cs.clusters$cluster==i),] - data$cs.clusters$centers[i,]))
      rowsum <- rowSums(abs(t(t(data$training[which(data$clusters$cluster==i),]) - data$clusters$centers[i,])))
      # cat("min:",min(rowsum), "\n")
      
      # cat(rowsum, "\n")
      data$clusters$representant[i] <- sort(names(which(rowsum == min(rowsum))), decreasing = F)[1]
      
      data$clusters$representant[i] <- sort(names(which(rowsum == min(rowsum))), decreasing = F)[1]
      
      cat(sort(names(which.min(rowsum)), decreasing = F), "\n")
    }
    
    ## Classes table ####
    nClusters <- length(data$clusters$representant)
    data$tblClusters <- data.frame(representative = rep("", nClusters ), learning.set =  rep("", nClusters ), corpus =  rep("", nClusters ))
    #TODO:   # for each row sepparately 
    # if there is a match - bold in corpus
    # if there is no match - italic in training
    for(i in 1:nClusters) {
       data$tblClusters$representative[i] <- data$clusters$representant[i]
       trainingI <- rownames(data$training)[data$clusters$cluster == i]
       trainingMatch <- trainingI 
    #   which(data)
     }
    
    # links to words
    
    trainingLinks <- sapply(as.list(rownames(data$training)), function(x) {
      paste('<a class="go-table" alt="Show detail" href="" data-type="word"', ' data-data="', x, '">',x,'</a>', sep="")})

    corpusLinks <- sapply(as.list(rownames(data$corpus)), function(x) {
      paste('<a class="go-table" alt="Show detail" href="" data-type="word"', ' data-data="', x, '">',x,'</a>', sep="")})
    
    data$tblClusters <- data.frame(
      # for each row sepparately 
      # if there is a match - bold in corpus
      # if there is no match - italic in training
      representative = data$clusters$representant, #usporadat
      
      "learning set" = sapply(1:input$nCluster, function(i) {
        links <- trainingLinks[data$clusters$cluster == i]
        wordsHere = rownames(data$training)[data$clusters$cluster == i] %in% rownames(data$corpus)[data$class == i]
        wordsElse = rownames(data$training)[data$clusters$cluster == i] %in% rownames(data$corpus)
        wordsElse = wordsElse & !wordsHere
        
        links[wordsHere] <- paste("<b>", links[wordsHere], "</b>")
        links[wordsElse] <- paste("<i>", links[wordsElse], "</i>")
        
        paste(sort(links), collapse = ", ")}), 
      corpus = sapply(1:input$nCluster, function(i) {
        links <- corpusLinks[data$class == i]
        wordsHere = rownames(data$corpus)[data$class == i] %in% rownames(data$training)[data$clusters$cluster == i] 
        wordsElse = rownames(data$corpus)[data$class == i] %in% rownames(data$training)
        wordsElse = wordsElse & !wordsHere
        
        links[wordsHere] <- paste("<b>", links[wordsHere], "</b>")
        links[wordsElse] <- paste("<i>", links[wordsElse], "</i>")
        
        paste(sort(links), collapse = ", ")}),
      row.names = 1:input$nCluster
    )
    
    
    open3d()
    ## PCA ####
    cs.pca  <- prcomp(data$training, retx = TRUE, scale. = TRUE)
    coords = cbind(cs.pca$x[,1:3],  cluster = data$clusters$cluster);
    coords = count(coords, vars = 1:4)
    
    #plot3d(jitter(cs.pca$x[,1:3], factor = 100), col = rainbow(input$nCluster)[data$cs.clusters$cluster], type="s", alpha = 1, size=1)
    clear3d()
    
    spheres3d(coords[,1:3], col =  rainbow(input$nCluster)[coords[,4]], radius = (coords$freq^(1/3))/4, add=T, alpha = 1)
    axes3d(c('x', 'y', 'z'), labels = FALSE)
    text3d(coords[,1:3]-.3, texts = as.character(coords[,4]), add = T)
    
    #spheres3d(x,y,z, col = rainbow(input$nCluster)[data$cs.clusters$cluster], radius = 1, alpha = .8, add=T)
    
    data$scene1 <- scene3d()
    rgl.close()   
  })
  
  
  
  output$myWebGL <- renderRglwidget({
    rglwidget(data$scene1)
  })
  output$tblCluster <- DT::renderDataTable({
    
    df <- data$tblClusters
    action <- DT::dataTableAjax(session, df, outputId = "loanstable")
    return(DT::datatable(df, colnames = colnames(df),
                         #                             filter = 'top',
                         options = list(ajax = list(url = action),  pageLength = 100), escape = FALSE))
    
  })
  
  
  observeEvent(input$btnClear, {
    data$word <- ""
  })
  # tblData ####
  output$tblData <- DT::renderDataTable({
    
    if(data$word != ""){
    word <- data$word
    #word <- "faaling_listen"
    #word <- strsplit(data$word, split =  "_")
    
    df1 <- data$training[word == rownames(data$training),]
    if(nrow(df1)) rownames(df1) <- "trainig set"
    df2 <- data$corpus[word == rownames(data$corpus),]
    if(nrow(df2)) rownames(df2) <- "corpus"
    df1[setdiff(names(df2), names(df1))] <- NA
    df2[setdiff(names(df1), names(df2))] <- NA
    df <- rbind(df1,df2)
    } else {
      if(input$dataFile == "Corpus") {
        df <- data$corpus
      } else {
        df <- data$training
      }
    }
    
    
    action <- DT::dataTableAjax(session, df, outputId = "tblData")
    return(DT::datatable(df, colnames = colnames(df),
                         #                             filter = 'top',
                         options = list(ajax = list(url = action),  pageLength = 100), escape = FALSE))
    
  })
  
  output$txtWord <- renderText({
    if(data$word != "")
      paste("Selected word:", data$word)
  })
  
  
  
  output$pom <- renderPrint({
    print(packageVersion("rgl"))
    
    print(data$olga.class)
    cat(sapply(1:input$nCluster, function(i) paste(sort(rownames(olga)[data$olga.class == i]), collapse = ", ")))
    
  })
  
  output$plotDistances <- renderPlot({
    # number of unique rows in training dataset
    nClustMax <- sum(!duplicated(data$training))
    wss <- (nrow(data$training)-1)*sum(apply(data$training,2,var))
    for (i in 2:nClustMax) wss[i] <- sum(kmeans(data$training,
                                                centers=i)$withinss)
    plot(3:nClustMax, wss[3:nClustMax], type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares") 
    grid()
  })
  
  # GOTO Events ####
  observe({
    if (is.null(input$goto))
      return()
    cat(names(input$goto))
    print(input$goto)
    if(input$goto$type == "word") {
      isolate({
        # do something....
        word <- input$goto$data
        data$word <- word
        updateNavbarPage(session, "nav", selected = "Data")
        
        })
    }
    
  })  
  
  
})
