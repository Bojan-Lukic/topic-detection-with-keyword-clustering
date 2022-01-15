rm(list=ls())

library(devtools)
library(httr)
library(tm)
library(word2vec)
library(wordVectors)
library(stringr)
library(readtext)
library(sjmisc)
library(stopwords)
library(SemNetCleaner)
library(corpus)
library(devtools)
library(httr)
library(tm)
library(factoextra)
library(ggplot2)
library(caret)


#Function that creates a model from a corpus text file mapping different parameters for each word in the text.
word2vec <- function(fileName){
  
  if(grepl('.txt', fileName, fixed = T)){
    
    #Convert test.txt to test.bin.
    binaryFileName <- gsub('.txt', '.bin', fileName, fixed = T)
  }
  
  else{
    
    binaryFileName <- paste0(fileName, '.bin')
  }
  
  #Train word2vec model.
  if(!file.exists(binaryFileName)){
    
    #Lowercase and setup ngrams.
    prepFileName <- 'temp.prep'
    prep_word2vec(origin = fileName, destination = prepFileName, lowercase = T)#, bundle_ngrams = 1)
    
    #Train word2vec model.
    model <- train_word2vec(prepFileName, binaryFileName, vectors = 200, threads = 4, window = 12, iter = 5, min_count = 1, force = T)#, negative_samples = 0)
    
    #Cleanup.
    unlink(prepFileName)
  } 
  
  else{
    
    model <- read.vectors(binaryFileName)
  }
  
  model
}

#Function that plots the decision boundaries for a model
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[, 1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  #Create grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  #Guess how to get class labels from predict
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load previously built datasets
load("TidyText.RData")
load("MatrixwordsSorted.RData")
#head(matrix)
#Create matrix which will store the top n words with highest combined similarities
store <- matrix(0, nrow = 8, ncol = length(matrix[, 1]))


for(i in 1:length(matrix[, 1])){
  
  #Create list with file names
  uniqueFiles <- unique(matrixwordsSorted[1])
  #select first file name in list of file names
  file <- uniqueFiles[i, 1]
  #Extract all entries corresponding to selected file name
  matrixwordsSortedFile <- matrixwordsSorted[matrixwordsSorted$File == file,]

  #Convert text into a corpus suitable for the word2vec function and save corpus text as text file
  corpus <- as_corpus_text(matrix[i, 2])
  #Save corpus file
  writeLines(as.character(corpus), con = "mycorpus.txt")
  
  #Create the model
  model <- word2vec('mycorpus.txt')

  #Select the two words with the highest tf-idf score for the upcoming similarity measures
  wordsimilarity_x = matrixwordsSortedFile[1, 2]
  wordsimilarity_y = matrixwordsSortedFile[2, 2]
  
  #Adjust the model for the selection
  selection <- model[[c(wordsimilarity_y, wordsimilarity_x), average = F]]
  len <- length(model[, 1])
  #Apply cosine similarity measure
  selectionModel <- model[1:len,] %>% cosineSimilarity(selection)
  
  #Prepare the data
  colnames(selectionModel)[1] <- "word_similarity_x"
  colnames(selectionModel)[2] <- "word_similarity_y"
  minimum = abs(min(selectionModel[, 1]))
  minimum2 = abs(min(selectionModel[, 2]))
  
  #Map the data to a positive range for better visualization
  for(j in 1:length(selectionModel[, 1])){
    
    selectionModel[j, 1] = selectionModel[j, 1] + minimum
    selectionModel[j, 2] = selectionModel[j, 2] + minimum2
  }
  
  #Remove the two reference words before plotting
  row.names.remove <- c(wordsimilarity_x, wordsimilarity_y)
  selectionModel = selectionModel[!(row.names(selectionModel) %in% row.names.remove),]
  
  #Prepare the plot of the last text analyzed for word similarities
  if(i == length(matrix[, 1])){
  
    selectionModelPlot <- as.data.frame(selectionModel)
    temp <- selectionModelPlot
    #Create a new window for upcoming plot
    dev.new(width = 15, height = 10, noRStudioGD = TRUE)
  }
  
  #Add a column for combined similarities for each word
  selectionModel <- cbind(selectionModel, combined_similarities = 0)
  
  #Compute the combined similarities for each word
  for(k in 1:length(selectionModel[, 1])){
    
    selectionModel[k, 3] <- sum(selectionModel[k, 1], selectionModel[k, 2])
  }
  
  #Order the combined similarities from highest to lowest
  selectionModel <- selectionModel[order(selectionModel[, 3], decreasing = TRUE),]
  
  
  ############################################################
  
  
  selectionModel <- as.data.frame(selectionModel)
  
  #Use kmeans clustering to cluster the words from previously built model
  set.seed(123)
  modelCluster <- kmeans(selectionModel, 4, nstart = 25)
  
  modelCluster$cluster <- as.factor(modelCluster$cluster)

  #Centroids of the clusters
  centroids <- modelCluster$center
  
  #Save the clusters for each word in the matrix containing the similarity parameters
  selectionModel$cluster = modelCluster$cluster
  
  selectedCluster = as.character(selectionModel[1, 4])
  
  #Preparethe plot of the last clustered text
  if(i == length(matrix[, 1])){
    
    temp2 <- selectionModel
  }
  
  selectionModel <- selectionModel[selectionModel$cluster == selectedCluster,]
  topicWords <- matrix(0, nrow = 8, ncol = 1)
  
  #Add reference words (words with two highest tf-idf scores) for each text, used for the 
  #similarity measures, to the list
  topicWords[1, 1] = wordsimilarity_x
  topicWords[2, 1] = wordsimilarity_y
  
  #Add remaining top n words with highest combined similarity to the list
  for(l in 3:8){
    
    topicWords[l, 1] = rownames(selectionModel)[l]
  }
  
  store[, i] = topicWords
  
  #Remove corpus from previously built model
  file.remove("mycorpus.bin")
}

#Save the list with top n words with highest combined similarity for each text as a *.csv file
write.csv(store, "store.csv")

#Plot the words for similarity parameters
ggplot(temp, aes(word_similarity_x, 
                               word_similarity_y)) + geom_text(aes(label = row.names(temp)))

#Create a new window for upcoming plot
dev.new(width = 15, height = 10, noRStudioGD = TRUE)

#Plot the clustered data
ggplot(temp2, aes(word_similarity_x, word_similarity_y, color = 
                             cluster)) + geom_text(aes(label = row.names(temp2)))

keeps <- c("word_similarity_x", "word_similarity_y", "cluster")
temp2 <- temp2[keeps]

#Use k-nearest-neighboring method to make the decision boundary
knn = knn3(cluster ~ ., data = temp2, k = 1)

#Create new window for upcoming plot
dev.new(width = 15, height = 10, noRStudioGD = TRUE)

#Plot the model with its decision boundaries
decisionplot(knn, temp2, class = "cluster", main = "Decision Boundary (on Pretrained Model)")



#https://towardsdatascience.com/a-friendly-introduction-to-text-clustering-fa996bcefd04
#https://code.google.com/archive/p/word2vec/
#https://gist.github.com/primaryobjects/8038d345aae48ae48988906b0525d175
#https://www.r-bloggers.com/2015/12/k-means-clustering-in-r/
#https://gis.stackexchange.com/questions/6025/finding-centroid-of-cluster-of-points-using-r/6026