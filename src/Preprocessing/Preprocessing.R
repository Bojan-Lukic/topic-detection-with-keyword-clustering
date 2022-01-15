rm(list=ls()) 

library("quanteda.textmodels")
library("quanteda")
library(quanteda)
library(janeaustenr)
library(stringr)
library(readtext)
library(sjmisc)
library(stopwords)
library(SemNetCleaner)
library(sjmisc)
library(SnowballC)
library(tidytext)
library(tidyverse)
library(zonator)
library(tm)
library(data.table)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

list = list.files("../Datasets/SentenceCorpus/SentenceCorpus/unlabeled_articles/arxiv_unlabeled/")
len = length(list)
fraction = len/20
matrix <- matrix(0, nrow = (len/fraction), ncol = 2)

#Prepare list of stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
#Prepare matrices containing other stopwords and written numbers
declaration <- matrix(c("citation", "symbol", "abstract", "</s>"))
numbers <- matrix(c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"))
#Prepare dictionary for stem completion with 3000 most common English words
words <- as.matrix(fread("english_words_short.txt"))


#Function that cleans a string of words with multiple techniques
cleanString <- function(string){
  
  #Lowercase
  temp <- tolower(string)
  #Remove everything that is not a number or letter 
  temp <- stringr::str_replace_all(temp, "[^a-zA-Z\\s]", "")
  #Remove stopwords
  temp <- stringr::str_replace_all(temp, stopwords_regex, "")
  #Minimize to just one white space
  temp <- stringr::str_replace_all(temp, "[\\s]+", " ")
  #Split the words
  temp <- stringr::str_split(temp, " ")[[1]]
  #Remove trailing spaces
  indexes <- which(temp == "")
  
  if(length(indexes) > 0){
    
    temp <- temp[-indexes]
  }
  
  #Singularize, stem, and stem complete each word
  for(i in 1:length(temp)){
    
    temp[i] = singularize(temp[i])
    temp[i] = wordStem(temp[i], language = "english")
    temp[i] = stemCompletion(temp[i], dictionary = words, type = "shortest")
  }
  
  #Prepare list of adverbs
  adverbs <- unnest_tokens(tibble(txt = string), word, txt) %>%
    left_join(parts_of_speech) %>%
    filter(pos %in% c("Adverb")) %>%
    pull(word) %>%
    unique
  
  #Remove adverbs, stopwords, and written numbers
  temp <- temp[!(temp %in% adverbs)]
  temp <- temp[!(temp %in% declaration)]
  temp <- temp[!(temp %in% numbers)]
  
  return(temp)
}

#Function that returns a String of words from a list of words
returnString <- function(matrix){
  
  temp = matrix
  String = ""
  
  for(i in 1:length(temp)){
    
    #If empty, or "NA", assign arbitrary short letter
    if(is.na(temp[i]) || temp[i] == "NA"){
      
      temp[i] = "a"
    }
    
    #If empty or number of characters in word less than 3, skip
    if(is_empty(temp[i]) || nchar(temp[i]) < 3){
      
    }
    
    #Else combine current word with string of words
    else{
      
      String = paste(String, " ", temp[i], sep = "")
    }
  }
  
  return(String)
}

#Function that returns a list of words from a string of words
returnWords <- function(string){
  
  temp = string
  #Split the words
  temp <- stringr::str_split(temp, " ")[[1]]
  #Remove trailing spaces
  indexes <- which(temp == "")
  
  if(length(indexes) > 0){
    
    temp <- temp[-indexes]
  } 
  
  return(as.matrix(temp))
}

#Function that computes the Jaccard measure for two sets of text
jaccard <- function(vector1, vector2){
  
  #vector1 <- unique(vector1)
  #vector2 <- unique(vector2)
  
  jaccard = (length(intersect(vector1, vector2)))/(length(union(vector1, vector2)))
  return(jaccard)
}


#For 20 texts in the directory, clean the text, return as one string, and save in a matrix
for(i in 1:(len/fraction)){
  
  dir = paste("../Datasets/SentenceCorpus/SentenceCorpus/unlabeled_articles/arxiv_unlabeled/", list[i], sep = "")
  matrix[i, 1] = list[i]
  matrix[i, 2] = returnString(cleanString(readtext(dir)))
}

#Prepare first text before preprocessing
rawWords = returnWords(readtext(paste("../Datasets/SentenceCorpus/SentenceCorpus/unlabeled_articles/arxiv_unlabeled/", 
                                      list[1], sep = "")))
#Prepare second text before preprocessing
rawWords2 = returnWords(readtext(paste("../Datasets/SentenceCorpus/SentenceCorpus/unlabeled_articles/arxiv_unlabeled/", 
                                       list[2], sep = "")))
#Prepare first text after preprocessing
cleanWords = returnWords(matrix[1, 2])
#Prepare second text after preprocessing
cleanWords2 = returnWords(matrix[2, 2])

#Compute Jaccard measured for texts before and after preprocessing
jaccard(rawWords, rawWords2)
jaccard(cleanWords, cleanWords2)

#head(matrix)
matrix <- as.data.frame(matrix)

#Change column names of matrix containing the processed strings of all 20 texts
colnames(matrix)[1] <- "File"
colnames(matrix)[2] <- "Text"

save(matrix, file = "../Clustering/TidyText.RData")


matrixwords <- matrix(0, nrow = 0, ncol = 2)

#Split the texts from the matrix file into single words (will be needed for the next task)
for(i in 1:(len/fraction)){
  
  words <- returnWords(matrix[i, 2])
  tempmatrix <- matrix(0, nrow = length(words), ncol = 2)
  
  for(j in 1:length(words)){
    
    tempmatrix[j, 1] = list[i]
    tempmatrix[j, 2] = words[j]
  }
  
  matrixwords <- rbind(matrixwords, tempmatrix)
}

#head(matrixwords)
matrixwords <- as.data.frame(matrixwords)

#Change column names of matrix containing single words of all 20 texts
colnames(matrixwords)[1] <- "File"
colnames(matrixwords)[2] <- "Word"

#Count the words in the matrix to make sure that there are no high frequency stopwords remaining in the data
matrixwords %>% count(Word, sort = TRUE)

save(matrixwords, file = "../Categorization/TidyWords.RData")



#https://www.mjdenny.com/Text_Processing_In_R.html
#https://programminghistorian.org/en/lessons/basic-text-processing-in-r